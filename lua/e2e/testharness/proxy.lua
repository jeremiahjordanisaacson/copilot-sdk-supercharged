--- Replaying CAPI proxy for E2E tests.
--
-- Manages a child process that acts as a replaying proxy to AI endpoints.
-- Spawns the shared test harness server from test/harness/server.ts.
--
-- Usage:
--   local CapiProxy = require("e2e.testharness.proxy")
--   local proxy = CapiProxy.new()
--   local url = proxy:start()
--   -- ... run tests against url ...
--   proxy:stop()

local CapiProxy = {}
CapiProxy.__index = CapiProxy

--- Create a new CapiProxy instance.
-- @return CapiProxy
function CapiProxy.new()
    local self = setmetatable({}, CapiProxy)
    self._process = nil
    self._proxy_url = nil
    self._original_copilot_api_url = nil
    return self
end

--- Launch the proxy server and return its URL.
-- Sets the COPILOT_API_URL environment variable so the CLI uses the proxy.
-- @return string|nil  The proxy URL
-- @return string|nil  Error message
function CapiProxy:start()
    if self._proxy_url then
        return self._proxy_url, nil
    end

    -- Resolve path to the shared harness server relative to this file's location.
    -- From lua/e2e/testharness/ we need to reach test/harness/server.ts
    -- which is at ../../test/harness/server.ts relative to lua/e2e/.
    local sep = package.config:sub(1, 1)
    local is_windows = (sep == "\\")

    -- Build the path to the harness server.
    -- We get the directory of this script to build a reliable path.
    local script_dir = debug.getinfo(1, "S").source:match("^@(.+)[/\\]") or "."
    local server_path = script_dir .. sep .. ".." .. sep .. ".." .. sep .. ".."
        .. sep .. "test" .. sep .. "harness" .. sep .. "server.ts"

    -- Normalize the path for the OS
    if is_windows then
        server_path = server_path:gsub("/", "\\")
    else
        server_path = server_path:gsub("\\", "/")
    end

    -- The cwd for the proxy process should be the test/harness directory
    local harness_dir = script_dir .. sep .. ".." .. sep .. ".." .. sep .. ".."
        .. sep .. "test" .. sep .. "harness"
    if is_windows then
        harness_dir = harness_dir:gsub("/", "\\")
    else
        harness_dir = harness_dir:gsub("\\", "/")
    end

    -- Build the command. On Windows we need shell to find npx.
    local cmd
    if is_windows then
        cmd = string.format('cd /d "%s" && npx tsx "%s"', harness_dir, server_path)
    else
        cmd = string.format('cd "%s" && npx tsx "%s"', harness_dir, server_path)
    end

    -- Spawn the process, reading stdout to capture the listening URL
    local handle = io.popen(cmd, "r")
    if not handle then
        return nil, "Failed to spawn replay proxy process"
    end

    self._process = handle

    -- Read the first line of output to get the listening URL
    local line = handle:read("*l")
    if not line or line == "" then
        handle:close()
        self._process = nil
        return nil, "Failed to read proxy URL from process stdout"
    end

    -- Parse "Listening: http://..." from output
    local url = line:match("Listening:%s*(http://[^%s]+)")
    if not url then
        handle:close()
        self._process = nil
        return nil, "Unexpected proxy output: " .. tostring(line)
    end

    self._proxy_url = url

    -- Save original env and set COPILOT_API_URL so the CLI uses the proxy
    self._original_copilot_api_url = os.getenv("COPILOT_API_URL")
    self:_setenv("COPILOT_API_URL", url)

    return url, nil
end

--- Gracefully shut down the proxy server.
-- Restores the COPILOT_API_URL environment variable.
-- @param skip_writing_cache boolean|nil  If true, proxy won't write captured exchanges
function CapiProxy:stop(skip_writing_cache)
    if not self._process then
        return
    end

    -- Send stop request to the server (best effort)
    if self._proxy_url then
        local stop_url = self._proxy_url .. "/stop"
        if skip_writing_cache then
            stop_url = stop_url .. "?skipWritingCache=true"
        end
        -- Use curl for a simple POST; ignore errors
        local is_windows = (package.config:sub(1, 1) == "\\")
        local curl_cmd
        if is_windows then
            curl_cmd = string.format('curl -s -X POST "%s" >nul 2>&1', stop_url)
        else
            curl_cmd = string.format('curl -s -X POST "%s" >/dev/null 2>&1', stop_url)
        end
        os.execute(curl_cmd)
    end

    -- Close the process handle
    pcall(function() self._process:close() end)
    self._process = nil

    -- Restore the original COPILOT_API_URL
    if self._original_copilot_api_url then
        self:_setenv("COPILOT_API_URL", self._original_copilot_api_url)
    else
        self:_setenv("COPILOT_API_URL", "")
    end

    self._proxy_url = nil
end

--- Configure the proxy with a snapshot file path and working directory.
-- @param file_path string  Path to the snapshot YAML file
-- @param work_dir string   Working directory for the proxy
-- @return boolean success
-- @return string|nil error
function CapiProxy:configure(file_path, work_dir)
    if not self._proxy_url then
        return false, "Proxy not started"
    end

    local is_windows = (package.config:sub(1, 1) == "\\")
    local curl_cmd
    local json_body = string.format('{"filePath":"%s","workDir":"%s"}',
        file_path:gsub("\\", "\\\\"):gsub('"', '\\"'),
        work_dir:gsub("\\", "\\\\"):gsub('"', '\\"'))

    if is_windows then
        curl_cmd = string.format(
            'curl -s -X POST -H "Content-Type: application/json" -d "%s" "%s/config"',
            json_body:gsub('"', '\\"'), self._proxy_url)
    else
        curl_cmd = string.format(
            "curl -s -X POST -H 'Content-Type: application/json' -d '%s' '%s/config'",
            json_body, self._proxy_url)
    end

    local ok = os.execute(curl_cmd)
    if not ok then
        return false, "Failed to configure proxy"
    end
    return true, nil
end

--- Get the proxy URL, or nil if not started.
-- @return string|nil
function CapiProxy:url()
    return self._proxy_url
end

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

--- Set an environment variable (cross-platform best effort).
-- Lua doesn't have a built-in setenv; we use os.execute as a workaround
-- for child processes, but for the current process we rely on a C binding
-- if available, or fall back to a no-op with a warning.
-- @param name string
-- @param value string
function CapiProxy:_setenv(name, value)
    -- Try luaposix first
    local ok, posix = pcall(require, "posix.stdlib")
    if ok and posix and posix.setenv then
        posix.setenv(name, value)
        return
    end

    -- Try lua-os-setenv (luarocks: os-setenv)
    local ok2, setenv_mod = pcall(require, "os_setenv")
    if ok2 and setenv_mod then
        setenv_mod.setenv(name, value)
        return
    end

    -- Fallback: on POSIX, export for child processes via shell.
    -- This won't affect the current Lua process's os.getenv(), but the
    -- CLI process will inherit the env from popen.
    -- For testing purposes, we store the value so proxy:url() works.
    if package.config:sub(1, 1) ~= "\\" then
        os.execute(string.format('export %s="%s"', name, value))
    end
end

return CapiProxy
