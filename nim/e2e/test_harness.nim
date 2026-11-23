## Test harness for E2E tests - manages the replaying CAPI proxy

import std/[osproc, strutils, os, httpclient, json, re, streams]

type
  CapiProxy* = object
    process: Process
    proxyUrl: string
    running: bool

proc repoRoot(): string =
  ## Repository root (this file lives in nim/e2e).
  normalizedPath(currentSourcePath().parentDir() / ".." / "..")

proc findCopilotCli*(): string =
  ## Locate the Copilot CLI for E2E runs, in order:
  ## 1. `COPILOT_CLI_PATH` when it points to an existing file
  ## 2. nodejs/node_modules/@github/copilot/npm-loader.js (run with node)
  ## 3. legacy nodejs/node_modules/@github/copilot-*/index.js, then
  ##    nodejs/node_modules/@github/copilot/index.js
  ## Returns "" when nothing is found.
  let envPath = getEnv("COPILOT_CLI_PATH")
  if envPath.len > 0 and fileExists(envPath):
    return envPath
  let githubDir = repoRoot() / "nodejs" / "node_modules" / "@github"
  let loader = githubDir / "copilot" / "npm-loader.js"
  if fileExists(loader):
    return loader
  if dirExists(githubDir):
    for kind, dir in walkDir(githubDir):
      if kind in {pcDir, pcLinkToDir} and extractFilename(dir).startsWith("copilot-"):
        let candidate = dir / "index.js"
        if fileExists(candidate):
          return candidate
  let legacy = githubDir / "copilot" / "index.js"
  if fileExists(legacy):
    return legacy
  ""

proc newCapiProxy*(): CapiProxy =
  result = CapiProxy(process: nil, proxyUrl: "", running: false)

proc start*(proxy: var CapiProxy): string =
  ## Spawn the replay proxy and return its URL
  if proxy.proxyUrl.len > 0:
    return proxy.proxyUrl

  # Path to harness server (from nim/e2e, up 2 to repo root)
  let harnessDir = repoRoot() / "test" / "harness"
  let serverPath = harnessDir / "server.ts"

  # Spawn proxy process
  proxy.process = startProcess(
    "npx", args = ["tsx", serverPath],
    workingDir = harnessDir,
    options = {poUsePath, poStdErrToStdOut}
  )

  # Read first line from stdout
  let line = proxy.process.outputStream.readLine()

  # Parse "Listening: http://..."
  var matches: array[1, string]
  if line.find(re"Listening: (http://\S+)", matches) >= 0:
    proxy.proxyUrl = matches[0]
  else:
    proxy.process.kill()
    raise newException(IOError, "Failed to parse proxy URL from: " & line)

  proxy.running = true
  putEnv("COPILOT_API_URL", proxy.proxyUrl)
  # The client resolves the CLI from COPILOT_CLI_PATH and runs `.js` entry points with node.
  let cliPath = findCopilotCli()
  if cliPath.len > 0:
    putEnv("COPILOT_CLI_PATH", cliPath)
  return proxy.proxyUrl

proc stop*(proxy: var CapiProxy) =
  ## Stop the proxy server
  if proxy.proxyUrl.len > 0:
    try:
      let client = newHttpClient()
      discard client.post(proxy.proxyUrl & "/stop")
      client.close()
    except:
      discard

  if proxy.process != nil:
    proxy.process.close()
    proxy.process = nil

  proxy.proxyUrl = ""
  proxy.running = false

proc configure*(proxy: var CapiProxy, filePath: string, workDir: string) =
  ## Configure the proxy with snapshot file and working directory
  if proxy.proxyUrl.len == 0:
    raise newException(IOError, "Proxy not started")

  let client = newHttpClient()
  client.headers = newHttpHeaders({"Content-Type": "application/json"})
  let body = %*{"filePath": filePath, "workDir": workDir}
  let resp = client.post(proxy.proxyUrl & "/config", $body)
  if resp.code != Http200:
    raise newException(IOError, "Config failed with status: " & $resp.code)
  client.close()

proc url*(proxy: CapiProxy): string =
  proxy.proxyUrl
