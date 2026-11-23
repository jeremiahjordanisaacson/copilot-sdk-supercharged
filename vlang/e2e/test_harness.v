// E2E test harness for the V Copilot SDK.
//
// Spawns the shared replay proxy (npx tsx test/harness/server.ts),
// parses the `Listening: http://...` URL from stdout, and provides
// helpers for E2E tests.

module e2e

import os
import net.http
import time

pub struct ProxyHandle {
pub:
	url string
pub mut:
	process os.Process
}

// find_copilot_cli locates the Copilot CLI for E2E runs, in order:
// 1. COPILOT_CLI_PATH when it points to an existing file
// 2. nodejs/node_modules/@github/copilot/npm-loader.js (run with node)
// 3. legacy nodejs/node_modules/@github/copilot-*/index.js, then
//    nodejs/node_modules/@github/copilot/index.js
// Returns '' when nothing is found. The SDK runs `.js` paths with node and
// executes any other path (for example the `.bin/copilot` shim) directly.
pub fn find_copilot_cli() string {
	env_path := os.getenv('COPILOT_CLI_PATH')
	if env_path.len > 0 && os.is_file(env_path) {
		return env_path
	}
	github_dir := os.join_path(os.dir(@FILE), '..', '..', 'nodejs', 'node_modules', '@github')
	loader := os.join_path(github_dir, 'copilot', 'npm-loader.js')
	if os.is_file(loader) {
		return loader
	}
	entries := os.ls(github_dir) or { []string{} }
	for entry in entries {
		if entry.starts_with('copilot-') {
			candidate := os.join_path(github_dir, entry, 'index.js')
			if os.is_file(candidate) {
				return candidate
			}
		}
	}
	legacy := os.join_path(github_dir, 'copilot', 'index.js')
	if os.is_file(legacy) {
		return legacy
	}
	return ''
}

// start_proxy starts the replay proxy and returns a handle with the URL.
pub fn start_proxy() !ProxyHandle {
	// Path from vlang/e2e to repo root test/harness/server.ts
	script_dir := os.dir(@FILE)
	server_path := os.real_path(os.join_path(script_dir, '..', '..', 'test', 'harness',
		'server.ts'))
	cwd := os.real_path(os.join_path(script_dir, '..', '..', 'test', 'harness'))

	mut p := os.new_process('npx')
	p.set_args(['tsx', server_path])
	p.set_work_folder(cwd)
	p.set_redirect_stdio()
	p.run()

	// Read stdout until we get the Listening line (up to 30s)
	mut url := ''
	for _ in 0 .. 60 {
		line := p.stdout_read()
		if line.contains('Listening: ') {
			start := line.index('http://') or { continue }
			mut end := start
			for end < line.len && line[end] != ` ` && line[end] != `\n` && line[end] != `\r` {
				end++
			}
			url = line[start..end]
			break
		}
		time.sleep(500 * time.millisecond)
	}

	if url.len == 0 {
		p.signal_kill()
		return error('Could not parse proxy URL')
	}

	os.setenv('COPILOT_API_URL', url, true)
	cli_path := find_copilot_cli()
	if cli_path.len > 0 {
		os.setenv('COPILOT_CLI_PATH', cli_path, true)
	}
	println('[vlang-e2e] Proxy listening at ${url}')

	return ProxyHandle{
		url: url
		process: p
	}
}

// stop_proxy terminates the replay proxy process.
pub fn stop_proxy(mut handle ProxyHandle) {
	handle.process.signal_kill()
	handle.process.wait()
}

// configure sets up the proxy for a specific test snapshot.
pub fn configure(handle ProxyHandle, file_path string, work_dir string) ! {
	body := '{"filePath":"${file_path}","workDir":"${work_dir}"}'
	http.post('${handle.url}/config', body) or { return error('configure failed: ${err}') }
}

// http_get performs a GET request against the proxy.
pub fn http_get(handle ProxyHandle, path string) !string {
	resp := http.get('${handle.url}${path}') or { return error('GET failed: ${err}') }
	return resp.body
}

// http_post performs a POST request against the proxy.
pub fn http_post(handle ProxyHandle, path string, body string) !string {
	mut config := http.FetchConfig{
		url: '${handle.url}${path}'
		method: .post
		header: http.new_header_from_map({
			.content_type:  'application/json'
			.authorization: 'Bearer fake-token-for-e2e-tests'
		})
		data: body
	}
	resp := http.fetch(config) or { return error('POST failed: ${err}') }
	return resp.body
}
