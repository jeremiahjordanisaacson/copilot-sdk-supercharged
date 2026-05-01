// E2E test harness for the V Copilot SDK.
//
// Spawns the shared replay proxy (npx tsx test/harness/server.ts),
// parses the `Listening: http://...` URL from stdout, and provides
// helpers for E2E tests.

module e2e

import os
import net.http
import json
import time

pub struct ProxyHandle {
pub:
	url string
pub mut:
	process os.Process
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
			'Content-Type':  'application/json'
			'Authorization': 'Bearer fake-token-for-e2e-tests'
		})
		data: body
	}
	resp := http.fetch(config) or { return error('POST failed: ${err}') }
	return resp.body
}
