## Test harness for E2E tests - manages the replaying CAPI proxy

import std/[osproc, strutils, os, httpclient, json, re]

type
  CapiProxy* = object
    process: Process
    proxyUrl: string
    running: bool

proc newCapiProxy*(): CapiProxy =
  result = CapiProxy(process: nil, proxyUrl: "", running: false)

proc start*(proxy: var CapiProxy): string =
  ## Spawn the replay proxy and return its URL
  if proxy.proxyUrl.len > 0:
    return proxy.proxyUrl

  # Path to harness server (from e2e dir, up 3 to repo root)
  let e2eDir = currentSourcePath().parentDir()
  let harnessDir = normalizedPath(e2eDir / ".." / ".." / ".." / "test" / "harness")
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
