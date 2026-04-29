## JSON-RPC 2.0 transport layer for the GitHub Copilot SDK.
##
## Handles Content-Length framed message encoding/decoding over
## stdin/stdout streams following the LSP base protocol.

import std/[json, options, strutils, streams, tables, asyncdispatch, locks]
import ./types

const
  JsonRpcVersion* = "2.0"
  ContentLengthHeader = "Content-Length: "

# ---------------------------------------------------------------------------
# Framing: encode / decode Content-Length delimited messages
# ---------------------------------------------------------------------------

proc encodeMessage*(payload: JsonNode): string =
  ## Encode a JSON payload with Content-Length header framing.
  let body = $payload
  result = ContentLengthHeader & $body.len & "\r\n\r\n" & body

proc parseContentLength*(header: string): int =
  ## Extract the Content-Length value from a header line.
  if header.startsWith(ContentLengthHeader):
    return parseInt(header[ContentLengthHeader.len .. ^1].strip())
  raise newException(ValueError, "Invalid Content-Length header: " & header)

# ---------------------------------------------------------------------------
# JSON-RPC message builders
# ---------------------------------------------------------------------------

var globalRequestId {.threadvar.}: int

proc nextRequestId*(): int =
  inc globalRequestId
  result = globalRequestId

proc buildRequest*(`method`: string; params: JsonNode = newJObject();
                   id: int = nextRequestId()): JsonNode =
  ## Build a JSON-RPC 2.0 request object.
  result = %*{
    "jsonrpc": JsonRpcVersion,
    "id": id,
    "method": `method`,
    "params": params,
  }

proc buildNotification*(`method`: string;
                        params: JsonNode = newJObject()): JsonNode =
  ## Build a JSON-RPC 2.0 notification (no id).
  result = %*{
    "jsonrpc": JsonRpcVersion,
    "method": `method`,
    "params": params,
  }

proc buildResponse*(id: int; resultNode: JsonNode): JsonNode =
  ## Build a JSON-RPC 2.0 success response.
  result = %*{
    "jsonrpc": JsonRpcVersion,
    "id": id,
    "result": resultNode,
  }

proc buildErrorResponse*(id: int; code: int; message: string;
                         data: JsonNode = newJNull()): JsonNode =
  ## Build a JSON-RPC 2.0 error response.
  result = %*{
    "jsonrpc": JsonRpcVersion,
    "id": id,
    "error": {
      "code": code,
      "message": message,
      "data": data,
    },
  }

# ---------------------------------------------------------------------------
# Message parsing helpers
# ---------------------------------------------------------------------------

proc isRequest*(msg: JsonNode): bool =
  msg.hasKey("id") and msg.hasKey("method")

proc isResponse*(msg: JsonNode): bool =
  msg.hasKey("id") and (msg.hasKey("result") or msg.hasKey("error"))

proc isNotification*(msg: JsonNode): bool =
  msg.hasKey("method") and not msg.hasKey("id")

proc getMethod*(msg: JsonNode): string =
  msg["method"].getStr()

proc getId*(msg: JsonNode): int =
  msg["id"].getInt()

proc getParams*(msg: JsonNode): JsonNode =
  if msg.hasKey("params"): msg["params"]
  else: newJObject()

proc getResult*(msg: JsonNode): JsonNode =
  if msg.hasKey("result"): msg["result"]
  else: newJNull()

proc getError*(msg: JsonNode): Option[JsonRpcError] =
  if msg.hasKey("error") and msg["error"].kind != JNull:
    let errNode = msg["error"]
    some(JsonRpcError(
      code: errNode["code"].getInt(),
      message: errNode["message"].getStr(),
      data: if errNode.hasKey("data"): errNode["data"] else: newJNull(),
    ))
  else:
    none(JsonRpcError)

# ---------------------------------------------------------------------------
# Stream-based reader (synchronous, for use in reader threads)
# ---------------------------------------------------------------------------

proc readRawMessage*(input: Stream): string =
  ## Read one Content-Length framed message from a stream.
  ## Returns the raw JSON body string. Blocks until a full message arrives.
  var contentLength = -1

  # Read headers until blank line
  while true:
    let line = input.readLine()
    if line.len == 0:
      break
    if line.startsWith(ContentLengthHeader):
      contentLength = parseContentLength(line)

  if contentLength < 0:
    raise newException(IOError, "Missing Content-Length header")

  var buf = newString(contentLength)
  let bytesRead = input.readData(addr buf[0], contentLength)
  if bytesRead != contentLength:
    raise newException(IOError,
      "Expected " & $contentLength & " bytes, got " & $bytesRead)
  result = buf

proc readMessage*(input: Stream): JsonNode =
  ## Read and parse one JSON-RPC message from a stream.
  let raw = readRawMessage(input)
  result = parseJson(raw)

# ---------------------------------------------------------------------------
# Pending request tracker
# ---------------------------------------------------------------------------

type
  PendingRequests* = ref object
    lock: Lock
    table: Table[int, Future[JsonNode]]

proc newPendingRequests*(): PendingRequests =
  result = PendingRequests(table: initTable[int, Future[JsonNode]]())
  initLock(result.lock)

proc register*(pr: PendingRequests; id: int): Future[JsonNode] =
  let fut = newFuture[JsonNode]("pending-rpc-" & $id)
  withLock pr.lock:
    pr.table[id] = fut
  result = fut

proc resolve*(pr: PendingRequests; id: int; payload: JsonNode) =
  var fut: Future[JsonNode]
  withLock pr.lock:
    if pr.table.hasKey(id):
      fut = pr.table[id]
      pr.table.del(id)
  if not fut.isNil:
    fut.complete(payload)

proc reject*(pr: PendingRequests; id: int; msg: string) =
  var fut: Future[JsonNode]
  withLock pr.lock:
    if pr.table.hasKey(id):
      fut = pr.table[id]
      pr.table.del(id)
  if not fut.isNil:
    fut.fail(newException(IOError, msg))

proc rejectAll*(pr: PendingRequests; msg: string) =
  withLock pr.lock:
    for id, fut in pr.table:
      fut.fail(newException(IOError, msg))
    pr.table.clear()
