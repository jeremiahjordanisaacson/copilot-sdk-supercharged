/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.github.copilot

import kotlinx.coroutines.*
import kotlinx.serialization.json.*
import java.io.*
import java.nio.charset.StandardCharsets
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

/**
 * JSON-RPC error response.
 */
class JsonRpcException(
    val code: Int,
    override val message: String,
    val data: JsonElement? = null
) : Exception("JSON-RPC Error $code: $message")

/**
 * Handler for incoming JSON-RPC requests from the server.
 */
typealias RequestHandler = suspend (params: JsonObject) -> JsonObject

/**
 * Minimal JSON-RPC 2.0 client for stdio/TCP transport.
 *
 * Uses a background coroutine for reading and provides a suspend-based interface.
 * Messages are framed with Content-Length headers per the LSP base protocol.
 */
class JsonRpcClient(
    private val inputStream: InputStream,
    private val outputStream: OutputStream,
    private val scope: CoroutineScope
) {
    private val json = Json {
        ignoreUnknownKeys = true
        encodeDefaults = false
    }

    private val pendingRequests = ConcurrentHashMap<String, CompletableDeferred<JsonElement>>()
    private val requestHandlers = ConcurrentHashMap<String, RequestHandler>()

    @Volatile
    private var notificationHandler: ((method: String, params: JsonObject) -> Unit)? = null

    @Volatile
    private var running = false

    private var readJob: Job? = null
    private val writeLock = Any()

    /**
     * Start listening for messages in a background coroutine.
     */
    fun start() {
        if (running) return
        running = true
        readJob = scope.launch(Dispatchers.IO) {
            readLoop()
        }
    }

    /**
     * Stop listening and complete all pending requests with errors.
     */
    fun stop() {
        running = false
        readJob?.cancel()
        readJob = null

        // Complete all pending requests with error
        for ((_, deferred) in pendingRequests) {
            deferred.completeExceptionally(IOException("Client stopped"))
        }
        pendingRequests.clear()
    }

    /**
     * Set handler for incoming notifications from the server.
     */
    fun setNotificationHandler(handler: (method: String, params: JsonObject) -> Unit) {
        this.notificationHandler = handler
    }

    /**
     * Set handler for incoming requests from the server.
     */
    fun setRequestHandler(method: String, handler: RequestHandler?) {
        if (handler == null) {
            requestHandlers.remove(method)
        } else {
            requestHandlers[method] = handler
        }
    }

    /**
     * Send a JSON-RPC request and wait for the response.
     *
     * @param method The RPC method name
     * @param params Optional parameters
     * @param timeoutMs Request timeout in milliseconds (default 30s)
     * @return The result from the response as a [JsonElement]
     * @throws JsonRpcException If the server returns an error
     * @throws kotlinx.coroutines.TimeoutCancellationException If the request times out
     */
    suspend fun request(
        method: String,
        params: JsonElement = JsonObject(emptyMap()),
        timeoutMs: Long = 30_000L
    ): JsonElement {
        val requestId = UUID.randomUUID().toString()
        val deferred = CompletableDeferred<JsonElement>()
        pendingRequests[requestId] = deferred

        val message = buildJsonObject {
            put("jsonrpc", "2.0")
            put("id", requestId)
            put("method", method)
            put("params", params)
        }

        sendMessage(message)

        try {
            return withTimeout(timeoutMs) {
                deferred.await()
            }
        } finally {
            pendingRequests.remove(requestId)
        }
    }

    /**
     * Send a JSON-RPC notification (no response expected).
     */
    suspend fun notify(method: String, params: JsonElement = JsonObject(emptyMap())) {
        val message = buildJsonObject {
            put("jsonrpc", "2.0")
            put("method", method)
            put("params", params)
        }
        sendMessage(message)
    }

    // ========================================================================
    // Private Implementation
    // ========================================================================

    private suspend fun sendMessage(message: JsonObject) {
        withContext(Dispatchers.IO) {
            val content = json.encodeToString(JsonObject.serializer(), message)
            val contentBytes = content.toByteArray(StandardCharsets.UTF_8)
            val header = "Content-Length: ${contentBytes.size}\r\n\r\n"

            synchronized(writeLock) {
                outputStream.write(header.toByteArray(StandardCharsets.UTF_8))
                outputStream.write(contentBytes)
                outputStream.flush()
            }
        }
    }

    private fun readLoop() {
        val bis = BufferedInputStream(inputStream)
        try {
            while (running) {
                val message = readMessage(bis) ?: break
                handleMessage(message)
            }
        } catch (e: Exception) {
            if (running) {
                // Connection error - complete all pending with exception
                for ((_, deferred) in pendingRequests) {
                    deferred.completeExceptionally(IOException("Connection lost: ${e.message}"))
                }
            }
        }
    }

    private fun readMessage(input: InputStream): JsonObject? {
        // Read header line character by character
        val headerLine = StringBuilder()
        var c: Int
        while (true) {
            c = input.read()
            if (c == -1) return null
            if (c == '\n'.code) break
            if (c != '\r'.code) headerLine.append(c.toChar())
        }

        val header = headerLine.toString().trim()
        if (!header.startsWith("Content-Length:")) return null

        val contentLength = header.substringAfter("Content-Length:").trim().toInt()

        // Read the empty line separator
        while (true) {
            c = input.read()
            if (c == -1) return null
            if (c == '\n'.code) break
        }

        // Read exact content bytes, handling short reads
        val contentBytes = ByteArray(contentLength)
        var bytesRead = 0
        while (bytesRead < contentLength) {
            val read = input.read(contentBytes, bytesRead, contentLength - bytesRead)
            if (read == -1) throw EOFException("Unexpected end of stream while reading JSON-RPC message")
            bytesRead += read
        }

        val content = String(contentBytes, StandardCharsets.UTF_8)
        return json.parseToJsonElement(content).jsonObject
    }

    private fun handleMessage(message: JsonObject) {
        val id = message["id"]?.let {
            when (it) {
                is JsonPrimitive -> it.content
                else -> it.toString()
            }
        }
        val method = message["method"]?.let {
            (it as? JsonPrimitive)?.content
        }

        // Response to our request (has id + result or error, no method)
        if (id != null && (message.containsKey("result") || message.containsKey("error"))) {
            val deferred = pendingRequests[id]
            if (deferred != null) {
                if (message.containsKey("error")) {
                    val error = message["error"]!!.jsonObject
                    val code = error["code"]?.let { (it as? JsonPrimitive)?.intOrNull } ?: -1
                    val errorMsg = error["message"]?.let { (it as? JsonPrimitive)?.content } ?: "Unknown error"
                    val data = error["data"]
                    deferred.completeExceptionally(JsonRpcException(code, errorMsg, data))
                } else {
                    deferred.complete(message["result"] ?: JsonObject(emptyMap()))
                }
            }
            return
        }

        // Notification from server (has method, no id)
        if (method != null && id == null) {
            val params = message["params"]?.jsonObject ?: JsonObject(emptyMap())
            notificationHandler?.invoke(method, params)
            return
        }

        // Incoming request from server (has both method and id)
        if (method != null && id != null) {
            val params = message["params"]?.jsonObject ?: JsonObject(emptyMap())
            val handler = requestHandlers[method]

            scope.launch {
                if (handler == null) {
                    sendErrorResponse(id, -32601, "Method not found: $method")
                    return@launch
                }
                try {
                    val result = handler(params)
                    sendResponse(id, result)
                } catch (e: JsonRpcException) {
                    sendErrorResponse(id, e.code, e.message ?: "Unknown error")
                } catch (e: Exception) {
                    sendErrorResponse(id, -32603, e.message ?: "Internal error")
                }
            }
        }
    }

    private suspend fun sendResponse(requestId: String, result: JsonObject) {
        val response = buildJsonObject {
            put("jsonrpc", "2.0")
            put("id", requestId)
            put("result", result)
        }
        sendMessage(response)
    }

    private suspend fun sendErrorResponse(requestId: String, code: Int, message: String) {
        val response = buildJsonObject {
            put("jsonrpc", "2.0")
            put("id", requestId)
            put("error", buildJsonObject {
                put("code", code)
                put("message", message)
            })
        }
        sendMessage(response)
    }
}
