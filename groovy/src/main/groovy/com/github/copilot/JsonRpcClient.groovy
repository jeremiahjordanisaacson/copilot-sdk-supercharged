/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot

import groovy.json.JsonOutput
import groovy.json.JsonSlurper
import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory
import java.util.concurrent.TimeUnit

/**
 * Minimal JSON-RPC 2.0 client for stdio/TCP transport.
 *
 * Uses Groovy's {@link JsonSlurper} and {@link JsonOutput} instead of external
 * libraries. Thread-safe write operations are guarded by a synchronization lock.
 */
@CompileStatic
class JsonRpcClient {
    /**
     * Sentinel a request handler returns to answer with a JSON {@code null} result
     * (for example {@code memoryStore.store}). Any other null or empty result is sent as {@code {}}.
     */
    static final Map<String, Object> NULL_RESULT = Collections.unmodifiableMap(new LinkedHashMap<String, Object>())

    private final InputStream inputStream
    private final OutputStream outputStream
    private final JsonSlurper slurper = new JsonSlurper()
    private final ConcurrentHashMap<String, CompletableFuture<Object>> pendingRequests = new ConcurrentHashMap<>()
    private final ConcurrentHashMap<String, Closure> requestHandlers = new ConcurrentHashMap<>()
    private volatile Closure notificationHandler
    private volatile boolean running
    private Thread readThread
    private final Object writeLock = new Object()
    private final ExecutorService handlerExecutor = Executors.newCachedThreadPool({ Runnable r ->
        Thread t = new Thread(r, 'jsonrpc-handler')
        t.daemon = true
        t
    } as ThreadFactory)

    JsonRpcClient(InputStream inputStream, OutputStream outputStream) {
        this.inputStream = inputStream
        this.outputStream = outputStream
    }

    void start() {
        if (running) return
        running = true
        readThread = new Thread(this.&readLoop as Runnable, 'jsonrpc-reader')
        readThread.daemon = true
        readThread.start()
    }

    void stop() {
        running = false
        handlerExecutor.shutdownNow()
        readThread?.interrupt()
        pendingRequests.each { String id, CompletableFuture<Object> f ->
            f.completeExceptionally(new IOException('Client stopped'))
        }
        pendingRequests.clear()
    }

    /**
     * Sets the notification handler closure.
     * Receives (String method, Map params).
     */
    void setNotificationHandler(Closure handler) {
        this.notificationHandler = handler
    }

    /** Sets a request handler for a specific method. */
    void setRequestHandler(String method, Closure handler) {
        if (handler == null) {
            requestHandlers.remove(method)
        } else {
            requestHandlers.put(method, handler)
        }
    }

    /** Sends a JSON-RPC request and waits for the response (30s default timeout). */
    Map<String, Object> request(String method, Object params) throws Exception {
        request(method, params, 30, TimeUnit.SECONDS)
    }

    /** Sends a JSON-RPC request and waits for the response with a custom timeout. */
    @SuppressWarnings('unchecked')
    Map<String, Object> request(String method, Object params, long timeout, TimeUnit unit) throws Exception {
        String requestId = UUID.randomUUID().toString()
        CompletableFuture<Object> future = new CompletableFuture<>()
        pendingRequests.put(requestId, future)

        Map<String, Object> message = [
            jsonrpc: '2.0',
            id     : requestId,
            method : method,
            params : params ?: [:]
        ] as Map<String, Object>

        sendMessage(message)

        try {
            Object result = future.get(timeout, unit)
            if (result instanceof Map) {
                return (Map<String, Object>) result
            }
            return [result: result] as Map<String, Object>
        } finally {
            pendingRequests.remove(requestId)
        }
    }

    // --- Private ---

    private void sendMessage(Map<String, Object> message) {
        String content = JsonOutput.toJson(message)
        byte[] contentBytes = content.getBytes(StandardCharsets.UTF_8)
        String header = "Content-Length: ${contentBytes.length}\r\n\r\n"

        synchronized (writeLock) {
            outputStream.write(header.getBytes(StandardCharsets.UTF_8))
            outputStream.write(contentBytes)
            outputStream.flush()
        }
    }

    private void readLoop() {
        try {
            BufferedInputStream bis = new BufferedInputStream(inputStream)
            while (running) {
                Map<String, Object> message = readMessage(bis)
                if (message == null) break
                handleMessage(message)
            }
        } catch (Exception e) {
            // Connection closed or error; stop silently if not running
        }
    }

    @SuppressWarnings('unchecked')
    private Map<String, Object> readMessage(InputStream stream) throws IOException {
        StringBuilder headerLine = new StringBuilder()
        int c
        while ((c = stream.read()) != -1) {
            if (c == 10) break          // '\n'
            if (c != 13) headerLine.append((char) c)  // '\r'
        }
        if (c == -1) return null

        String header = headerLine.toString().trim()
        if (!header.startsWith('Content-Length:')) return null

        int contentLength = header.substring('Content-Length:'.length()).trim() as int

        // Read empty line separator
        while ((c = stream.read()) != -1) {
            if (c == 10) break  // '\n'
        }

        byte[] content = new byte[contentLength]
        int bytesRead = 0
        while (bytesRead < contentLength) {
            int read = stream.read(content, bytesRead, contentLength - bytesRead)
            if (read == -1) throw new EOFException('Unexpected end of stream')
            bytesRead += read
        }

        (Map<String, Object>) slurper.parse(content)
    }

    @SuppressWarnings('unchecked')
    private void handleMessage(Map<String, Object> message) {
        // Response to our request
        if (message.containsKey('id') && (message.containsKey('result') || message.containsKey('error'))) {
            String id = message.id?.toString()
            CompletableFuture<Object> future = pendingRequests.get(id)
            if (future != null) {
                if (message.containsKey('error')) {
                    Map<String, Object> error = (Map<String, Object>) message.error
                    String errorMsg = error?.message?.toString() ?: 'Unknown error'
                    int code = (error?.code as Number)?.intValue() ?: -1
                    Object data = error != null ? error.get('data') : null
                    future.completeExceptionally(new JsonRpcException(code, errorMsg, data))
                } else {
                    future.complete(message.result)
                }
            }
            return
        }

        // Notification (no id, has method)
        if (message.containsKey('method') && !message.containsKey('id')) {
            String method = (String) message.method
            Map<String, Object> params = (Map<String, Object>) (message.params ?: [:])
            Closure handler = notificationHandler
            handler?.call(method, params)
            return
        }

        // Incoming request (has both method and id)
        if (message.containsKey('method') && message.containsKey('id')) {
            String method = (String) message.method
            // Echo the id exactly as received: the runtime uses numeric ids and ignores responses
            // whose id type differs.
            Object id = message.id
            Map<String, Object> params = (Map<String, Object>) (message.params ?: [:])

            Closure handler = requestHandlers.get(method)
            if (handler == null) {
                handlerExecutor.submit {
                    try { sendErrorResponse(id, -32601, "Method not found: $method") }
                    catch (Exception ignored) {}
                }
                return
            }

            handlerExecutor.submit {
                try {
                    Object raw = handler.call(params)
                    if (raw.is(NULL_RESULT)) {
                        sendMessage([jsonrpc: '2.0', id: id, result: null] as Map<String, Object>)
                    } else {
                        Map<String, Object> result = (Map<String, Object>) raw
                        sendResponse(id, result ?: [:] as Map<String, Object>)
                    }
                } catch (Exception e) {
                    try { sendErrorResponse(id, -32603, e.message) }
                    catch (Exception ignored) {}
                }
            }
        }
    }

    private void sendResponse(Object requestId, Map<String, Object> result) {
        sendMessage([jsonrpc: '2.0', id: requestId, result: result] as Map<String, Object>)
    }

    private void sendErrorResponse(Object requestId, int code, String message) {
        sendMessage([
            jsonrpc: '2.0',
            id     : requestId,
            error  : [code: code, message: message]
        ] as Map<String, Object>)
    }

    /**
     * Returns whether a request failed because the server does not implement the method
     * (JSON-RPC {@code -32601} or an "Unhandled method" error), walking the cause chain.
     *
     * @param error the failure thrown by {@link #request}
     * @param method the method that was called
     * @return true when the method is unknown to the server
     */
    static boolean isMethodNotFound(Throwable error, String method) {
        Throwable current = error
        while (current != null) {
            if (current instanceof JsonRpcException) {
                JsonRpcException rpcError = (JsonRpcException) current
                if (rpcError.code == -32601) return true
                String text = rpcError.message
                return text != null && text.contains("Unhandled method ${method}".toString())
            }
            current = current.cause
        }
        false
    }

    /** JSON-RPC error with code, message, and optional structured {@code data}. */
    @CompileStatic
    static class JsonRpcException extends Exception {
        final int code
        /** The error's {@code data} member (for example {@code policyHelperFailure}), or null. */
        final Object data

        JsonRpcException(int code, String message) {
            this(code, message, null)
        }

        JsonRpcException(int code, String message, Object data) {
            super("JSON-RPC Error ${code}: ${message}".toString())
            this.code = code
            this.data = data
        }
    }
}
