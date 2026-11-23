/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot.internal;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.*;
import java.util.function.BiConsumer;

/**
 * Minimal JSON-RPC 2.0 client for stdio/TCP transport.
 * Uses threads for blocking IO and provides a synchronous + async interface.
 */
public class JsonRpcClient {
    private final InputStream inputStream;
    private final OutputStream outputStream;
    private final ObjectMapper mapper = new ObjectMapper();
    private final ConcurrentHashMap<String, CompletableFuture<Object>> pendingRequests = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, RequestHandler> requestHandlers = new ConcurrentHashMap<>();
    private volatile BiConsumer<String, Map<String, Object>> notificationHandler;
    private volatile boolean running;
    private Thread readThread;
    private final Object writeLock = new Object();
    private final ExecutorService handlerExecutor = Executors.newCachedThreadPool(r -> {
        Thread t = new Thread(r, "jsonrpc-handler");
        t.setDaemon(true);
        return t;
    });

    /**
     * Result a {@link RequestHandler} returns to answer a request with a JSON {@code null}
     * result (a handler that returns Java {@code null} answers with an empty object).
     */
    public static final Map<String, Object> NULL_RESULT = java.util.Collections.unmodifiableMap(new HashMap<>());

    /** Functional interface for handling incoming JSON-RPC requests. */
    @FunctionalInterface
    public interface RequestHandler {
        /**
         * Handles an incoming JSON-RPC request.
         *
         * @param params the request parameters
         * @return the response payload
         * @throws Exception if request handling fails
         */
        Map<String, Object> handle(Map<String, Object> params) throws Exception;
    }

    /**
     * Creates a JSON-RPC client.
     *
     * @param inputStream the input stream to read from
     * @param outputStream the output stream to write to
     */
    public JsonRpcClient(InputStream inputStream, OutputStream outputStream) {
        this.inputStream = inputStream;
        this.outputStream = outputStream;
    }

    /** Starts the JSON-RPC reader loop. */
    public void start() {
        if (running) return;
        running = true;
        readThread = new Thread(this::readLoop, "jsonrpc-reader");
        readThread.setDaemon(true);
        readThread.start();
    }

    /** Stops the client and fails pending requests. */
    public void stop() {
        running = false;
        handlerExecutor.shutdownNow();
        if (readThread != null) {
            readThread.interrupt();
        }
        // Complete all pending requests with error
        for (var entry : pendingRequests.entrySet()) {
            entry.getValue().completeExceptionally(new IOException("Client stopped"));
        }
        pendingRequests.clear();
    }

    /**
     * Sets the notification handler.
     *
     * @param handler the notification handler
     */
    public void setNotificationHandler(BiConsumer<String, Map<String, Object>> handler) {
        this.notificationHandler = handler;
    }

    /**
     * Registers a handler for an incoming request method.
     *
     * @param method the request method name
     * @param handler the request handler, or null to remove it
     */
    public void setRequestHandler(String method, RequestHandler handler) {
        if (handler == null) {
            requestHandlers.remove(method);
        } else {
            requestHandlers.put(method, handler);
        }
    }

    /**
     * Send a JSON-RPC request and wait for the response.
     */
    @SuppressWarnings("unchecked")
    public Map<String, Object> request(String method, Object params) throws Exception {
        return request(method, params, 30, TimeUnit.SECONDS);
    }

    /**
     * Sends a JSON-RPC request and waits for the response.
     *
     * @param method the request method
     * @param params the request parameters
     * @param timeout the request timeout
     * @param unit the timeout unit
     * @return the response payload
     * @throws Exception if the request fails or times out
     */
    @SuppressWarnings("unchecked")
    public Map<String, Object> request(String method, Object params, long timeout, TimeUnit unit) throws Exception {
        String requestId = UUID.randomUUID().toString();
        CompletableFuture<Object> future = new CompletableFuture<>();
        pendingRequests.put(requestId, future);

        Map<String, Object> message = Map.of(
            "jsonrpc", "2.0",
            "id", requestId,
            "method", method,
            "params", params != null ? params : Map.of()
        );

        sendMessage(message);

        try {
            Object result = future.get(timeout, unit);
            if (result instanceof Map) {
                return (Map<String, Object>) result;
            }
            if (result == null) {
                // Void methods (e.g. managedSettings.clearCache) answer with a null result.
                return new HashMap<>();
            }
            return Map.of("result", result);
        } finally {
            pendingRequests.remove(requestId);
        }
    }

    /**
     * Returns whether a request failed because the server does not implement the method
     * (JSON-RPC error -32601 or an "Unhandled method" message), e.g. an older CLI.
     *
     * @param error the failure thrown by {@link #request}
     * @param method the requested method name
     * @return true if the failure is a method-not-found error
     */
    public static boolean isMethodNotFound(Throwable error, String method) {
        Throwable current = error;
        while (current != null) {
            if (current instanceof JsonRpcException) {
                JsonRpcException rpcError = (JsonRpcException) current;
                if (rpcError.code == -32601) return true;
                String message = rpcError.getMessage();
                return message != null && message.contains("Unhandled method " + method);
            }
            current = current.getCause();
        }
        return false;
    }

    private void sendMessage(Map<String, Object> message) throws JsonProcessingException, IOException {
        String content = mapper.writeValueAsString(message);
        byte[] contentBytes = content.getBytes(StandardCharsets.UTF_8);
        String header = "Content-Length: " + contentBytes.length + "\r\n\r\n";

        synchronized (writeLock) {
            outputStream.write(header.getBytes(StandardCharsets.UTF_8));
            outputStream.write(contentBytes);
            outputStream.flush();
        }
    }

    private void readLoop() {
        try (BufferedInputStream bis = new BufferedInputStream(inputStream)) {
            while (running) {
                Map<String, Object> message = readMessage(bis);
                if (message == null) break;
                handleMessage(message);
            }
        } catch (Exception e) {
            if (running) {
                // Connection error
            }
        }
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> readMessage(InputStream in) throws IOException {
        // Read header line
        StringBuilder headerLine = new StringBuilder();
        int c;
        while ((c = in.read()) != -1) {
            if (c == '\n') break;
            if (c != '\r') headerLine.append((char) c);
        }
        if (c == -1) return null;

        String header = headerLine.toString().trim();
        if (!header.startsWith("Content-Length:")) return null;

        int contentLength = Integer.parseInt(header.substring("Content-Length:".length()).trim());

        // Read empty line
        while ((c = in.read()) != -1) {
            if (c == '\n') break;
        }

        // Read content
        byte[] content = new byte[contentLength];
        int bytesRead = 0;
        while (bytesRead < contentLength) {
            int read = in.read(content, bytesRead, contentLength - bytesRead);
            if (read == -1) throw new EOFException("Unexpected end of stream");
            bytesRead += read;
        }

        return mapper.readValue(content, Map.class);
    }

    @SuppressWarnings("unchecked")
    private void handleMessage(Map<String, Object> message) {
        // Response to our request
        if (message.containsKey("id") && (message.containsKey("result") || message.containsKey("error"))) {
            String id = String.valueOf(message.get("id"));
            CompletableFuture<Object> future = pendingRequests.get(id);
            if (future != null) {
                if (message.containsKey("error")) {
                    Map<String, Object> error = (Map<String, Object>) message.get("error");
                    String errorMsg = String.valueOf(error.getOrDefault("message", "Unknown error"));
                    future.completeExceptionally(new JsonRpcException(
                        ((Number) error.getOrDefault("code", -1)).intValue(),
                        errorMsg,
                        error.get("data")
                    ));
                } else {
                    future.complete(message.get("result"));
                }
            }
            return;
        }

        // Notification (no id)
        if (message.containsKey("method") && !message.containsKey("id")) {
            String method = (String) message.get("method");
            Map<String, Object> params = (Map<String, Object>) message.getOrDefault("params", Map.of());
            BiConsumer<String, Map<String, Object>> handler = notificationHandler;
            if (handler != null) {
                handler.accept(method, params);
            }
            return;
        }

        // Incoming request (has both method and id)
        if (message.containsKey("method") && message.containsKey("id")) {
            String method = (String) message.get("method");
            // Echo the id exactly as received: the runtime uses numeric ids and ignores responses
            // whose id type differs.
            Object id = message.get("id");
            Map<String, Object> params = (Map<String, Object>) message.getOrDefault("params", Map.of());

            RequestHandler handler = requestHandlers.get(method);
            if (handler == null) {
                handlerExecutor.submit(() -> {
                    try {
                        sendErrorResponse(id, -32601, "Method not found: " + method);
                    } catch (Exception ignored) {}
                });
                return;
            }

            handlerExecutor.submit(() -> {
                try {
                    Map<String, Object> result = handler.handle(params);
                    if (result == NULL_RESULT) {
                        sendResponse(id, null);
                    } else {
                        sendResponse(id, result != null ? result : Map.of());
                    }
                } catch (Exception e) {
                    try {
                        sendErrorResponse(id, -32603, e.getMessage());
                    } catch (Exception ignored) {}
                }
            });
        }
    }

    private void sendResponse(Object requestId, Map<String, Object> result) throws Exception {
        // HashMap (unlike Map.of) keeps a null result, which serializes as JSON null.
        Map<String, Object> response = new HashMap<>();
        response.put("jsonrpc", "2.0");
        response.put("id", requestId);
        response.put("result", result);
        sendMessage(response);
    }

    private void sendErrorResponse(Object requestId, int code, String message) throws Exception {
        Map<String, Object> error = new HashMap<>();
        error.put("code", code);
        error.put("message", message != null ? message : "Internal error");
        Map<String, Object> response = new HashMap<>();
        response.put("jsonrpc", "2.0");
        response.put("id", requestId);
        response.put("error", error);
        String json = mapper.writeValueAsString(response);
        byte[] contentBytes = json.getBytes(StandardCharsets.UTF_8);
        String header = "Content-Length: " + contentBytes.length + "\r\n\r\n";
        synchronized (writeLock) {
            outputStream.write(header.getBytes(StandardCharsets.UTF_8));
            outputStream.write(contentBytes);
            outputStream.flush();
        }
    }

    /** Exception thrown when a JSON-RPC request returns an error. */
    public static class JsonRpcException extends Exception {
        public final int code;
        /**
         * The error's {@code data} member, or null. Session create/resume failures caused by a
         * Policy Helper carry {@code data.policyHelperFailure}
         * (see {@code Types.PolicyHelperFailure.fromError}).
         */
        public final Object data;

        /**
         * Creates a JSON-RPC exception.
         *
         * @param code the JSON-RPC error code
         * @param message the error message
         */
        public JsonRpcException(int code, String message) {
            this(code, message, null);
        }

        /**
         * Creates a JSON-RPC exception carrying the error's {@code data} member.
         *
         * @param code the JSON-RPC error code
         * @param message the error message
         * @param data the error data, or null
         */
        public JsonRpcException(int code, String message, Object data) {
            super("JSON-RPC Error " + code + ": " + message);
            this.code = code;
            this.data = data;
        }
    }
}
