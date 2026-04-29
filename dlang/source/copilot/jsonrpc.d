/// JSON-RPC 2.0 transport with Content-Length framing.
///
/// Reads and writes messages over stdio or a TCP socket using the
/// Language Server Protocol framing convention.
module copilot.jsonrpc;

import std.json;
import std.conv : to, text;
import std.string : indexOf, strip;
import std.exception : enforce;
import std.typecons : Nullable;
import std.format : format;
import core.sync.mutex : Mutex;
import core.sync.condition : Condition;

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/// A pending RPC request awaiting its response.
private struct PendingRequest
{
    bool fulfilled = false;
    Nullable!JSONValue result;
    Nullable!string errorMsg;
    Condition cond;
}

/// Callback type for notifications.
alias NotificationHandler = void delegate(string method, JSONValue params) @safe;

// ---------------------------------------------------------------------------
// JsonRpcClient
// ---------------------------------------------------------------------------

/// Minimal JSON-RPC 2.0 client that works over any pair of input/output
/// streams (typically stdin/stdout of a child process).
final class JsonRpcClient
{
    import std.stdio : File;
    import core.thread : Thread;

    private
    {
        File _input;
        File _output;
        Mutex _writeMutex;
        Mutex _pendingMutex;
        int _nextId = 1;
        PendingRequest*[int] _pending;
        NotificationHandler _onNotification;
        Thread _readerThread;
        bool _running = false;
    }

    /// Construct a client over the given input (readable) and output (writable) files.
    this(File input, File output) @trusted
    {
        _input = input;
        _output = output;
        _writeMutex = new Mutex();
        _pendingMutex = new Mutex();
    }

    /// Register a handler invoked for every incoming notification.
    void onNotification(NotificationHandler handler) @safe
    {
        _onNotification = handler;
    }

    /// Start the background reader thread.
    void start() @trusted
    {
        if (_running) return;
        _running = true;
        _readerThread = new Thread(&_readerLoop);
        _readerThread.isDaemon = true;
        _readerThread.start();
    }

    /// Stop the reader and clean up.
    void stop() @trusted
    {
        _running = false;
        // Unblock all waiting requests.
        _pendingMutex.lock();
        scope(exit) _pendingMutex.unlock();
        foreach (ref pr; _pending)
        {
            pr.errorMsg = "client stopped";
            pr.fulfilled = true;
            pr.cond.notify();
        }
        _pending = null;
    }

    /// Send a JSON-RPC request and wait for the response.
    /// Returns the `result` field, or throws on error.
    JSONValue request(string method, JSONValue params = JSONValue(null)) @trusted
    {
        int id;
        PendingRequest* pr;

        // Allocate slot.
        {
            _pendingMutex.lock();
            scope(exit) _pendingMutex.unlock();
            id = _nextId++;
            pr = new PendingRequest();
            pr.cond = new Condition(_pendingMutex);
            _pending[id] = pr;
        }

        // Build and send the message.
        auto msg = JSONValue(string[string].init);
        msg["jsonrpc"] = "2.0";
        msg["id"] = id;
        msg["method"] = method;
        if (params.type != JSONType.null_)
            msg["params"] = params;

        _writeMessage(msg);

        // Wait for the response.
        {
            _pendingMutex.lock();
            scope(exit) _pendingMutex.unlock();
            while (!pr.fulfilled)
                pr.cond.wait();
        }

        if (!pr.errorMsg.isNull)
            throw new JsonRpcError(pr.errorMsg.get);

        return pr.result.isNull ? JSONValue(null) : pr.result.get;
    }

    /// Fire-and-forget notification (no `id` field).
    void notify(string method, JSONValue params = JSONValue(null)) @trusted
    {
        auto msg = JSONValue(string[string].init);
        msg["jsonrpc"] = "2.0";
        msg["method"] = method;
        if (params.type != JSONType.null_)
            msg["params"] = params;
        _writeMessage(msg);
    }

    // ------------------------------------------------------------------
    // Internal
    // ------------------------------------------------------------------

    /// Serialize a JSONValue and write it with Content-Length framing.
    private void _writeMessage(JSONValue msg) @trusted
    {
        auto body_ = msg.toString();
        auto header = format!"Content-Length: %d\r\n\r\n"(body_.length);

        _writeMutex.lock();
        scope(exit) _writeMutex.unlock();

        _output.rawWrite(cast(const(ubyte)[]) header);
        _output.rawWrite(cast(const(ubyte)[]) body_);
        _output.flush();
    }

    /// Background loop that reads Content-Length framed messages.
    private void _readerLoop() @trusted
    {
        try
        {
            while (_running)
            {
                auto msg = _readMessage();
                if (msg.isNull) break;
                _dispatch(msg.get);
            }
        }
        catch (Exception)
        {
            // Stream closed or parse error -- stop gracefully.
        }
    }

    /// Read one Content-Length framed message. Returns null on EOF.
    private Nullable!JSONValue _readMessage() @trusted
    {
        // Read headers until blank line.
        long contentLength = -1;

        while (true)
        {
            char[] buf;
            // Read a full header line.
            auto line = (() @trusted {
                char[] lineBuf;
                if (_input.eof) return cast(string) null;
                foreach (c; _input.byChunk(1))
                {
                    lineBuf ~= cast(char[]) c;
                    if (lineBuf.length >= 2 &&
                        lineBuf[$ - 2] == '\r' && lineBuf[$ - 1] == '\n')
                    {
                        return cast(string) lineBuf[0 .. $ - 2];
                    }
                }
                return lineBuf.length > 0 ? cast(string) lineBuf : null;
            })();

            if (line is null)
                return Nullable!JSONValue.init;

            if (line.length == 0)
                break; // End of headers.

            // Parse Content-Length.
            enum prefix = "Content-Length: ";
            if (line.length > prefix.length && line[0 .. prefix.length] == prefix)
                contentLength = to!long(line[prefix.length .. $].strip());
        }

        if (contentLength <= 0)
            return Nullable!JSONValue.init;

        // Read exactly contentLength bytes.
        auto bodyBuf = new char[](cast(size_t) contentLength);
        size_t read = 0;
        while (read < bodyBuf.length)
        {
            auto chunk = (() @trusted {
                auto slice = cast(ubyte[]) bodyBuf[read .. $];
                return _input.rawRead(slice);
            })();
            if (chunk.length == 0) break;
            read += chunk.length;
        }

        if (read < bodyBuf.length)
            return Nullable!JSONValue.init;

        return Nullable!JSONValue(parseJSON(cast(string) bodyBuf));
    }

    /// Route an incoming message to the appropriate handler.
    private void _dispatch(JSONValue msg) @trusted
    {
        // Response (has `id` and either `result` or `error`).
        auto pId = "id" in msg;
        if (pId !is null && (*pId).type == JSONType.integer)
        {
            int id = cast(int) (*pId).integer;
            _pendingMutex.lock();
            scope(exit) _pendingMutex.unlock();

            auto pp = id in _pending;
            if (pp is null) return;
            auto pr = *pp;

            auto pErr = "error" in msg;
            if (pErr !is null)
            {
                auto pErrMsg = "message" in *pErr;
                pr.errorMsg = (pErrMsg !is null && (*pErrMsg).type == JSONType.string)
                    ? (*pErrMsg).str
                    : "unknown RPC error";
            }
            else
            {
                auto pRes = "result" in msg;
                pr.result = (pRes !is null) ? *pRes : JSONValue(null);
            }
            pr.fulfilled = true;
            pr.cond.notify();
            _pending.remove(id);
            return;
        }

        // Notification (no `id`).
        auto pMethod = "method" in msg;
        if (pMethod !is null && (*pMethod).type == JSONType.string)
        {
            if (_onNotification !is null)
            {
                auto pParams = "params" in msg;
                auto params = (pParams !is null) ? *pParams : JSONValue(null);
                try { _onNotification((*pMethod).str, params); } catch (Exception) {}
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Error type
// ---------------------------------------------------------------------------

/// Exception raised when a JSON-RPC request returns an error.
class JsonRpcError : Exception
{
    this(string msg, string file = __FILE__, size_t line = __LINE__) @safe pure nothrow
    {
        super(msg, file, line);
    }
}
