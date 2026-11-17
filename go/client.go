// Package copilot provides a Go SDK for interacting with the GitHub Copilot CLI.
//
// The copilot package enables Go applications to communicate with the Copilot CLI
// server, create and manage conversation sessions, and integrate custom tools.
//
// Basic usage:
//
//	client := copilot.NewClient(&copilot.ClientOptions{Mode: copilot.ModeCodingAgent})
//	if err := client.Start(); err != nil {
//	    log.Fatal(err)
//	}
//	defer client.Stop()
//
//	session, err := client.CreateSession(&copilot.CreateSessionOptions{
//	    OnPermissionRequest: copilot.PermissionHandler.ApproveAll,
//	    Model: "gpt-5.4",
//	})
//	if err != nil {
//	    log.Fatal(err)
//	}
//
//	session.On(func(event copilot.SessionEvent) {
//	    if d, ok := event.Data.(*copilot.AssistantMessageData); ok {
//	        fmt.Println(d.Content)
//	    }
//	})
//
//	session.Send(copilot.MessageOptions{Prompt: "Hello!"})
package copilot

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"net"
	"net/netip"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/debug"
	"sort"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"syscall"
	"time"
	"unicode"
	"unicode/utf8"

	"github.com/google/uuid"

	"github.com/microsoft/copilot-sdk/go/v2/internal/embeddedcli"
	"github.com/microsoft/copilot-sdk/go/v2/internal/jsonrpc2"
	"github.com/microsoft/copilot-sdk/go/v2/internal/truncbuffer"
	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

type cloudSkillProviderError struct{}

func (cloudSkillProviderError) Error() string {
	return "Skill providers are not supported for cloud sessions."
}

type disabledMemoryStoreError struct{}

func (disabledMemoryStoreError) Error() string {
	return "StoreMemory requires Memory.Enabled to be true."
}

type cloudMemoryStoreError struct{}

func (cloudMemoryStoreError) Error() string {
	return "Memory stores are not supported for cloud sessions."
}

// requestedMemoryStore returns the memory store callback, failing when it is
// set without enabling memory.
func requestedMemoryStore(
	memory *MemoryConfiguration,
	store func(context.Context, StoredMemory) error,
) (func(context.Context, StoredMemory) error, error) {
	if store == nil {
		return nil, nil
	}
	if memory == nil || !memory.Enabled {
		return nil, disabledMemoryStoreError{}
	}
	return store, nil
}

// collectBearerTokenProviders gathers the per-provider [BearerTokenProvider] callbacks
// from the singular provider and any named providers, keyed by provider name. The
// singular provider uses the implicit name "default"; named providers use their
// own Name. Returns nil when no callbacks are configured.
func collectBearerTokenProviders(providers []NamedProviderConfig) map[string]BearerTokenProvider {
	callbacks := make(map[string]BearerTokenProvider)
	for i := range providers {
		if providers[i].BearerTokenProvider != nil {
			callbacks[providers[i].Name] = providers[i].BearerTokenProvider
		}
	}
	if len(callbacks) == 0 {
		return nil
	}
	return callbacks
}

func validateSessionFSConfig(config *SessionFSConfig) error {
	if config == nil {
		return nil
	}
	if config.InitialWorkingDirectory == "" {
		return errors.New("SessionFS.InitialWorkingDirectory is required")
	}
	if config.SessionStatePath == "" {
		return errors.New("SessionFS.SessionStatePath is required")
	}
	if config.Conventions != rpc.SessionFSSetProviderConventionsPosix && config.Conventions != rpc.SessionFSSetProviderConventionsWindows {
		return errors.New("SessionFS.Conventions must be either 'posix' or 'windows'")
	}
	return nil
}

func validateConnectionOptions(connection RuntimeConnection, opts *clientOptions) {
	if _, ok := connection.(TCPConnection); ok {
		var runtimeOnlyOptions []string
		if opts.BaseDirectory != "" {
			runtimeOnlyOptions = append(runtimeOnlyOptions, "Runtime.BaseDirectory")
		}
		if opts.GitHubToken != "" {
			runtimeOnlyOptions = append(runtimeOnlyOptions, "GitHub.Token")
		}
		if opts.UseLoggedInUser != nil {
			runtimeOnlyOptions = append(runtimeOnlyOptions, "GitHub.UseLoggedInUser")
		}
		if opts.LogLevel != "" {
			runtimeOnlyOptions = append(runtimeOnlyOptions, "Observability.LogLevel")
		}
		if opts.Telemetry != nil {
			runtimeOnlyOptions = append(runtimeOnlyOptions, "Observability.Telemetry")
		}
		if opts.SessionIdleTimeoutSeconds != 0 {
			runtimeOnlyOptions = append(runtimeOnlyOptions, "Runtime.SessionIdleTimeoutSeconds")
		}
		if len(runtimeOnlyOptions) != 0 {
			panic(strings.Join(runtimeOnlyOptions, ", ") + " cannot be used with TCPConnection (external runtime manages its own startup configuration)")
		}
	}
	if _, ok := connection.(InProcessConnection); ok {
		if opts.Telemetry != nil && !utf8.ValidString(opts.Telemetry.FilePath) {
			panic("Telemetry.FilePath must be valid UTF-8 with InProcessConnection")
		}
	}
}

// Client manages the connection to the Copilot CLI server and provides session management.
//
// The Client hosts the native runtime in-process by default, or uses an explicit
// subprocess or external-server connection.
// It handles JSON-RPC communication, session lifecycle, tool execution, and permission requests.
//
// Example:
//
//	// Create a client with default transport (hosts the runtime in-process)
//	client := copilot.NewClient(&copilot.ClientOptions{Mode: copilot.ModeCodingAgent})
//
//	// Or connect to an existing server
//	client := copilot.NewClient(&copilot.ClientOptions{
//	    Mode: copilot.ModeCodingAgent,
//	    Runtime: copilot.TCPConnection{Host: "localhost", Port: 3000},
//	})
//
//	if err := client.Start(); err != nil {
//	    log.Fatal(err)
//	}
//	defer client.Stop()
type Client struct {
	options                               clientOptions
	process                               *exec.Cmd
	processStdin                          io.WriteCloser
	client                                *jsonrpc2.Client
	actualPort                            atomic.Int64 // read by RuntimePort without startStopMux
	actualHost                            string
	state                                 connectionState
	sessions                              map[string]*Session
	sessionsMux                           sync.Mutex
	sessionsGeneration                    uint64
	autoModeRecoveryCleanups              map[*jsonrpc2.Client]map[string]<-chan struct{}
	pendingShellAuthenticationRequests    map[shellAuthenticationKey][]*shellAuthenticationTicket
	pendingShellAuthenticationRequestsMux sync.Mutex
	ahp                                   ahpHostState
	gitHubTokenProviders                  map[string]GitHubTokenProvider
	gitHubTokenProvidersMux               sync.RWMutex
	requestAdapter                        *copilotRequestAdapter
	requestAdapterMux                     sync.Mutex
	installationConfirmationAdapter       *installationConfirmationAdapter
	installationConfirmationAdapterMux    sync.Mutex
	sessionOperations                     map[string]*sessionOperation
	sessionOperationsMux                  sync.Mutex
	activeSessionCallbacks                map[string]int
	isExternalServer                      bool
	conn                                  net.Conn // stores net.Conn for external TCP connections
	useStdio                              bool     // resolved value from options
	useInProcess                          bool     // true for InProcessConnection (FFI transport)
	ffiHost                               inProcessHost
	runOnUiThread                         func(action func() error) error
	// resolved process options for the spawned runtime (zero values for TCPConnection)
	cliPath            string
	cliArgs            []string
	environment        []string
	workingDirectory   string
	port               int
	tcpConnectionToken string

	modelsCache               []Model
	modelsCacheMux            sync.Mutex
	lifecycleHandlers         map[uint64]SessionLifecycleHandler
	typedLifecycleHandlers    map[SessionLifecycleEventType]map[uint64]SessionLifecycleHandler
	nextLifecycleHandlerID    uint64
	lifecycleHandlersMux      sync.Mutex
	startStopMux              sync.RWMutex // protects process and state during start/[force]stop
	transportMux              sync.Mutex   // guards startup, stop fencing and transport visibility; see ensureConnected
	startup                   *startupAttempt
	lastStartup               *startupAttempt
	stopGeneration            uint64
	stopsInProgress           int
	testHookTransportAcquired func() // tests only: runs after ensureConnected acquires a transport
	processDone               chan struct{}
	processErrorPtr           *error
	osProcess                 atomic.Pointer[os.Process]
	negotiatedProtocolVersion int
	// effectiveConnectionToken is the token sent in `connect`; auto-generated when
	// the SDK spawns its own CLI in TCP mode.
	effectiveConnectionToken string
	onListModels             func(ctx context.Context) ([]Model, error)

	// RPC provides typed server-scoped RPC methods.
	// This field is nil until the client is connected via Start().
	RPC *rpc.ServerRPC

	// internalRPC provides SDK-internal RPC methods (handshake helpers etc.).
	// Lowercase = not exported; external callers cannot reach it.
	internalRPC *rpc.InternalServerRPC
}

type sessionOperation struct {
	permit chan struct{}
	users  int
}

// NewClient creates a new Copilot runtime client with the given options.
//
// Options and [ClientOptions.Mode] are required. The client is not connected
// after creation; call [Client.Start] to
// connect, or simply call [Client.CreateSession]/[Client.ResumeSession], which
// auto-start the runtime on first use.
//
// Example:
//
//	// Default transport: bundled runtime in-process
//	client := copilot.NewClient(&copilot.ClientOptions{Mode: copilot.ModeCodingAgent})
//
//	// Custom CLI path over stdio
//	client := copilot.NewClient(&copilot.ClientOptions{
//	    Mode: copilot.ModeCodingAgent,
//	    Runtime: copilot.SpawnedStdioConnection{Path: "/usr/local/bin/copilot"},
//	    Observability: copilot.ClientObservabilityOptions{LogLevel: "debug"},
//	})
//
//	// Connect to an already-running runtime
//	client := copilot.NewClient(&copilot.ClientOptions{
//	    Mode: copilot.ModeCodingAgent,
//	    Runtime: copilot.TCPConnection{Host: "localhost", Port: 8080},
//	})
func NewClient(options *ClientOptions) *Client {
	if options == nil || (options.Mode != ModeCodingAgent && options.Mode != ModeEmpty) {
		panic("ClientOptions.Mode is required and must be ModeCodingAgent or ModeEmpty")
	}
	opts := *options.normalize()

	client := &Client{
		options:              opts,
		state:                stateDisconnected,
		sessions:             make(map[string]*Session),
		gitHubTokenProviders: make(map[string]GitHubTokenProvider),
		actualHost:           "localhost",
		isExternalServer:     false,
		useStdio:             true,
	}

	for _, path := range opts.BuiltinPluginDirectories {
		if !filepath.IsAbs(path) {
			panic(fmt.Sprintf("BuiltinPluginDirectories must contain only absolute paths: %s", path))
		}
	}
	opts.BuiltinPluginDirectories = append([]string(nil), opts.BuiltinPluginDirectories...)

	// Resolve the connection from the host environment unless explicitly supplied.
	connection := opts.Connection
	if connection == nil {
		connection = resolveDefaultConnection(os.Environ())
	}
	switch conn := connection.(type) {
	case SpawnedStdioConnection:
		client.useStdio = true
		client.cliPath = conn.Path
		if len(conn.Args) > 0 {
			client.cliArgs = append([]string{}, conn.Args...)
		}
	case SpawnedTCPConnection:
		client.useStdio = false
		client.cliPath = conn.Path
		if len(conn.Args) > 0 {
			client.cliArgs = append([]string{}, conn.Args...)
		}
		client.port = conn.Port
		client.tcpConnectionToken = conn.ConnectionToken
	case TCPConnection:
		if conn.Host == "" || strings.ContainsAny(conn.Host, "/\\?#@[]") {
			panic("TCPConnection Host must be a hostname or IP address without a scheme, port, or path")
		}
		if strings.Contains(conn.Host, ":") {
			if address, err := netip.ParseAddr(conn.Host); err != nil || !address.Is6() {
				panic("TCPConnection Host must be a hostname or IP address without a scheme, port, or path")
			}
		} else if strings.ContainsFunc(conn.Host, unicode.IsSpace) {
			panic("TCPConnection Host must be a hostname or IP address without a scheme, port, or path")
		}
		if conn.Port <= 0 || conn.Port > 65535 {
			panic("TCPConnection Port must be from 1 through 65535")
		}
		client.actualHost = conn.Host
		client.actualPort.Store(int64(conn.Port))
		client.isExternalServer = true
		client.useStdio = false
		client.tcpConnectionToken = conn.ConnectionToken
	case InProcessConnection:
		client.useStdio = false
		client.useInProcess = true
		client.runOnUiThread = conn.RunOnUiThread
		if opts.Telemetry != nil {
			telemetry := *opts.Telemetry
			if telemetry.CaptureContent != nil {
				telemetry.CaptureContent = Bool(*telemetry.CaptureContent)
			}
			opts.Telemetry = &telemetry
		}
	default:
		panic(fmt.Sprintf("unknown RuntimeConnection type: %T", connection))
	}

	// Validate transport-specific option constraints before resolving defaults.
	validateConnectionOptions(connection, &opts)
	if client.isExternalServer && opts.InternalTestOptions != nil {
		panic("InternalTestOptions requires a runtime owned by this client")
	}

	// Preserve an explicitly empty child environment instead of inheriting.
	if cp, ok := connection.(childProcessConnection); ok {
		client.environment = cp.connEnv()
		client.workingDirectory = cp.connWorkingDirectory()
	}

	if client.environment == nil {
		client.environment = os.Environ()
	}
	client.environment = append([]string{}, client.environment...)

	// Check the effective environment for a child-process runtime override.
	if client.cliPath == "" && !client.useInProcess {
		if cliPath := getEnvValue(client.environment, "COPILOT_CLI_PATH"); cliPath != "" {
			client.cliPath = cliPath
		}
	}

	// Resolve the effective connection token: explicit value if set; else if the SDK
	// spawns its own runtime in TCP mode, generate a UUID; otherwise empty. The
	// in-process transport uses no socket, so it needs no connection token.
	if client.tcpConnectionToken != "" {
		client.effectiveConnectionToken = client.tcpConnectionToken
	} else if !client.useStdio && !client.isExternalServer && !client.useInProcess {
		client.effectiveConnectionToken = uuid.NewString()
	}

	if opts.OnListModels != nil {
		client.onListModels = opts.OnListModels
	}
	if opts.SessionFS != nil {
		if err := validateSessionFSConfig(opts.SessionFS); err != nil {
			panic(err.Error())
		}
	}
	if opts.Network != nil {
		// Copy so later changes by the caller cannot make the registration and
		// the installed handlers disagree.
		network := *opts.Network
		opts.Network = &network
	}

	client.options = opts
	validateNewClientForMode(&client.options)
	return client
}

func resolveRuntimeExecutable(explicitPath, bundledRuntimePath string) (string, error) {
	if explicitPath != "" {
		return explicitPath, nil
	}
	if bundledRuntimePath == "" {
		return "", errors.New(
			"managed Copilot runtime unavailable: the embedded bundle does not contain copilot-runtime and adjacent runtime.node; regenerate the bundle, provide an explicit path, or set COPILOT_CLI_PATH",
		)
	}
	return bundledRuntimePath, nil
}

// exec.Command otherwise searches the parent PATH before Cmd.Env can be set.
func lookPathInEnvironment(name string, env []string, workingDirectory string) (string, error) {
	if filepath.Base(name) != name {
		return name, nil
	}
	extensions := []string{""}
	if runtime.GOOS == "windows" {
		pathExt := getEnvValue(env, "PATHEXT")
		if pathExt == "" {
			pathExt = ".COM;.EXE;.BAT;.CMD"
		}
		if filepath.Ext(name) == "" {
			extensions = nil
		}
		extensions = append(extensions, strings.Split(pathExt, ";")...)
	}
	lookupErr := exec.ErrNotFound
	var relativePath string
	var applicationDirectory string
	// filepath.Join would fold POSIX ".." segments before following symlinks.
	join := func(directory, name string) string {
		if runtime.GOOS == "windows" {
			return filepath.Join(directory, name)
		}
		if directory == "" {
			return name
		}
		if os.IsPathSeparator(directory[len(directory)-1]) {
			return directory + name
		}
		return directory + string(os.PathSeparator) + name
	}
	for _, directory := range filepath.SplitList(getEnvValue(env, "PATH")) {
		if directory == "" {
			directory = "."
		}
		for _, extension := range extensions {
			candidate := join(directory, name+extension)
			// Rooted and drive-qualified entries retain their own resolution.
			if filepath.VolumeName(directory) == "" && !os.IsPathSeparator(directory[0]) {
				candidate = join(workingDirectory, candidate)
			}
			if runtime.GOOS == "windows" {
				var err error
				candidate, err = filepath.Abs(candidate)
				if err != nil {
					return "", err
				}
			} else if !filepath.IsAbs(candidate) {
				if applicationDirectory == "" {
					var err error
					applicationDirectory, err = os.Getwd()
					if err != nil {
						return "", err
					}
				}
				candidate = join(applicationDirectory, candidate)
			}
			info, err := os.Stat(candidate)
			if err != nil {
				if !errors.Is(err, os.ErrNotExist) && !errors.Is(err, syscall.ENOTDIR) {
					lookupErr = err
				}
				continue
			}
			if info.IsDir() {
				continue
			}
			path, err := exec.LookPath(candidate)
			if err != nil {
				lookupErr = err
				continue
			}
			// Windows accepts a later absolute PATH entry only for the same file.
			if relativePath != "" {
				relativeInfo, relativeErr := os.Lstat(relativePath)
				candidateInfo, candidateErr := os.Lstat(path)
				if relativeErr != nil || candidateErr != nil || !os.SameFile(relativeInfo, candidateInfo) {
					return "", &exec.Error{Name: name, Err: exec.ErrDot}
				}
			}
			if !filepath.IsAbs(directory) {
				if runtime.GOOS != "windows" {
					return "", &exec.Error{Name: name, Err: exec.ErrDot}
				}
				if relativePath == "" {
					relativePath = path
				}
				continue
			}
			return path, nil
		}
	}
	if relativePath != "" {
		lookupErr = exec.ErrDot
	}
	return "", &exec.Error{Name: name, Err: lookupErr}
}

const defaultConnectionEnvVar = "COPILOT_SDK_DEFAULT_CONNECTION"

// resolveDefaultConnection selects the transport when no explicit connection
// was supplied. The override is primarily used by hosts and the E2E transport
// matrix; explicit connection options always take precedence.
func resolveDefaultConnection(env []string) RuntimeConnection {
	value := getEnvValue(env, defaultConnectionEnvVar)
	switch {
	case strings.EqualFold(value, "stdio"):
		return SpawnedStdioConnection{}
	case value == "", strings.EqualFold(value, "inprocess"):
		return InProcessConnection{}
	default:
		panic(fmt.Sprintf(
			"invalid %s value %q: expected \"inprocess\", \"stdio\", or unset",
			defaultConnectionEnvVar,
			value,
		))
	}
}

// getEnvValue looks up a key in an environment slice ([]string of "KEY=VALUE").
// Keys are case-insensitive on Windows and case-sensitive elsewhere.
// Returns the value if found, or empty string otherwise.
func getEnvValue(env []string, key string) string {
	for i := len(env) - 1; i >= 0; i-- {
		name, value, ok := strings.Cut(env[i], "=")
		if ok && (name == key || runtime.GOOS == "windows" && strings.EqualFold(name, key)) {
			return value
		}
	}
	return ""
}

// setEnvValue returns a copy of env with all existing entries for key removed and
// a single trailing KEY=VALUE entry added so SDK-managed values win deterministically.
func setEnvValue(env []string, key string, value string) []string {
	prefix := key + "="
	filtered := make([]string, 0, len(env)+1)
	for _, entry := range env {
		if !strings.HasPrefix(entry, prefix) {
			filtered = append(filtered, entry)
		}
	}
	return append(filtered, key+"="+value)
}

// Start starts the runtime host (unless using an external server) and establishes
// a connection.
//
// If connecting to an external server (via TCPConnection), only establishes the connection.
// Otherwise, starts an in-process host or the explicitly selected subprocess.
//
// Management helpers call this method automatically when the client is not connected.
// Operations called while Start is in progress wait for it and fail with its
// error if it fails.
//
// Returns an error if the server fails to start or the connection fails.
//
// Example:
//
//	client := copilot.NewClient(&copilot.ClientOptions{Mode: copilot.ModeCodingAgent})
//	if err := client.Start(context.Background()); err != nil {
//	    log.Fatal("Failed to start:", err)
//	}
//	// Now ready to create sessions
func (c *Client) Start(ctx context.Context) error {
	return c.start(ctx, nil)
}

func (c *Client) start(ctx context.Context, expected *transportGeneration) (err error) {
	c.startStopMux.Lock()
	defer c.startStopMux.Unlock()

	if expected != nil {
		c.transportMux.Lock()
		stopped := c.stopsInProgress != 0 || c.stopGeneration != expected.stop
		attempt := c.lastStartup
		c.transportMux.Unlock()
		if stopped {
			return errors.New("client is stopping or was stopped")
		}
		// Another caller may have completed startup while this one waited for
		// startStopMux. Join its outcome instead of retrying a failed attempt.
		if attempt != expected.startup {
			return attempt.err
		}
	}

	if c.state == stateConnected && c.client != nil {
		select {
		case <-c.client.ConnectionClosed():
			// The close callback may still be waiting to publish disconnected state.
		case <-c.processDone:
			// The runtime exited and requests already fail with its exit error,
			// but the reader may still be running a handler and has not reached
			// the end of the stream to close the connection.
		default:
			return nil
		}
	}

	if c.client != nil {
		if err := c.killProcessAndWait(); err != nil {
			return fmt.Errorf("failed to retire previous runtime: %w", err)
		}
		// Nothing listens on the retired runtime's port any more.
		if !c.isExternalServer {
			c.actualPort.Store(0)
		}
		// Do not join the lost connection's reader: the caller may be a handler
		// on it, such as a lifecycle listener that saw a request fail when the
		// runtime exited. The reader exits once that handler returns.
		c.client.Close()
		// Sessions are bound to the lost connection, so the new one cannot
		// serve them; retire them as ForceStop does so their waiters fail now.
		c.disconnectAhpHosts()
		c.retireSessionsLocally()
	}

	// Publish startup before creating the transport and while holding
	// startStopMux, so ensureConnected never observes a partially started
	// transport and never waits on startup while Stop holds the lock.
	startup := &startupAttempt{done: make(chan struct{})}
	c.transportMux.Lock()
	c.startup = startup
	c.lastStartup = startup
	c.transportMux.Unlock()
	defer func() {
		c.transportMux.Lock()
		c.startup = nil
		startup.finish(err)
		c.transportMux.Unlock()
	}()

	c.state = stateConnecting

	// Only start a runtime host if not connecting to an external server.
	if !c.isExternalServer {
		if err := c.startCLIServer(ctx); err != nil {
			c.process = nil
			c.state = stateError
			return err
		}
	}

	// Connect to the server
	if err := c.connectToServer(ctx); err != nil {
		killErr := c.killProcess()
		if !c.isExternalServer {
			c.actualPort.Store(0)
		}
		c.state = stateError
		return errors.Join(err, killErr)
	}

	if err := c.initializeConnection(ctx); err != nil {
		// Fail waiting operations first: one may be a handler on the reader that
		// closing the connection joins.
		startup.finish(err)
		// The runtime keeps registrations made on this connection, such as an
		// exclusive runtime-scoped network callback claim, until it closes, so
		// close it on any failure.
		c.client.Stop()
		c.client = nil
		c.conn = nil
		c.RPC = nil
		c.internalRPC = nil
		killErr := c.killProcess()
		if !c.isExternalServer {
			c.actualPort.Store(0)
		}
		c.state = stateError
		return errors.Join(err, killErr)
	}

	c.state = stateConnected
	return nil
}

// initializeConnection verifies the protocol version and registers the
// client's providers on a new connection.
func (c *Client) initializeConnection(ctx context.Context) error {
	// Verify protocol version compatibility
	if err := c.verifyProtocolVersion(ctx); err != nil {
		return err
	}

	if request := networkSetProviderRequest(c.options.Network); request != nil {
		if _, err := c.RPC.Network.SetProvider(ctx, request); err != nil {
			return err
		}
	}

	if c.options.ExtensionLaunchProvider != nil {
		if _, err := c.RPC.RegisterExtensionLaunchProvider(ctx); err != nil {
			return err
		}
	}

	if len(c.options.BuiltinPluginDirectories) > 0 {
		if _, err := c.client.Request(ctx, "plugins.builtin.set", map[string]any{
			"paths": c.options.BuiltinPluginDirectories,
		}); err != nil {
			return err
		}
	}

	// If a session filesystem provider was configured, register it.
	if c.options.SessionFS != nil {
		req := &rpc.SessionFSSetProviderRequest{
			InitialCwd:       c.options.SessionFS.InitialWorkingDirectory,
			SessionStatePath: c.options.SessionFS.SessionStatePath,
			Conventions:      c.options.SessionFS.Conventions,
		}
		if c.options.SessionFS.Capabilities != nil {
			sqlite := c.options.SessionFS.Capabilities.Sqlite
			binary := c.options.SessionFS.Capabilities.Binary
			req.Capabilities = &rpc.SessionFSSetProviderCapabilities{
				Sqlite: &sqlite,
				Binary: &binary,
			}
			if c.options.SessionFS.Capabilities.ReadFileRange {
				readFileRange := true
				req.Capabilities.ReadFileRange = &readFileRange
			}
		}
		_, err := c.RPC.SessionFS.SetProvider(ctx, req)
		if err != nil {
			return err
		}
	}

	// If a request handler was configured, register as the inference provider.
	if c.options.RequestHandler != nil {
		if _, err := c.RPC.LlmInference.SetProvider(ctx); err != nil {
			return err
		}
	}

	return nil
}

// Stop stops the CLI server and closes all active sessions.
//
// This method performs graceful cleanup:
//  1. Closes all active sessions (releases in-memory resources); detach requests
//     share one 10 second timeout, so an unresponsive runtime cannot hold the
//     remaining steps back
//  2. Requests runtime shutdown for SDK-owned CLI processes
//  3. Closes owned stdio input and waits up to 10 seconds for host cleanup and exit
//  4. Terminates any remaining owned CLI process and closes the JSON-RPC connection
//
// Note: session data on disk is preserved, so sessions can be resumed later.
// To permanently remove session data before stopping, call [Client.DeleteSession]
// for each session first.
//
// Returns an error that aggregates all errors encountered during cleanup.
//
// Example:
//
//	if err := client.Stop(); err != nil {
//	    log.Printf("Cleanup error: %v", err)
//	}
func (c *Client) Stop() error {
	finishStop := c.beginStop()
	defer finishStop()
	c.disconnectAhpHosts()
	var errs []error

	// Failed resumes may restore the resident's callbacks until detach finishes.
	c.sessionsMux.Lock()
	sessions := make([]*Session, 0, len(c.sessions))
	for _, session := range c.sessions {
		sessions = append(sessions, session)
	}
	c.sessionsMux.Unlock()

	// A hung runtime never answers session.detach, so every detach shares one
	// deadline; past it, Stop moves on to runtime shutdown and termination.
	detachCtx, cancelDetach := context.WithTimeout(context.Background(), runtimeShutdownTimeout)
	for _, session := range sessions {
		if err := session.disconnect(detachCtx); err != nil {
			errs = append(errs, fmt.Errorf("failed to disconnect session %s: %w", session.SessionID, err))
		}
	}
	cancelDetach()

	c.sessionsMux.Lock()
	// Fence later rollback and retire residents restored during graceful detach.
	c.sessionsGeneration++
	remainingSessions := make([]*Session, 0, len(c.sessions))
	for _, session := range c.sessions {
		remainingSessions = append(remainingSessions, session)
	}
	c.sessions = make(map[string]*Session)
	c.sessionsMux.Unlock()
	for _, session := range remainingSessions {
		session.cancelPendingExternalTools()
		session.retireEventProcessing(true)
	}
	c.cancelPendingShellAuthenticationRequests()
	c.clearGitHubTokenProviders()
	c.closeCopilotRequestAdapter()
	c.closeInstallationConfirmationAdapter()

	c.startStopMux.Lock()
	defer c.startStopMux.Unlock()

	if (c.process != nil || c.ffiHost != nil) && !c.isExternalServer && c.RPC != nil {
		rpcClient := c.RPC
		runtimeShutdownStart := time.Now()
		shutdownDone := make(chan error, 1)
		go func() {
			_, err := rpcClient.Runtime.Shutdown(context.Background())
			shutdownDone <- err
		}()

		select {
		case err := <-shutdownDone:
			if err != nil {
				c.logDebugTiming(runtimeShutdownStart, "CopilotClient.Stop runtime shutdown failed")
				errs = append(errs, fmt.Errorf("failed to gracefully shut down runtime: %w", err))
			} else {
				c.logDebugTiming(runtimeShutdownStart, "CopilotClient.Stop runtime shutdown complete")
			}
		case <-time.After(runtimeShutdownTimeout):
			c.logDebugTiming(runtimeShutdownStart, "CopilotClient.Stop runtime shutdown timed out")
			errs = append(errs, fmt.Errorf("timed out gracefully shutting down runtime after %s", runtimeShutdownTimeout))
		}
	}

	// The stdio host finalizes telemetry after EOF, not after runtime.shutdown.
	// Keep stdout open while allowing the child to finish that cleanup naturally.
	if c.process != nil && !c.isExternalServer && c.processStdin != nil {
		processExitStart := time.Now()
		if err := c.processStdin.Close(); err != nil && !errors.Is(err, os.ErrClosed) {
			errs = append(errs, fmt.Errorf("failed to close CLI stdin: %w", err))
		}
		c.processStdin = nil
		select {
		case <-c.processDone:
			c.logDebugTiming(processExitStart, "CopilotClient.Stop CLI process exited gracefully")
			c.osProcess.Store(nil)
			c.process = nil
		case <-time.After(processExitTimeout):
			c.logDebugTiming(processExitStart, "CopilotClient.Stop CLI process exit timed out; killing process")
		}
	}

	if c.process != nil && !c.isExternalServer {
		if err := c.killProcessAndWait(); err != nil {
			errs = append(errs, err)
		}
	}
	c.process = nil

	// Tear down the in-process FFI host (closes the connection and shuts down the
	// native runtime). No child process to reap in this mode.
	if c.ffiHost != nil {
		c.ffiHost.Dispose()
		c.ffiHost = nil
	}

	// Close external TCP connection if exists
	if c.isExternalServer && c.conn != nil {
		if err := c.conn.Close(); err != nil {
			errs = append(errs, fmt.Errorf("failed to close socket: %w", err))
		}
		c.conn = nil
	}

	// Then close JSON-RPC client (readLoop can now exit)
	if c.client != nil {
		c.client.Stop()
	}
	c.retireTransport()

	// Clear models cache
	c.modelsCacheMux.Lock()
	c.modelsCache = nil
	c.modelsCacheMux.Unlock()

	c.state = stateDisconnected
	if !c.isExternalServer {
		c.actualPort.Store(0)
	}

	return errors.Join(errs...)
}

// retireTransport clears the stopped transport. Callers that passed
// ensureConnected keep their snapshot, whose requests then fail with an error.
func (c *Client) retireTransport() {
	c.transportMux.Lock()
	defer c.transportMux.Unlock()
	c.client = nil
	c.RPC = nil
	c.internalRPC = nil
}

func (c *Client) beginStop() func() {
	c.transportMux.Lock()
	c.stopGeneration++
	c.stopsInProgress++
	c.transportMux.Unlock()
	return func() {
		c.transportMux.Lock()
		c.stopsInProgress--
		c.transportMux.Unlock()
	}
}

func (c *Client) logDebugTiming(start time.Time, message string) {
	switch strings.ToLower(c.options.LogLevel) {
	case "debug", "all":
		log.Printf("%s elapsed=%s", message, time.Since(start))
	}
}

// ForceStop forcefully stops the CLI server without graceful cleanup.
//
// Use this when [Client.Stop] fails or takes too long. This method:
//   - Clears all sessions immediately without destroying them
//   - Force closes the connection
//   - Kills the CLI process (if spawned by this client)
//
// Event processing observes the stop signal before dispatching each remaining
// callback. Callbacks already being dispatched are not awaited and may finish
// after this method returns; an event handler can itself call ForceStop.
// Pending response waits fail when the session is retired, without waiting for
// an idle event or their context deadline.
// Registered session callbacks are released locally, including unadmitted event
// handler entries retained by a dispatch snapshot.
//
// Example:
//
//	// If normal stop hangs, force stop
//	done := make(chan struct{})
//	go func() {
//	    client.Stop()
//	    close(done)
//	}()
//
//	select {
//	case <-done:
//	    // Stopped successfully
//	case <-time.After(5 * time.Second):
//	    client.ForceStop()
//	}
func (c *Client) ForceStop() {
	finishStop := c.beginStop()
	defer finishStop()
	c.disconnectAhpHosts()
	// Kill the process without waiting for startStopMux, which Start may hold.
	// This unblocks any I/O Start is doing (connect, version check).
	if p := c.osProcess.Swap(nil); p != nil {
		p.Kill()
	}

	// Clear sessions immediately without trying to destroy them
	c.retireSessionsLocally()

	c.startStopMux.Lock()
	defer c.startStopMux.Unlock()

	// Kill CLI process (only if we spawned it)
	// This is a fallback in case the process wasn't killed above (e.g. if Start hadn't set
	// osProcess yet), or if the process was restarted and osProcess now points to a new process.
	if c.process != nil && !c.isExternalServer {
		_ = c.killProcess() // Ignore errors since we're force stopping
	}
	c.process = nil

	// Dispose the in-process FFI host (if any) without waiting on graceful shutdown.
	if c.ffiHost != nil {
		c.ffiHost.Dispose()
		c.ffiHost = nil
	}

	// Close external TCP connection if exists
	if c.isExternalServer && c.conn != nil {
		_ = c.conn.Close() // Ignore errors
		c.conn = nil
	}

	// Close JSON-RPC client
	if c.client != nil {
		c.client.Stop()
	}
	c.retireTransport()

	// Clear models cache
	c.modelsCacheMux.Lock()
	c.modelsCache = nil
	c.modelsCacheMux.Unlock()

	c.state = stateDisconnected
	if !c.isExternalServer {
		c.actualPort.Store(0)
	}
}

// retireSessionsLocally removes every session and retires it without
// contacting the runtime: pending waits fail, and registered callbacks and
// connection-scoped handlers are released. It does not wait for event
// callbacks already being dispatched, which may themselves stop the client.
func (c *Client) retireSessionsLocally() {
	c.sessionsMux.Lock()
	c.sessionsGeneration++
	sessions := make([]*Session, 0, len(c.sessions))
	for _, session := range c.sessions {
		sessions = append(sessions, session)
	}
	c.sessions = make(map[string]*Session)
	c.sessionsMux.Unlock()
	c.cancelPendingShellAuthenticationRequests()
	for _, session := range sessions {
		session.clearSkillProvider()
		session.clearMemoryStore()
		session.cancelPendingExternalTools()
		session.stopEventProcessing()
	}
	c.clearGitHubTokenProviders()
	c.closeCopilotRequestAdapter()
	c.closeInstallationConfirmationAdapter()
}

// startupAttempt publishes the outcome of an in-progress Start to the
// operations waiting for it in ensureConnected.
type startupAttempt struct {
	done chan struct{} // closed once the outcome is published
	err  error         // the startup failure; written before done is closed
	once sync.Once
}

type transportGeneration struct {
	startup *startupAttempt
	stop    uint64
}

// finish publishes the outcome of the attempt; only the first call has effect.
func (a *startupAttempt) finish(err error) {
	a.once.Do(func() {
		a.err = err
		close(a.done)
	})
}

// ensureConnected returns the transport for the caller's requests, waiting for
// an in-progress Start and starting the client when it is disconnected. Callers
// must use the returned client rather than re-reading c.client, which a
// concurrent Stop may retire; a stopped transport fails requests with an error.
//
// transportMux, never held across I/O or while joining the JSON-RPC reader,
// orders this check with transport changes: Start writes the transport while
// startup is set and publishes it by clearing startup, and Stop and ForceStop
// retire it through retireTransport. A failed Start publishes its error before
// closing the transport and keeps startup set until it returns, so callers fail
// with that error instead of restarting the client during its cleanup.
// Stop generations fence calls that overlap teardown without preventing a
// fresh call after Stop or ForceStop has completed from starting again.
func (c *Client) ensureConnected(ctx context.Context) (*jsonrpc2.Client, error) {
	// Do not take startStopMux: the caller may be the JSON-RPC reader, which
	// Stop joins while holding that lock.
	if err := ctx.Err(); err != nil {
		return nil, err
	}
	c.transportMux.Lock()
	generation := transportGeneration{startup: c.lastStartup, stop: c.stopGeneration}
	c.transportMux.Unlock()
	started := false
	for {
		c.transportMux.Lock()
		stopped := c.stopsInProgress != 0 || c.stopGeneration != generation.stop
		startup := c.startup
		var transport *jsonrpc2.Client
		if startup == nil {
			transport = c.client
		}
		c.transportMux.Unlock()
		if stopped {
			return nil, errors.New("client is stopping or was stopped")
		}
		if transport != nil {
			select {
			case <-transport.ConnectionClosed():
				transport = nil
			default:
			}
		}
		if startup != nil {
			// Start creates the transport before its registrations complete, and
			// sessions must not be created before them.
			select {
			case <-startup.done:
				if startup.err != nil {
					return nil, fmt.Errorf("client failed to start: %w", startup.err)
				}
				continue
			case <-ctx.Done():
				return nil, ctx.Err()
			}
		}
		if transport != nil {
			if hook := c.testHookTransportAcquired; hook != nil {
				hook()
			}
			return transport, nil
		}
		if started {
			return nil, errors.New("client is not connected")
		}
		if err := c.start(ctx, &generation); err != nil {
			return nil, err
		}
		started = true
	}
}

// CreateSession creates a new conversation session with the Copilot CLI.
//
// Sessions maintain conversation state, handle events, and manage tool execution.
// If the client is not connected, this will automatically start the runtime.
//
// The config parameter is optional. If no OnPermissionRequest handler is provided,
// permission requests are surfaced as events for the caller to resolve manually.
//
// Returns the created session or an error if session creation fails.
//
// Example:
//
//	// Basic session
//	session, err := client.CreateSession(context.Background(), nil)
//
//	// Session with model and tools
//	session, err := client.CreateSession(context.Background(), &copilot.CreateSessionOptions{
//	    OnPermissionRequest: copilot.PermissionHandler.ApproveAll,
//	    Model: "gpt-5.4",
//	    Tools: []copilot.Tool{
//	        {
//	            Name:        "get_weather",
//	            Description: "Get weather for a location",
//	            Handler:     weatherHandler,
//	        },
//	    },
//	})
//
// extractTransformCallbacks separates transform callbacks from a SystemMessageConfig,
// returning a wire-safe config and a map of callbacks (nil if none).
func extractTransformCallbacks(config *SystemMessageConfig) (*SystemMessageConfig, map[string]SectionTransformFn) {
	if config == nil || config.Mode != "customize" || len(config.Sections) == 0 {
		return config, nil
	}

	callbacks := make(map[string]SectionTransformFn)
	wireSections := make(map[string]SectionOverride)
	for id, override := range config.Sections {
		if override.Transform != nil {
			callbacks[id] = override.Transform
			wireSections[id] = SectionOverride{Action: "transform"}
		} else {
			wireSections[id] = override
		}
	}

	if len(callbacks) == 0 {
		return config, nil
	}

	wireConfig := &SystemMessageConfig{
		Mode:     config.Mode,
		Content:  config.Content,
		Sections: wireSections,
	}
	return wireConfig, callbacks
}

func hasManagedSettings(enableManagedSettings *bool, managedSettings *ManagedSettings) bool {
	return (enableManagedSettings != nil && *enableManagedSettings) || managedSettings != nil
}

func validateAskUserVariant(variant AskUserVariant) error {
	if variant != "" && variant != AskUserVariantLegacy && variant != AskUserVariantElicitation {
		return fmt.Errorf("invalid AskUserVariant %q: expected %q, %q, or unset", variant, AskUserVariantLegacy, AskUserVariantElicitation)
	}
	return nil
}

func (c *Client) CreateSession(ctx context.Context, input *CreateSessionOptions) (*Session, error) {
	config := input.normalize()
	if config == nil {
		config = &sessionOptions{}
	}
	runtimeRefreshHandler := config.OnMCPAuthRuntimeRefresh
	permissionEnvelopeHandler := config.OnPermissionRequestWithEnvelope
	recovery := config.AutoModeRecovery
	if recovery && config.OnAutoModeSwitchRequest == nil {
		return nil, fmt.Errorf("AutoModeRecovery requires OnAutoModeSwitchRequest")
	}
	if config.Cloud != nil && config.SkillProvider != nil {
		return nil, cloudSkillProviderError{}
	}
	storeMemory, err := requestedMemoryStore(config.Memory, config.StoreMemory)
	if err != nil {
		return nil, err
	}
	if config.Cloud != nil && storeMemory != nil {
		return nil, cloudMemoryStoreError{}
	}
	if config.GitHubToken != "" && config.GitHubTokenProvider != nil {
		return nil, fmt.Errorf("GitHubToken and GitHubTokenProvider cannot be used together")
	}
	if err := validateAskUserVariant(config.AskUserVariant); err != nil {
		return nil, err
	}
	if trust := config.TrustWorkingDirectory; trust != "" && trust != WorkingDirectoryTrustSession && trust != WorkingDirectoryTrustUser {
		return nil, fmt.Errorf("invalid TrustWorkingDirectory %q: expected %q, %q, or unset", trust, WorkingDirectoryTrustSession, WorkingDirectoryTrustUser)
	}

	transport, err := c.ensureConnected(ctx)
	if err != nil {
		return nil, err
	}

	c.applyConfigDefaultsForMode(config)

	registrationID := c.registerGitHubTokenProvider(config.GitHubTokenProvider)
	registrationTransferred := false
	defer func() {
		if !registrationTransferred {
			c.unregisterGitHubTokenProvider(registrationID)
		}
	}()

	req := createSessionRequest{}
	req.Model = config.Model
	req.ClientName = config.ClientName
	req.ReasoningEffort = config.ReasoningEffort
	req.AllowMultipleToolCalls = config.AllowMultipleToolCalls
	req.ReasoningSummary = config.ReasoningSummary
	req.ContextTier = config.ContextTier
	req.ConfigDir = config.ConfigDirectory
	req.EnableConfigDiscovery = config.EnableConfigDiscovery
	req.TrustWorkingDirectory = string(config.TrustWorkingDirectory)
	req.SkipEmbeddingRetrieval = config.SkipEmbeddingRetrieval
	req.EmbeddingCacheStorage = config.EmbeddingCacheStorage
	req.OrganizationCustomInstructions = config.OrganizationCustomInstructions
	req.EnableOnDemandInstructionDiscovery = config.EnableOnDemandInstructionDiscovery
	req.EnableFileHooks = config.EnableFileHooks
	req.EnableHostGitOperations = config.EnableHostGitOperations
	req.EnableSessionStore = config.EnableSessionStore
	req.EnableSkills = config.EnableSkills
	if config.SkillProvider != nil {
		req.HasSkillProvider = Bool(true)
	}
	if storeMemory != nil {
		req.HasMemoryStore = Bool(true)
	}
	if config.Tools != nil {
		req.Tools = toolDefinitionsForWire(config.Tools)
	}
	systemMessage := c.systemMessageForMode(config.SystemMessage)
	wireSystemMessage, transformCallbacks := extractTransformCallbacks(systemMessage)
	req.SystemMessage = wireSystemMessage
	availableTools, excludedTools, ferr := c.resolveToolFilterOptions(config.AvailableTools, config.ExcludedTools)
	if ferr != nil {
		return nil, ferr
	}
	req.AvailableTools = availableTools
	req.ExcludedTools = excludedTools
	req.ExcludedBuiltInAgents = config.ExcludedBuiltInAgents
	req.IncludedBuiltInAgents = config.IncludedBuiltInAgents
	req.DefaultProvider = config.DefaultProvider
	req.Capi = config.Capi
	req.Providers = config.Providers
	req.Models = config.Models
	req.AskUserVariant = config.AskUserVariant
	req.EnableSessionTelemetry = config.EnableSessionTelemetry
	req.EnableCitations = config.EnableCitations
	req.EnableFileChangeTracking = config.EnableFileChangeTracking
	req.SessionLimits = config.SessionLimits
	req.Autopilot = config.Autopilot
	req.IsExperimentalMode = config.EnableExperimentalMode
	req.SkipCustomInstructions = config.SkipCustomInstructions
	req.RefreshCustomInstructions = config.RefreshCustomInstructions
	req.CustomAgentsLocalOnly = config.CustomAgentsLocalOnly
	req.CoauthorEnabled = config.CoauthorEnabled
	req.ManageScheduleEnabled = config.ManageScheduleEnabled
	req.ModelCapabilities = config.ModelCapabilities
	req.WorkingDirectory = config.WorkingDirectory
	req.AdditionalDirectories = config.AdditionalDirectories
	if config.MCPServers != nil {
		req.MCPServers = &config.MCPServers
	}
	req.Diagnostics = config.Diagnostics
	req.MCPOAuthTokenStorage = config.MCPOAuthTokenStorage
	req.AuthClientIDMetadataURL = config.AuthClientIDMetadataURL
	req.EnvValueMode = "direct"
	req.CustomAgents = config.CustomAgents
	req.DefaultAgent = config.DefaultAgent
	req.Agent = config.Agent
	req.SkillDirectories = config.SkillDirectories
	req.AgentDirectories = config.AgentDirectories
	req.HookDirectories = config.HookDirectories
	req.PluginDirectories = config.PluginDirectories
	req.InstructionDirectories = config.InstructionDirectories
	req.DisabledSkills = config.DisabledSkills
	if config.DisabledMCPServers != nil {
		req.DisabledMCPServers = &config.DisabledMCPServers
	}
	req.SandboxConfig = config.SandboxConfig
	req.InfiniteSessions = config.InfiniteSessions
	req.LargeOutput = config.LargeOutput
	req.ToolSearch = config.ToolSearch
	req.ImageGeneration = config.ImageGeneration
	req.Memory = config.Memory
	req.GitHubToken = config.GitHubToken
	req.GitHubTokenProviderRegistrationID = registrationID
	req.Cloud = config.Cloud
	req.Canvases = config.Canvases
	req.ExtensionInfo = config.ExtensionInfo
	req.CanvasProvider = config.CanvasProvider
	req.RequestCanvasRenderer = config.RequestCanvasRenderer
	req.RequestExtensions = config.RequestExtensions
	req.ExtensionSDKPath = config.ExtensionSDKPath
	req.ExtensionInfo = config.ExtensionInfo
	req.ExpAssignments = config.ExpAssignments
	if config.FeatureFlags != nil {
		req.FeatureFlags = &config.FeatureFlags
	}
	req.EnableManagedSettings = config.EnableManagedSettings
	req.ExcludedManagedSettings = config.ExcludedManagedSettings
	req.EnforceManagedModelDefaults = config.EnforceManagedModelDefaults
	req.InteractiveUser = config.InteractiveUser
	req.PermissionMode = config.PermissionMode
	req.ManagedSettings = config.ManagedSettings

	if config.Commands != nil {
		cmds := make([]wireCommand, 0, len(config.Commands))
		for _, cmd := range config.Commands {
			cmds = append(cmds, wireCommand{Name: cmd.Name, Description: cmd.Description})
		}
		req.Commands = cmds
	}
	if config.OnElicitationRequest != nil || config.OnAttributedElicitationRequest != nil {
		req.RequestElicitation = Bool(true)
	}
	if config.OnExitPlanModeRequest != nil || config.OnAttributedExitPlanModeRequest != nil {
		req.RequestExitPlanMode = Bool(true)
	}
	if config.OnAutoModeSwitchRequest != nil || recovery {
		req.RequestAutoModeSwitch = Bool(true)
	}
	if recovery {
		req.RequestAutoModeRecovery = Bool(true)
	}
	if config.EnableMCPApps {
		req.RequestMCPApps = Bool(true)
	}
	req.GitHubMCPToolConfig = config.GitHubMCPToolConfig

	if config.Streaming != nil {
		req.Streaming = config.Streaming
	}
	if config.IncludeSubAgentStreamingEvents != nil {
		req.IncludeSubAgentStreamingEvents = config.IncludeSubAgentStreamingEvents
	} else {
		req.IncludeSubAgentStreamingEvents = Bool(true)
	}
	if c.options.OnGitHubTelemetry != nil {
		req.EnableGitHubTelemetryForwarding = Bool(true)
	}
	if config.OnUserInputRequest != nil || config.OnAttributedUserInputRequest != nil {
		req.RequestUserInput = Bool(true)
	}
	if config.OnShellAuthenticationRequest != nil {
		req.RequestShellAuthentication = Bool(true)
	}
	if config.Hooks != nil && (config.Hooks.OnPreToolUse != nil ||
		config.Hooks.OnPreMCPToolCall != nil ||
		config.Hooks.OnPostToolUse != nil ||
		config.Hooks.OnPostToolUseFailure != nil ||
		config.Hooks.OnUserPromptSubmitted != nil ||
		config.Hooks.OnUserPromptTransformed != nil ||
		config.Hooks.OnSessionStart != nil ||
		config.Hooks.OnSessionEnd != nil ||
		config.Hooks.OnErrorOccurred != nil ||
		config.Hooks.OnAgentStop != nil ||
		config.Hooks.OnSubagentStart != nil ||
		config.Hooks.OnSubagentStop != nil) {
		req.Hooks = Bool(true)
	}
	if config.OnPermissionRequest != nil || config.OnPermissionRequestWithPrompt != nil || permissionEnvelopeHandler != nil {
		req.RequestPermission = Bool(true)
	}

	traceparent, tracestate := getTraceContext(ctx)
	req.Traceparent = traceparent
	req.Tracestate = tracestate

	// For cloud sessions, let the CLI/server assign the session id and
	// register the session lazily once the response arrives. For non-cloud
	// sessions we generate the id client-side (when the caller didn't
	// supply one) so the session can be registered BEFORE the RPC — the
	// CLI may issue session-scoped requests (e.g. sessionFs.writeFile for
	// workspace metadata) during session.create processing, before it has
	// sent the response.
	useServerGeneratedID := config.Cloud != nil && config.SessionID == ""
	var localSessionID string
	if useServerGeneratedID {
		localSessionID = ""
	} else if config.SessionID != "" {
		localSessionID = config.SessionID
	} else {
		localSessionID = uuid.NewString()
	}
	req.SessionID = localSessionID
	req.Name = config.Name
	if localSessionID != "" {
		// Keep provisional ownership through user setup, capability checks, and rejection cleanup.
		unlockSession, err := c.lockSessionOperation(ctx, localSessionID)
		if err != nil {
			return nil, err
		}
		defer unlockSession()
	}
	if err := c.waitForAutoModeRecoveryCleanup(ctx, localSessionID); err != nil {
		return nil, err
	}

	// unregisterSession removes only the session created by this call and stops
	// its event consumer. The latter is essential on CreateSession error paths:
	// newSession starts processEvents eagerly, and no caller receives the failed
	// session to disconnect it.
	unregisterSession := func(sessionID string, s *Session) {
		c.sessionsMux.Lock()
		if c.sessions[sessionID] == s {
			delete(c.sessions, sessionID)
		}
		c.sessionsMux.Unlock()
		s.cancelPendingExternalTools()
		s.stopEventProcessing()
	}

	// initializeSession creates the session, wires up handlers, and registers
	// it in the sessions map. Invoked from the read loop the instant the
	// session.create response arrives (synchronously, before the next
	// message is dispatched) so notifications for the new session id are
	// routed to a registered session.
	initializeSession := func(sessionID string) (*Session, error) {
		s := newSession(
			sessionID,
			transport,
			"",
			hasManagedSettings(config.EnableManagedSettings, config.ManagedSettings),
		)

		s.registerTools(config.Tools)
		s.registerPermissionHandler(config.OnPermissionRequest)
		s.registerPermissionHandlerWithPrompt(config.OnPermissionRequestWithPrompt)
		s.registerPermissionHandlerWithEnvelope(permissionEnvelopeHandler)
		s.registerMCPAuthHandlers(config.OnMCPAuthRequest, runtimeRefreshHandler)
		if handler := userInputHandlerFor(config.OnUserInputRequest, config.OnAttributedUserInputRequest); handler != nil {
			s.registerAttributedUserInputHandler(handler)
		}
		if config.OnShellAuthenticationRequest != nil {
			s.registerShellAuthenticationHandler(config.OnShellAuthenticationRequest)
		}
		if config.Hooks != nil {
			s.registerHooks(config.Hooks)
		}
		if transformCallbacks != nil {
			s.registerTransformCallbacks(transformCallbacks)
		}
		if config.OnEvent != nil {
			s.On(config.OnEvent)
		}
		if config.OnEventWithPermissionEnvelope != nil {
			s.OnWithPermissionEnvelope(config.OnEventWithPermissionEnvelope)
		}
		if len(config.Commands) > 0 {
			s.registerCommands(config.Commands)
		}
		if handler := elicitationHandlerFor(config.OnElicitationRequest, config.OnAttributedElicitationRequest); handler != nil {
			s.registerAttributedElicitationHandler(handler)
		}
		if handler := exitPlanModeHandlerFor(config.OnExitPlanModeRequest, config.OnAttributedExitPlanModeRequest); handler != nil {
			s.registerAttributedExitPlanModeHandler(handler)
		}
		if config.OnAutoModeSwitchRequest != nil {
			s.registerAutoModeSwitchHandler(config.OnAutoModeSwitchRequest, recovery)
		}
		if config.CanvasHandler != nil {
			s.registerCanvasHandler(config.CanvasHandler)
		}
		if config.SkillProvider != nil {
			s.registerSkillProvider(config.SkillProvider)
		}
		if storeMemory != nil {
			s.registerMemoryStore(storeMemory)
		}
		if bearerTokenProviders := collectBearerTokenProviders(config.Providers); bearerTokenProviders != nil {
			s.registerBearerTokenProviders(bearerTokenProviders)
		}

		if c.options.SessionFS != nil {
			if config.CreateSessionFSProvider == nil {
				unregisterSession(sessionID, s)
				return nil, fmt.Errorf("CreateSessionFSProvider is required in session config when SessionFS is enabled in client options")
			}
			provider := c.createSessionFSProvider(s, config.CreateSessionFSProvider)
			if c.options.SessionFS.Capabilities != nil && c.options.SessionFS.Capabilities.ReadFileRange {
				if _, ok := provider.(SessionFSReadFileRangeProvider); !ok {
					unregisterSession(sessionID, s)
					return nil, fmt.Errorf("SessionFS capabilities declare byte-range reads but the provider does not implement SessionFSReadFileRangeProvider")
				}
			}
			if c.options.SessionFS.Capabilities != nil && c.options.SessionFS.Capabilities.Sqlite {
				if _, ok := provider.(SessionFSSqliteProvider); !ok {
					unregisterSession(sessionID, s)
					return nil, fmt.Errorf("SessionFS capabilities declare SQLite support but the provider does not implement SessionFSSqliteProvider")
				}
			}
			if c.options.SessionFS.Capabilities != nil && c.options.SessionFS.Capabilities.Binary {
				if _, ok := provider.(SessionFSBinaryProvider); !ok {
					unregisterSession(sessionID, s)
					return nil, fmt.Errorf("SessionFS capabilities declare binary support but the provider does not implement SessionFSBinaryProvider")
				}
			}
			s.clientSessionAPIs.SessionFS = c.sessionFSAdapter(sessionID, provider)
		}

		s.releaseFromClient = func() { c.forgetSession(s) }
		c.sessionsMux.Lock()
		if c.autoModeRecoveryCleanups[transport][sessionID] != nil {
			c.sessionsMux.Unlock()
			s.stopEventProcessing()
			return nil, fmt.Errorf("session %s is still detaching after rejected AutoModeRecovery enrollment", sessionID)
		}
		c.sessions[sessionID] = s
		c.sessionsMux.Unlock()
		return s, nil
	}

	var session *Session
	var registeredSessionID string
	var orphanedSessionID string
	var inlineMu sync.Mutex
	var requestFinished bool
	var unlockAssignedSession func()
	defer func() {
		inlineMu.Lock()
		unlock := unlockAssignedSession
		inlineMu.Unlock()
		if unlock != nil {
			unlock()
		}
	}()
	deleteOrphan := func(sessionID string) error {
		cleanupCtx, cancelCleanup := context.WithTimeout(context.WithoutCancel(ctx), 10*time.Second)
		defer cancelCleanup()
		return requestSessionDeletion(cleanupCtx, transport, sessionID)
	}

	// Pre-register non-cloud sessions BEFORE issuing the RPC so any
	// session-scoped requests the CLI emits during session.create processing
	// (e.g. sessionFs.writeFile for workspace metadata) can be routed to the
	// correct handlers.
	if localSessionID != "" {
		s, err := initializeSession(localSessionID)
		if err != nil {
			return nil, err
		}
		session = s
		registeredSessionID = localSessionID
	}

	// For the server-assigned (cloud) path, register the session
	// synchronously from the read loop the instant the response arrives,
	// before the read loop dispatches the next message. Without this hook
	// the awaiter goroutine may not run until after the read loop has
	// dispatched the first session.event notification, which would be
	// silently dropped because the session id isn't yet in the lookup
	// table. Non-cloud sessions are already registered above.
	var inlineCb func(raw json.RawMessage) error
	if session == nil {
		inlineCb = func(raw json.RawMessage) error {
			var early struct {
				SessionID string `json:"sessionId"`
			}
			if err := json.Unmarshal(raw, &early); err != nil {
				return fmt.Errorf("failed to parse sessionId from response: %w", err)
			}
			if early.SessionID == "" {
				return fmt.Errorf("session.create response did not include a sessionId")
			}
			// The read loop cannot wait for a permit held by an RPC awaiting this loop.
			unlock, err := c.acquireSessionOperation(context.WithoutCancel(ctx), early.SessionID, false)
			if err != nil {
				return err
			}
			s, err := initializeSession(early.SessionID)
			inlineMu.Lock()
			if err != nil {
				if requestFinished {
					// Cleanup must not wait for an RPC response on the read loop.
					go func() {
						defer unlock()
						if cleanupErr := deleteOrphan(early.SessionID); cleanupErr != nil {
							log.Printf("failed to delete uninitialized cloud session %s: %v", early.SessionID, cleanupErr)
						}
					}()
				} else {
					orphanedSessionID = early.SessionID
					unlockAssignedSession = unlock
				}
				inlineMu.Unlock()
				return err
			}
			if requestFinished {
				inlineMu.Unlock()
				unregisterSession(early.SessionID, s)
				unlock()
				return nil
			}
			unlockAssignedSession = unlock
			session = s
			registeredSessionID = early.SessionID
			inlineMu.Unlock()
			return nil
		}
	}

	result, err := transport.RequestWithInlineResponse(ctx, "session.create", req, inlineCb)
	inlineMu.Lock()
	requestFinished = true
	cleanupSessionID := orphanedSessionID
	sessionToUnregister := session
	registeredID := registeredSessionID
	inlineMu.Unlock()
	if err != nil {
		if registeredID != "" {
			unregisterSession(registeredID, sessionToUnregister)
		}
		if cleanupSessionID != "" {
			if cleanupErr := deleteOrphan(cleanupSessionID); cleanupErr != nil {
				err = errors.Join(err, fmt.Errorf("failed to delete uninitialized cloud session: %w", cleanupErr))
			}
		}
		return nil, fmt.Errorf("failed to create session: %w", err)
	}

	var response createSessionResponse
	if err := json.Unmarshal(result, &response); err != nil {
		if registeredSessionID != "" {
			unregisterSession(registeredSessionID, session)
		}
		return nil, fmt.Errorf("failed to unmarshal response: %w", err)
	}

	if session == nil {
		return nil, fmt.Errorf("session.create response did not include a sessionId")
	}

	if localSessionID != "" && response.SessionID != "" && response.SessionID != localSessionID {
		unregisterSession(registeredSessionID, session)
		return nil, fmt.Errorf("session.create returned sessionId %s but the caller requested %s", response.SessionID, localSessionID)
	}
	session.setCapabilities(response.Capabilities)
	failSetup := func(err error) (*Session, error) {
		if useServerGeneratedID && recovery {
			return nil, c.cleanupFailedSession(ctx, session, err, true, true)
		}
		unregisterSession(registeredSessionID, session)
		return nil, err
	}
	if recovery {
		if registrationID != "" {
			session.setGitHubTokenProviderRegistrationRelease(func() {
				c.unregisterGitHubTokenProvider(registrationID)
			})
			registrationTransferred = true
		}
		if err := c.requireAutoModeRecovery(ctx, session, true, useServerGeneratedID); err != nil {
			return nil, err
		}
	}
	if config.OnMCPAuthRequest != nil || runtimeRefreshHandler != nil {
		if _, err := transport.Request(ctx, "session.eventLog.registerInterest", map[string]any{
			"sessionId": session.SessionID,
			"eventType": "mcp.oauth_required",
		}); err != nil {
			return failSetup(err)
		}
	}

	session.workspacePath = response.WorkspacePath
	session.permissionModeResult = response.PermissionModeResult

	if err := c.updateSessionOptionsForMode(ctx, session, optBackInFields{
		SkipCustomInstructions: config.SkipCustomInstructions,
		CustomAgentsLocalOnly:  config.CustomAgentsLocalOnly,
		CoauthorEnabled:        config.CoauthorEnabled,
		ManageScheduleEnabled:  config.ManageScheduleEnabled,
		IncludedBuiltinSkills:  config.IncludedBuiltinSkills,
	}, useServerGeneratedID && recovery); err != nil {
		return failSetup(err)
	}

	if err := c.captureAhpSession(session, req); err != nil {
		return failSetup(err)
	}
	if registrationID != "" {
		session.setGitHubTokenProviderRegistrationRelease(func() {
			c.unregisterGitHubTokenProvider(registrationID)
		})
		registrationTransferred = true
	}
	return session, nil
}

// ResumeSession resumes an existing conversation with its history and configuration.
func (c *Client) ResumeSession(ctx context.Context, sessionID string, input *ResumeSessionOptions) (*Session, error) {
	config := input.normalize()
	runtimeRefreshHandler := config.OnMCPAuthRuntimeRefresh
	permissionEnvelopeHandler := config.OnPermissionRequestWithEnvelope
	recovery := config.AutoModeRecovery
	if recovery && config.OnAutoModeSwitchRequest == nil {
		return nil, fmt.Errorf("AutoModeRecovery requires OnAutoModeSwitchRequest")
	}
	requestedSessionID := sessionID
	operationSessionID := requestedSessionID
	unlockSession, err := c.lockSessionOperation(ctx, operationSessionID)
	if err != nil {
		return nil, err
	}
	defer func() {
		if unlockSession != nil {
			unlockSession()
		}
	}()

	if err := c.waitForAutoModeRecoveryCleanup(ctx, operationSessionID); err != nil {
		return nil, err
	}
	if config.GitHubToken != "" && config.GitHubTokenProvider != nil {
		return nil, fmt.Errorf("GitHubToken and GitHubTokenProvider cannot be used together")
	}
	if err := validateAskUserVariant(config.AskUserVariant); err != nil {
		return nil, err
	}
	storeMemory, err := requestedMemoryStore(config.Memory, config.StoreMemory)
	if err != nil {
		return nil, err
	}

	transport, err := c.ensureConnected(ctx)
	if err != nil {
		return nil, err
	}

	resolveAliases := recovery && c.options.SessionFS == nil
	if resolveAliases && isAbbreviatedUUIDPrefix(requestedSessionID) {
		// Resolve persisted uniqueness before replacing callbacks, and pin the
		// full ID so resume cannot resolve the prefix to another session later.
		lookup := &rpc.SessionsFindByPrefixRequest{Prefix: requestedSessionID}
		if config.ConfigDirectory != "" {
			lookup.ConfigDir = &config.ConfigDirectory
		}
		resolved, err := rpc.NewServerRPC(transport).Sessions.FindByPrefix(ctx, lookup)
		if err != nil {
			return nil, fmt.Errorf("failed to resolve session prefix: %w", err)
		}
		if resolved.SessionID == nil || *resolved.SessionID == "" {
			return nil, fmt.Errorf("no unique session matches prefix %q", requestedSessionID)
		}
		sessionID = *resolved.SessionID
	} else if resolveAliases {
		sessionID = c.residentSessionID(sessionID)
	}

	c.applyResumeDefaultsForMode(config)

	registrationID := c.registerGitHubTokenProvider(config.GitHubTokenProvider)
	registrationTransferred := false
	defer func() {
		if !registrationTransferred {
			c.unregisterGitHubTokenProvider(registrationID)
		}
	}()

	var req resumeSessionRequest
	req.SessionID = requestedSessionID
	if resolveAliases && isAbbreviatedUUIDPrefix(requestedSessionID) {
		req.SessionID = sessionID
	}
	req.ClientName = config.ClientName
	req.Model = config.Model
	req.ReasoningEffort = config.ReasoningEffort
	req.AllowMultipleToolCalls = config.AllowMultipleToolCalls
	req.ReasoningSummary = config.ReasoningSummary
	req.ContextTier = config.ContextTier
	systemMessage := c.systemMessageForMode(config.SystemMessage)
	wireSystemMessage, transformCallbacks := extractTransformCallbacks(systemMessage)
	req.SystemMessage = wireSystemMessage
	if config.Tools != nil {
		req.Tools = toolDefinitionsForWire(config.Tools)
	}
	req.DefaultProvider = config.DefaultProvider
	req.Capi = config.Capi
	req.Providers = config.Providers
	req.Models = config.Models
	req.AskUserVariant = config.AskUserVariant
	req.EnableSessionTelemetry = config.EnableSessionTelemetry
	req.IsExperimentalMode = config.EnableExperimentalMode
	req.SkipCustomInstructions = config.SkipCustomInstructions
	req.CustomAgentsLocalOnly = config.CustomAgentsLocalOnly
	req.CoauthorEnabled = config.CoauthorEnabled
	req.ManageScheduleEnabled = config.ManageScheduleEnabled
	req.ModelCapabilities = config.ModelCapabilities
	availableTools, excludedTools, ferr := c.resolveToolFilterOptions(config.AvailableTools, config.ExcludedTools)
	if ferr != nil {
		return nil, ferr
	}
	req.AvailableTools = availableTools
	req.ExcludedTools = excludedTools
	req.ExcludedBuiltInAgents = config.ExcludedBuiltInAgents
	req.IncludedBuiltInAgents = config.IncludedBuiltInAgents
	req.EnableCitations = config.EnableCitations
	req.EnableFileChangeTracking = config.EnableFileChangeTracking
	req.SessionLimits = config.SessionLimits
	req.Autopilot = config.Autopilot
	if config.Streaming != nil {
		req.Streaming = config.Streaming
	}
	if config.IncludeSubAgentStreamingEvents != nil {
		req.IncludeSubAgentStreamingEvents = config.IncludeSubAgentStreamingEvents
	} else {
		req.IncludeSubAgentStreamingEvents = Bool(true)
	}
	if c.options.OnGitHubTelemetry != nil {
		req.EnableGitHubTelemetryForwarding = Bool(true)
	}
	if config.OnUserInputRequest != nil || config.OnAttributedUserInputRequest != nil {
		req.RequestUserInput = Bool(true)
	}
	if config.OnShellAuthenticationRequest != nil {
		req.RequestShellAuthentication = Bool(true)
	}
	if config.Hooks != nil && (config.Hooks.OnPreToolUse != nil ||
		config.Hooks.OnPreMCPToolCall != nil ||
		config.Hooks.OnPostToolUse != nil ||
		config.Hooks.OnPostToolUseFailure != nil ||
		config.Hooks.OnUserPromptSubmitted != nil ||
		config.Hooks.OnUserPromptTransformed != nil ||
		config.Hooks.OnSessionStart != nil ||
		config.Hooks.OnSessionEnd != nil ||
		config.Hooks.OnErrorOccurred != nil ||
		config.Hooks.OnAgentStop != nil ||
		config.Hooks.OnSubagentStart != nil ||
		config.Hooks.OnSubagentStop != nil) {
		req.Hooks = Bool(true)
	}
	req.WorkingDirectory = config.WorkingDirectory
	req.AdditionalDirectories = config.AdditionalDirectories
	req.ConfigDir = config.ConfigDirectory
	req.EnableConfigDiscovery = config.EnableConfigDiscovery
	req.SkipEmbeddingRetrieval = config.SkipEmbeddingRetrieval
	req.EmbeddingCacheStorage = config.EmbeddingCacheStorage
	req.OrganizationCustomInstructions = config.OrganizationCustomInstructions
	req.EnableOnDemandInstructionDiscovery = config.EnableOnDemandInstructionDiscovery
	req.EnableFileHooks = config.EnableFileHooks
	req.EnableHostGitOperations = config.EnableHostGitOperations
	req.EnableSessionStore = config.EnableSessionStore
	req.EnableSkills = config.EnableSkills
	if config.SkillProvider != nil {
		req.HasSkillProvider = Bool(true)
	}
	if storeMemory != nil {
		req.HasMemoryStore = Bool(true)
	}
	if config.SuppressResumeEvent {
		req.DisableResume = Bool(true)
	}
	req.ContinuePendingWork = config.ContinuePendingWork
	req.AllowTranscriptRecovery = config.AllowTranscriptRecovery
	if config.MCPServers != nil {
		req.MCPServers = &config.MCPServers
	}
	req.Diagnostics = config.Diagnostics
	req.MCPOAuthTokenStorage = config.MCPOAuthTokenStorage
	req.AuthClientIDMetadataURL = config.AuthClientIDMetadataURL
	req.EnvValueMode = "direct"
	req.CustomAgents = config.CustomAgents
	req.DefaultAgent = config.DefaultAgent
	req.Agent = config.Agent
	req.SkillDirectories = config.SkillDirectories
	req.AgentDirectories = config.AgentDirectories
	req.HookDirectories = config.HookDirectories
	req.PluginDirectories = config.PluginDirectories
	req.InstructionDirectories = config.InstructionDirectories
	req.DisabledSkills = config.DisabledSkills
	if config.DisabledMCPServers != nil {
		req.DisabledMCPServers = &config.DisabledMCPServers
	}
	req.SandboxConfig = config.SandboxConfig
	req.InfiniteSessions = config.InfiniteSessions
	req.LargeOutput = config.LargeOutput
	req.ToolSearch = config.ToolSearch
	req.ImageGeneration = config.ImageGeneration
	req.Memory = config.Memory
	req.GitHubToken = config.GitHubToken
	req.GitHubTokenProviderRegistrationID = registrationID
	req.Canvases = config.Canvases
	req.OpenCanvases = config.OpenCanvases
	req.ExtensionInfo = config.ExtensionInfo
	req.CanvasProvider = config.CanvasProvider
	req.RequestCanvasRenderer = config.RequestCanvasRenderer
	req.RequestExtensions = config.RequestExtensions
	req.ExtensionSDKPath = config.ExtensionSDKPath
	req.ExtensionInfo = config.ExtensionInfo
	req.ExpAssignments = config.ExpAssignments
	if config.FeatureFlags != nil {
		req.FeatureFlags = &config.FeatureFlags
	}
	req.EnableManagedSettings = config.EnableManagedSettings
	req.ExcludedManagedSettings = config.ExcludedManagedSettings
	req.EnforceManagedModelDefaults = config.EnforceManagedModelDefaults
	req.InteractiveUser = config.InteractiveUser
	req.PermissionMode = config.PermissionMode
	req.ManagedSettings = config.ManagedSettings
	if config.OnPermissionRequest != nil || config.OnPermissionRequestWithPrompt != nil || permissionEnvelopeHandler != nil {
		req.RequestPermission = Bool(true)
	}

	if config.Commands != nil {
		cmds := make([]wireCommand, 0, len(config.Commands))
		for _, cmd := range config.Commands {
			cmds = append(cmds, wireCommand{Name: cmd.Name, Description: cmd.Description})
		}
		req.Commands = cmds
	}
	if config.OnElicitationRequest != nil || config.OnAttributedElicitationRequest != nil {
		req.RequestElicitation = Bool(true)
	}
	if config.OnExitPlanModeRequest != nil || config.OnAttributedExitPlanModeRequest != nil {
		req.RequestExitPlanMode = Bool(true)
	}
	if config.OnAutoModeSwitchRequest != nil || recovery {
		req.RequestAutoModeSwitch = Bool(true)
	}
	if recovery {
		req.RequestAutoModeRecovery = Bool(true)
	}
	if config.EnableMCPApps {
		req.RequestMCPApps = Bool(true)
	}
	req.GitHubMCPToolConfig = config.GitHubMCPToolConfig

	traceparent, tracestate := getTraceContext(ctx)
	req.Traceparent = traceparent
	req.Tracestate = tracestate

	// Create and register the session before issuing the RPC so that
	// events emitted by the CLI (e.g. session.start) are not dropped.
	session := newSession(
		sessionID,
		transport,
		"",
		hasManagedSettings(config.EnableManagedSettings, config.ManagedSettings),
	)

	session.registerTools(config.Tools)
	session.registerPermissionHandler(config.OnPermissionRequest)
	session.registerPermissionHandlerWithPrompt(config.OnPermissionRequestWithPrompt)
	session.registerPermissionHandlerWithEnvelope(permissionEnvelopeHandler)
	session.registerMCPAuthHandlers(config.OnMCPAuthRequest, runtimeRefreshHandler)
	if handler := userInputHandlerFor(config.OnUserInputRequest, config.OnAttributedUserInputRequest); handler != nil {
		session.registerAttributedUserInputHandler(handler)
	}
	if config.OnShellAuthenticationRequest != nil {
		session.registerShellAuthenticationHandler(config.OnShellAuthenticationRequest)
	}
	if config.Hooks != nil {
		session.registerHooks(config.Hooks)
	}
	if transformCallbacks != nil {
		session.registerTransformCallbacks(transformCallbacks)
	}
	if config.OnEvent != nil {
		session.On(config.OnEvent)
	}
	if config.OnEventWithPermissionEnvelope != nil {
		session.OnWithPermissionEnvelope(config.OnEventWithPermissionEnvelope)
	}
	if len(config.Commands) > 0 {
		session.registerCommands(config.Commands)
	}
	if handler := elicitationHandlerFor(config.OnElicitationRequest, config.OnAttributedElicitationRequest); handler != nil {
		session.registerAttributedElicitationHandler(handler)
	}
	if handler := exitPlanModeHandlerFor(config.OnExitPlanModeRequest, config.OnAttributedExitPlanModeRequest); handler != nil {
		session.registerAttributedExitPlanModeHandler(handler)
	}
	if config.OnAutoModeSwitchRequest != nil {
		session.registerAutoModeSwitchHandler(config.OnAutoModeSwitchRequest, recovery)
	}
	if config.CanvasHandler != nil {
		session.registerCanvasHandler(config.CanvasHandler)
	}
	if config.SkillProvider != nil {
		session.registerSkillProvider(config.SkillProvider)
	}
	if storeMemory != nil {
		session.registerMemoryStore(storeMemory)
	}
	if bearerTokenProviders := collectBearerTokenProviders(config.Providers); bearerTokenProviders != nil {
		session.registerBearerTokenProviders(bearerTokenProviders)
	}

	if c.options.SessionFS != nil {
		if config.CreateSessionFSProvider == nil {
			session.stopEventProcessing()
			return nil, fmt.Errorf("CreateSessionFSProvider is required in session config when SessionFS is enabled in client options")
		}
		provider := c.createSessionFSProvider(session, config.CreateSessionFSProvider)
		if c.options.SessionFS.Capabilities != nil && c.options.SessionFS.Capabilities.ReadFileRange {
			if _, ok := provider.(SessionFSReadFileRangeProvider); !ok {
				session.stopEventProcessing()
				return nil, fmt.Errorf("SessionFS capabilities declare byte-range reads but the provider does not implement SessionFSReadFileRangeProvider")
			}
		}
		if c.options.SessionFS.Capabilities != nil && c.options.SessionFS.Capabilities.Sqlite {
			if _, ok := provider.(SessionFSSqliteProvider); !ok {
				session.stopEventProcessing()
				return nil, fmt.Errorf("SessionFS capabilities declare SQLite support but the provider does not implement SessionFSSqliteProvider")
			}
		}
		if c.options.SessionFS.Capabilities != nil && c.options.SessionFS.Capabilities.Binary {
			if _, ok := provider.(SessionFSBinaryProvider); !ok {
				session.stopEventProcessing()
				return nil, fmt.Errorf("SessionFS capabilities declare binary support but the provider does not implement SessionFSBinaryProvider")
			}
		}
		session.clientSessionAPIs.SessionFS = c.sessionFSAdapter(sessionID, provider)
	}

	// Publish only fully initialized handlers: the runtime may still be issuing
	// SessionFS callbacks for the previous session while its replacement is built.
	session.releaseFromClient = func() { c.forgetSession(session) }
	c.sessionsMux.Lock()
	if c.autoModeRecoveryCleanups[transport][sessionID] != nil {
		c.sessionsMux.Unlock()
		session.stopEventProcessing()
		return nil, fmt.Errorf("session %s is still detaching after rejected AutoModeRecovery enrollment", sessionID)
	}
	replacedSession := c.sessions[sessionID]
	sessionsGeneration := c.sessionsGeneration
	c.sessions[sessionID] = session
	c.sessionsMux.Unlock()

	restoreReplacedSession := func() {
		c.sessionsMux.Lock()
		retired := c.sessionsGeneration != sessionsGeneration
		restored := false
		if current := c.sessions[sessionID]; !retired && (current == nil || current == session) {
			if replacedSession != nil && !replacedSession.detached.Load() {
				c.sessions[sessionID] = replacedSession
				restored = true
			} else {
				delete(c.sessions, sessionID)
			}
		}
		c.sessionsMux.Unlock()
		// Resumes for an ID are serialized. A different owner is a create,
		// which cannot roll back to this displaced resident.
		if !restored && replacedSession != nil {
			replacedSession.cancelPendingExternalTools()
			replacedSession.stopEventProcessing()
		}
		session.cancelPendingExternalTools()
		// newSession starts processEvents eagerly, before the RPC confirms the
		// resume; every failure path here restores the previously-registered
		// session (if any) but never returns this failed one to the caller, so
		// its event consumer must be stopped here or it leaks forever.
		session.stopEventProcessing()
	}

	commitResumedSession := func() {
		if registrationID != "" {
			session.setGitHubTokenProviderRegistrationRelease(func() {
				c.unregisterGitHubTokenProvider(registrationID)
			})
			registrationTransferred = true
		}
		if replacedSession != nil && replacedSession != session {
			replacedSession.releaseGitHubTokenProviderRegistration()
		}
	}

	result, err := transport.Request(ctx, "session.resume", req)
	if err != nil {
		restoreReplacedSession()
		return nil, fmt.Errorf("failed to resume session: %w", err)
	}

	var response resumeSessionResponse
	if err := json.Unmarshal(result, &response); err != nil {
		restoreReplacedSession()
		return nil, fmt.Errorf("failed to unmarshal response: %w", err)
	}

	if response.SessionID != "" && response.SessionID != sessionID {
		// The runtime resolves abbreviations. Move admission and routing to that
		// identity before deciding whether this is a resident attachment.
		c.sessionsMux.Lock()
		if c.sessions[sessionID] == session {
			if replacedSession != nil {
				c.sessions[sessionID] = replacedSession
			} else {
				delete(c.sessions, sessionID)
			}
		}
		c.sessionsMux.Unlock()
		if !sessionIDsOverlap(operationSessionID, response.SessionID) {
			unlockSession()
			unlockSession = nil
			unlockSession, err = c.lockSessionOperation(ctx, response.SessionID)
			if err != nil {
				session.stopEventProcessing()
				return nil, err
			}
		}
		if err := c.waitForAutoModeRecoveryCleanup(ctx, response.SessionID); err != nil {
			session.stopEventProcessing()
			return nil, err
		}
		sessionID = response.SessionID
		// The provisional session has not received canonical callbacks yet.
		session.SessionID = sessionID
		session.RPC = rpc.NewSessionRPC(transport, sessionID)
		if adapter, ok := session.clientSessionAPIs.SessionFS.(*sessionFSAdapter); ok {
			adapter.beginCallback = func() func() { return c.beginSessionCallback(sessionID) }
		}
		c.sessionsMux.Lock()
		replacedSession = c.sessions[sessionID]
		c.sessions[sessionID] = session
		c.sessionsMux.Unlock()
	}

	session.workspacePath = response.WorkspacePath
	session.transcriptRecovery = response.TranscriptRecovery
	session.permissionModeResult = response.PermissionModeResult
	session.setOpenCanvases(response.OpenCanvases)
	session.setCapabilities(response.Capabilities)
	if recovery {
		if registrationID != "" {
			session.setGitHubTokenProviderRegistrationRelease(func() {
				c.unregisterGitHubTokenProvider(registrationID)
			})
			registrationTransferred = true
		}
		if err := c.requireAutoModeRecovery(ctx, session, replacedSession == nil, false); err != nil {
			if replacedSession != nil {
				// Resume already committed these callbacks; missing support does not roll it back.
				commitResumedSession()
			}
			return nil, err
		}
	}
	if config.OnMCPAuthRequest != nil || runtimeRefreshHandler != nil {
		if _, err := transport.Request(ctx, "session.eventLog.registerInterest", map[string]any{
			"sessionId": sessionID,
			"eventType": "mcp.oauth_required",
		}); err != nil {
			restoreReplacedSession()
			return nil, err
		}
	}

	if err := c.updateSessionOptionsForMode(ctx, session, optBackInFields{
		SkipCustomInstructions: config.SkipCustomInstructions,
		CustomAgentsLocalOnly:  config.CustomAgentsLocalOnly,
		CoauthorEnabled:        config.CoauthorEnabled,
		ManageScheduleEnabled:  config.ManageScheduleEnabled,
		IncludedBuiltinSkills:  config.IncludedBuiltinSkills,
	}, false); err != nil {
		restoreReplacedSession()
		return nil, err
	}

	if err := c.captureAhpSession(session, req); err != nil {
		restoreReplacedSession()
		return nil, err
	}
	commitResumedSession()
	return session, nil
}

// ListSessions returns metadata about all sessions known to the server.
//
// Returns a list of SessionMetadata for all available sessions, including their IDs,
// timestamps, optional summaries, and context information.
//
// An optional filter can be provided to filter sessions by working directory, git root, repository, or branch.
//
// Example:
//
//	sessions, err := client.ListSessions(context.Background(), nil)
//	if err != nil {
//	    log.Fatal(err)
//	}
//	for _, session := range sessions {
//	    fmt.Printf("Session: %s\n", session.SessionID)
//	}
//
// Example with filter:
//
//	sessions, err := client.ListSessions(context.Background(), &SessionListFilter{Repository: "owner/repo"})
func (c *Client) ListSessions(ctx context.Context, filter *SessionListFilter) ([]SessionMetadata, error) {
	transport, err := c.ensureConnected(ctx)
	if err != nil {
		return nil, err
	}

	params := listSessionsRequest{}
	if filter != nil {
		params.Filter = filter
	}
	result, err := transport.Request(ctx, "session.list", params)
	if err != nil {
		return nil, err
	}

	var response listSessionsResponse
	if err := json.Unmarshal(result, &response); err != nil {
		return nil, fmt.Errorf("failed to unmarshal sessions response: %w", err)
	}

	return response.Sessions, nil
}

// GetSessionMetadata returns metadata for a specific session by ID.
//
// This provides an efficient O(1) lookup of a single session's metadata
// instead of listing all sessions. Returns nil if the session is not found.
//
// Example:
//
//	metadata, err := client.GetSessionMetadata(context.Background(), "session-123")
//	if err != nil {
//	    log.Fatal(err)
//	}
//	if metadata != nil {
//	    fmt.Printf("Session started at: %s\n", metadata.StartTime)
//	}
func (c *Client) GetSessionMetadata(ctx context.Context, sessionID string) (*SessionMetadata, error) {
	transport, err := c.ensureConnected(ctx)
	if err != nil {
		return nil, err
	}

	result, err := transport.Request(ctx, "session.getMetadata", getSessionMetadataRequest{SessionID: sessionID})
	if err != nil {
		return nil, err
	}

	var response getSessionMetadataResponse
	if err := json.Unmarshal(result, &response); err != nil {
		return nil, fmt.Errorf("failed to unmarshal session metadata response: %w", err)
	}

	return response.Session, nil
}

// DeleteSession permanently deletes a session and all its data from disk,
// including conversation history, planning state, and artifacts.
//
// Unlike [Session.Disconnect], which only releases in-memory resources and
// preserves session data for later resumption, DeleteSession is irreversible.
// The session cannot be resumed after deletion. If the session is in the local
// sessions map, it will be removed.
//
// Example:
//
//	if err := client.DeleteSession(context.Background(), "session-123"); err != nil {
//	    log.Fatal(err)
//	}
func (c *Client) DeleteSession(ctx context.Context, sessionID string) error {
	unlockSession, err := c.lockSessionOperation(ctx, sessionID)
	if err != nil {
		return err
	}
	defer unlockSession()

	if err := c.waitForAutoModeRecoveryCleanup(ctx, sessionID); err != nil {
		return err
	}

	transport, err := c.ensureConnected(ctx)
	if err != nil {
		return err
	}

	if err := requestSessionDeletion(ctx, transport, sessionID); err != nil {
		return err
	}

	// Remove from local sessions map if present
	c.sessionsMux.Lock()
	session := c.sessions[sessionID]
	delete(c.sessions, sessionID)
	c.sessionsMux.Unlock()
	if session != nil {
		session.clearSkillProvider()
		session.clearMemoryStore()
		session.releaseGitHubTokenProviderRegistration()
	}

	return nil
}

// forgetSession stops tracking a disconnected session unless a later create or
// resume replaced it under the same ID.
func (c *Client) forgetSession(s *Session) {
	c.sessionsMux.Lock()
	if c.sessions[s.SessionID] == s {
		delete(c.sessions, s.SessionID)
	}
	c.sessionsMux.Unlock()
}

func requestSessionDeletion(ctx context.Context, transport *jsonrpc2.Client, sessionID string) error {
	result, err := transport.Request(ctx, "session.delete", deleteSessionRequest{SessionID: sessionID})
	if err != nil {
		return err
	}
	var response deleteSessionResponse
	if err := json.Unmarshal(result, &response); err != nil {
		return fmt.Errorf("failed to unmarshal delete response: %w", err)
	}
	if !response.Success {
		errorMsg := "unknown error"
		if response.Error != nil {
			errorMsg = *response.Error
		}
		return fmt.Errorf("failed to delete session %s: %s", sessionID, errorMsg)
	}
	return nil
}

// GetLastSessionID returns the ID of the most recently updated session.
//
// This is useful for resuming the last conversation when the session ID
// was not stored. Returns nil if no sessions exist.
//
// Example:
//
//	lastID, err := client.GetLastSessionID(context.Background())
//	if err != nil {
//	    log.Fatal(err)
//	}
//	if lastID != nil {
//	    session, err := client.ResumeSession(context.Background(), *lastID, &copilot.ResumeSessionOptions{
//	        OnPermissionRequest: copilot.PermissionHandler.ApproveAll,
//	    })
//	}
func (c *Client) GetLastSessionID(ctx context.Context) (*string, error) {
	transport, err := c.ensureConnected(ctx)
	if err != nil {
		return nil, err
	}

	result, err := transport.Request(ctx, "session.getLastId", getLastSessionIDRequest{})
	if err != nil {
		return nil, err
	}

	var response getLastSessionIDResponse
	if err := json.Unmarshal(result, &response); err != nil {
		return nil, fmt.Errorf("failed to unmarshal getLastId response: %w", err)
	}

	return response.SessionID, nil
}

// GetForegroundSessionID returns the ID of the session currently displayed in the TUI.
//
// This is only available when connecting to a server running in TUI+server mode
// (--ui-server). Returns nil if no foreground session is set.
//
// Example:
//
//	sessionID, err := client.GetForegroundSessionID()
//	if err != nil {
//	    log.Fatal(err)
//	}
//	if sessionID != nil {
//	    fmt.Printf("TUI is displaying session: %s\n", *sessionID)
//	}
func (c *Client) GetForegroundSessionID(ctx context.Context) (*string, error) {
	transport, err := c.ensureConnected(ctx)
	if err != nil {
		return nil, err
	}

	result, err := transport.Request(ctx, "session.getForeground", getForegroundSessionRequest{})
	if err != nil {
		return nil, err
	}

	var response getForegroundSessionResponse
	if err := json.Unmarshal(result, &response); err != nil {
		return nil, fmt.Errorf("failed to unmarshal getForeground response: %w", err)
	}

	return response.SessionID, nil
}

// SetForegroundSessionID requests the TUI to switch to displaying the specified session.
//
// This is only available when connecting to a server running in TUI+server mode
// (--ui-server).
//
// Example:
//
//	if err := client.SetForegroundSessionID("session-123"); err != nil {
//	    log.Fatal(err)
//	}
func (c *Client) SetForegroundSessionID(ctx context.Context, sessionID string) error {
	transport, err := c.ensureConnected(ctx)
	if err != nil {
		return err
	}

	result, err := transport.Request(ctx, "session.setForeground", setForegroundSessionRequest{SessionID: sessionID})
	if err != nil {
		return err
	}

	var response setForegroundSessionResponse
	if err := json.Unmarshal(result, &response); err != nil {
		return fmt.Errorf("failed to unmarshal setForeground response: %w", err)
	}

	if !response.Success {
		errorMsg := "unknown error"
		if response.Error != nil {
			errorMsg = *response.Error
		}
		return fmt.Errorf("failed to set foreground session: %s", errorMsg)
	}

	return nil
}

// On subscribes to all session lifecycle events.
//
// Lifecycle events are emitted when sessions are created, deleted, updated,
// or change foreground/background state (in TUI+server mode).
//
// Returns a function that, when called, unsubscribes the handler.
//
// Example:
//
//	unsubscribe := client.On(func(event copilot.SessionLifecycleEvent) {
//	    fmt.Printf("Session %s: %s\n", event.SessionID, event.Type)
//	})
//	defer unsubscribe()
func (c *Client) On(handler SessionLifecycleHandler) func() {
	c.lifecycleHandlersMux.Lock()
	if c.lifecycleHandlers == nil {
		c.lifecycleHandlers = make(map[uint64]SessionLifecycleHandler)
	}
	c.nextLifecycleHandlerID++
	id := c.nextLifecycleHandlerID
	c.lifecycleHandlers[id] = handler
	c.lifecycleHandlersMux.Unlock()

	return func() {
		c.lifecycleHandlersMux.Lock()
		defer c.lifecycleHandlersMux.Unlock()
		delete(c.lifecycleHandlers, id)
	}
}

// OnEventType subscribes to a specific session lifecycle event type.
//
// Returns a function that, when called, unsubscribes the handler.
//
// Example:
//
//	unsubscribe := client.OnEventType(copilot.SessionLifecycleForeground, func(event copilot.SessionLifecycleEvent) {
//	    fmt.Printf("Session %s is now in foreground\n", event.SessionID)
//	})
//	defer unsubscribe()
func (c *Client) OnEventType(eventType SessionLifecycleEventType, handler SessionLifecycleHandler) func() {
	c.lifecycleHandlersMux.Lock()
	if c.typedLifecycleHandlers == nil {
		c.typedLifecycleHandlers = make(map[SessionLifecycleEventType]map[uint64]SessionLifecycleHandler)
	}
	if c.typedLifecycleHandlers[eventType] == nil {
		c.typedLifecycleHandlers[eventType] = make(map[uint64]SessionLifecycleHandler)
	}
	c.nextLifecycleHandlerID++
	id := c.nextLifecycleHandlerID
	c.typedLifecycleHandlers[eventType][id] = handler
	c.lifecycleHandlersMux.Unlock()

	return func() {
		c.lifecycleHandlersMux.Lock()
		defer c.lifecycleHandlersMux.Unlock()
		if handlers, ok := c.typedLifecycleHandlers[eventType]; ok {
			delete(handlers, id)
		}
	}
}

// handleLifecycleEvent dispatches a lifecycle event to all registered handlers
func (c *Client) handleLifecycleEvent(event SessionLifecycleEvent) {
	c.lifecycleHandlersMux.Lock()
	// Copy handlers to avoid holding lock during callbacks
	typedHandlers := make([]SessionLifecycleHandler, 0)
	if handlers, ok := c.typedLifecycleHandlers[event.Type]; ok {
		for _, handler := range handlers {
			typedHandlers = append(typedHandlers, handler)
		}
	}
	wildcardHandlers := make([]SessionLifecycleHandler, 0, len(c.lifecycleHandlers))
	for _, handler := range c.lifecycleHandlers {
		wildcardHandlers = append(wildcardHandlers, handler)
	}
	c.lifecycleHandlersMux.Unlock()

	// Dispatch to typed handlers
	for _, handler := range typedHandlers {
		func() {
			defer func() { recover() }() // Ignore handler panics
			handler(event)
		}()
	}

	// Dispatch to wildcard handlers
	for _, handler := range wildcardHandlers {
		func() {
			defer func() { recover() }() // Ignore handler panics
			handler(event)
		}()
	}
}

// RuntimePort returns the TCP port the runtime is listening on.
// Returns 0 if the client is not connected or using stdio transport.
func (c *Client) RuntimePort() int {
	return int(c.actualPort.Load())
}

// Ping sends a ping request to the server to verify connectivity.
// It starts the client if needed.
//
// The message parameter is optional and will be echoed back in the response.
// Returns a PingResponse containing the message and server timestamp, or an error.
//
// Example:
//
//	resp, err := client.Ping(context.Background(), "health check")
//	if err != nil {
//	    log.Printf("Server unreachable: %v", err)
//	} else {
//	    log.Printf("Server responded at %s", resp.Timestamp)
//	}
func (c *Client) Ping(ctx context.Context, message string) (*PingResponse, error) {
	transport, err := c.ensureConnected(ctx)
	if err != nil {
		return nil, err
	}
	return ping(ctx, transport, message)
}

func ping(ctx context.Context, transport *jsonrpc2.Client, message string) (*PingResponse, error) {
	result, err := transport.Request(ctx, "ping", pingRequest{Message: message})
	if err != nil {
		return nil, err
	}

	var response PingResponse
	if err := json.Unmarshal(result, &response); err != nil {
		return nil, err
	}
	return &response, nil
}

// GetStatus returns CLI status including version and protocol information,
// starting the client if needed.
func (c *Client) GetStatus(ctx context.Context) (*GetStatusResponse, error) {
	transport, err := c.ensureConnected(ctx)
	if err != nil {
		return nil, err
	}

	result, err := transport.Request(ctx, "status.get", getStatusRequest{})
	if err != nil {
		return nil, err
	}

	var response GetStatusResponse
	if err := json.Unmarshal(result, &response); err != nil {
		return nil, err
	}
	return &response, nil
}

// GetAuthStatus returns current authentication status, starting the client if needed.
func (c *Client) GetAuthStatus(ctx context.Context) (*GetAuthStatusResponse, error) {
	transport, err := c.ensureConnected(ctx)
	if err != nil {
		return nil, err
	}

	result, err := transport.Request(ctx, "auth.getStatus", getAuthStatusRequest{})
	if err != nil {
		return nil, err
	}

	var response GetAuthStatusResponse
	if err := json.Unmarshal(result, &response); err != nil {
		return nil, err
	}
	return &response, nil
}

// ListModels returns available models with their metadata.
//
// Results are cached after the first successful call to avoid rate limiting.
// The cache is cleared when the client disconnects.
// It starts the client if needed, unless a custom OnListModels handler is configured.
func (c *Client) ListModels(ctx context.Context) ([]Model, error) {
	var transport *jsonrpc2.Client
	if c.onListModels == nil {
		var err error
		// Startup and failed-start cleanup must not wait on the models lock.
		transport, err = c.ensureConnected(ctx)
		if err != nil {
			return nil, err
		}
	}
	// Use mutex for locking to prevent race condition with concurrent calls
	c.modelsCacheMux.Lock()
	defer c.modelsCacheMux.Unlock()

	if transport != nil {
		c.transportMux.Lock()
		current := c.startup == nil && c.client == transport && c.stopsInProgress == 0
		c.transportMux.Unlock()
		if !current {
			return nil, errors.New("client not connected")
		}
	}

	// Check cache (already inside lock)
	if c.modelsCache != nil {
		result := make([]Model, len(c.modelsCache))
		copy(result, c.modelsCache)
		return result, nil
	}

	var models []Model
	if c.onListModels != nil {
		// Use custom handler instead of CLI RPC
		var err error
		models, err = c.onListModels(ctx)
		if err != nil {
			return nil, err
		}
	} else {
		// Cache miss - fetch from backend while holding lock
		result, err := transport.Request(ctx, "models.list", listModelsRequest{})
		if err != nil {
			return nil, err
		}

		var response rpc.ModelList
		if err := json.Unmarshal(result, &response); err != nil {
			return nil, fmt.Errorf("failed to unmarshal models response: %w", err)
		}
		models = response.Models
	}

	// Update cache before releasing lock (copy to prevent external mutation)
	cache := make([]Model, len(models))
	copy(cache, models)
	c.modelsCache = cache

	// Return a copy to prevent cache mutation
	result := make([]Model, len(models))
	copy(result, models)
	return result, nil
}

// minProtocolVersion is the minimum protocol version this SDK can communicate with.
const minProtocolVersion = 4

// runtimeShutdownTimeout is a variable so tests can shorten it.
var runtimeShutdownTimeout = 10 * time.Second

const processExitTimeout = 10 * time.Second

// processOutputDrainTimeout bounds how long waiting for a stdio runtime that has
// exited also waits for its output to close. A process the runtime started can
// inherit its stdout and stderr and hold them open after the runtime exits.
const processOutputDrainTimeout = time.Second

// verifyProtocolVersion sends the `connect` handshake (carrying the optional token) and
// verifies the server's protocol version. Falls back to `ping` against legacy servers
// that don't implement `connect`.
func (c *Client) verifyProtocolVersion(ctx context.Context) error {
	if c.client == nil {
		return fmt.Errorf("client not connected")
	}
	maxVersion := GetSDKProtocolVersion()

	var serverVersion *int
	tokenPtr := (*string)(nil)
	if c.effectiveConnectionToken != "" {
		t := c.effectiveConnectionToken
		tokenPtr = &t
	}
	connectReq := &connectHandshakeRequest{
		Token: tokenPtr,
		SupportedTaskKinds: []rpc.TaskKind{
			rpc.TaskKindAgent,
			rpc.TaskKindClient,
			rpc.TaskKindShell,
		},
	}
	if c.options.Entra != nil {
		connectReq.Entra = &rpc.EntraAuthConfig{
			ClientID:             c.options.Entra.ClientID,
			MatsAppName:          c.options.Entra.MatsAppName,
			OneAuthApplicationID: c.options.Entra.OneAuthApplicationID,
		}
		if c.options.Entra.TokenProvider != nil {
			connectReq.EntraTokenProvider = Bool(true)
		}
	}
	// Opt in to GitHub telemetry forwarding at the connection level when a handler is
	// registered (mirrors the runtime, which reads this flag on the `connect` handshake
	// so the first session's un-replayable `session.start` event is forwarded). Also
	// sent on session.create/resume for older CLIs.
	if c.options.OnGitHubTelemetry != nil {
		connectReq.EnableGitHubTelemetryForwarding = Bool(true)
	}
	// Declare the integrating host's identity so the runtime attributes the
	// telemetry it emits on this connection to a consistent surface instead of
	// its own build. Nil when the app didn't supply it.
	connectReq.ClientInfo = c.options.ClientInfo.toWire()
	rawConnectResult, err := c.client.Request(ctx, "connect", connectReq)
	if err != nil {
		var rpcErr *jsonrpc2.Error
		if errors.As(err, &rpcErr) && (rpcErr.Code == jsonrpc2.ErrMethodNotFound.Code || rpcErr.Message == "Unhandled method connect") {
			if c.options.Entra != nil {
				return fmt.Errorf("this runtime does not support Entra client configuration: %w", err)
			}
			// Legacy server without `connect`; fall back to `ping`. A token, if any,
			// is silently dropped — the legacy server can't enforce one. Ping would
			// wait for this Start to publish the transport, so use it directly.
			pingResult, perr := ping(ctx, c.client, "")
			if perr != nil {
				return perr
			}
			serverVersion = pingResult.ProtocolVersion
		} else {
			return err
		}
	} else {
		var connectResult rpc.ConnectResult
		if err := json.Unmarshal(rawConnectResult, &connectResult); err != nil {
			return err
		}
		if c.options.Entra != nil && c.options.Entra.TokenProvider != nil &&
			(connectResult.EntraTokenProvider == nil || !*connectResult.EntraTokenProvider) {
			return fmt.Errorf("this runtime does not support host-owned Entra token providers")
		}
		v := int(connectResult.ProtocolVersion)
		serverVersion = &v
	}

	if serverVersion == nil {
		return fmt.Errorf("SDK protocol version mismatch: SDK supports versions %d-%d, but server does not report a protocol version. Please update your server to ensure compatibility", minProtocolVersion, maxVersion)
	}

	if *serverVersion < minProtocolVersion || *serverVersion > maxVersion {
		return fmt.Errorf("SDK protocol version mismatch: SDK supports versions %d-%d, but server reports version %d. Please update your SDK or server to ensure compatibility", minProtocolVersion, maxVersion, *serverVersion)
	}

	c.negotiatedProtocolVersion = *serverVersion
	return nil
}

type connectHandshakeRequest struct {
	Entra                           *rpc.EntraAuthConfig   `json:"entra,omitempty"`
	EntraTokenProvider              *bool                  `json:"entraTokenProvider,omitempty"`
	Token                           *string                `json:"token,omitempty"`
	EnableGitHubTelemetryForwarding *bool                  `json:"enableGitHubTelemetryForwarding,omitempty"`
	ClientInfo                      *rpc.ConnectClientInfo `json:"clientInfo,omitempty"`
	SupportedTaskKinds              []rpc.TaskKind         `json:"supportedTaskKinds,omitempty"`
}

// stderrBufferSize is the maximum number of bytes kept from the CLI process's
// stderr. Only the tail is retained so that memory stays bounded even when the
// process produces a large amount of diagnostic output.
const stderrBufferSize = 64 * 1024

// startFailure wraps a failed process start. Whether Go names a missing
// working directory itself (chdir <dir>) depends on the platform, and a working
// directory that is a file only fails with the program path, so the directory
// is named here whenever it is missing or not a directory.
func (c *Client) startFailure(err error) error {
	if dir := c.process.Dir; dir != "" {
		if info, statErr := os.Stat(dir); errors.Is(statErr, os.ErrNotExist) || errors.Is(statErr, syscall.ENOTDIR) || (statErr == nil && !info.IsDir()) {
			return fmt.Errorf("failed to start CLI server: working directory %s does not exist or is not a directory: %w", dir, err)
		}
	}
	return fmt.Errorf("failed to start CLI server: %w", err)
}

// startCLIServer starts the CLI server process.
//
// This spawns the CLI server as a subprocess using the configured transport
// mode (stdio or TCP).
func (c *Client) startCLIServer(ctx context.Context) error {
	if c.useInProcess {
		return c.startInProcess(ctx)
	}

	bundledRuntimePath := ""
	if c.cliPath == "" {
		var err error
		bundledRuntimePath, err = embeddedcli.RuntimePath()
		if err != nil {
			return fmt.Errorf("embedded runtime installation failed: %w", err)
		}
	}
	cliPath, err := resolveRuntimeExecutable(c.cliPath, bundledRuntimePath)
	if err != nil {
		return err
	}

	// Start with user-provided CLIArgs, then add SDK-managed args
	args := append([]string{}, c.cliArgs...)
	args = append(args, "--headless", "--no-auto-update")
	// Only pass --log-level when explicitly configured; otherwise let the
	// runtime use its own default.
	if c.options.LogLevel != "" {
		args = append(args, "--log-level", c.options.LogLevel)
	}

	// Choose transport mode
	if c.useStdio {
		args = append(args, "--stdio")
	} else if c.port > 0 {
		args = append(args, "--port", strconv.Itoa(c.port))
	}

	// Add auth-related flags
	if c.options.GitHubToken != "" {
		args = append(args, "--auth-token-env", "COPILOT_SDK_AUTH_TOKEN")
	}
	// Default useLoggedInUser to false when GitHubToken is provided
	useLoggedInUser := true
	if c.options.UseLoggedInUser != nil {
		useLoggedInUser = *c.options.UseLoggedInUser
	} else if c.options.GitHubToken != "" {
		useLoggedInUser = false
	}
	if !useLoggedInUser {
		args = append(args, "--no-auto-login")
	}

	if c.options.SessionIdleTimeoutSeconds > 0 {
		args = append(args, "--session-idle-timeout", strconv.Itoa(c.options.SessionIdleTimeoutSeconds))
	}

	// If CLIPath is a .js file, run it with node
	// Note we can't rely on the shebang as Windows doesn't support it
	command := cliPath
	if strings.HasSuffix(cliPath, ".js") {
		command = "node"
		args = append([]string{cliPath}, args...)
	}

	command, err = lookPathInEnvironment(command, c.environment, c.workingDirectory)
	if err != nil {
		return fmt.Errorf("failed to start CLI server: %w", err)
	}
	c.process = exec.Command(command, args...)

	// Configure platform-specific process attributes (e.g., hide window on Windows)
	configureProcAttr(c.process)

	// Set working directory if specified
	if c.workingDirectory != "" {
		c.process.Dir = c.workingDirectory
	}

	c.process.Env = append([]string{}, c.environment...)
	for key, value := range c.options.InternalTestOptions.environment() {
		c.process.Env = setEnvValue(c.process.Env, key, value)
	}
	if c.options.GitHubToken != "" {
		c.process.Env = setEnvValue(c.process.Env, "COPILOT_SDK_AUTH_TOKEN", c.options.GitHubToken)
	}

	if c.effectiveConnectionToken != "" {
		c.process.Env = setEnvValue(c.process.Env, "COPILOT_CONNECTION_TOKEN", c.effectiveConnectionToken)
	}

	if c.options.BaseDirectory != "" {
		c.process.Env = setEnvValue(c.process.Env, "COPILOT_HOME", c.options.BaseDirectory)
	}

	if c.options.Mode == ModeEmpty {
		c.process.Env = setEnvValue(c.process.Env, "COPILOT_DISABLE_KEYTAR", "1")
	}

	if c.options.Mode != ModeEmpty {
		c.process.Env = setEnvValue(c.process.Env, "COPILOT_RUNTIME_PROCESS_FILE_LOGGING", "1")
	}

	for key, value := range c.options.Telemetry.environment() {
		c.process.Env = setEnvValue(c.process.Env, key, value)
	}

	if c.useStdio {
		// For stdio mode, we need stdin/stdout pipes
		stdin, err := c.process.StdinPipe()
		if err != nil {
			return fmt.Errorf("failed to create stdin pipe: %w", err)
		}

		stdout, err := c.process.StdoutPipe()
		if err != nil {
			return fmt.Errorf("failed to create stdout pipe: %w", err)
		}

		c.process.Stderr = truncbuffer.NewTruncBuffer(stderrBufferSize)
		// Without a bound, Wait keeps copying stderr while a process the runtime
		// started holds it, so neither processDone nor the end of stdout reports
		// the exit and Start keeps the dead connection. Once the bound passes,
		// Wait closes the pipes, which ends the reader as an exit without one does.
		c.process.WaitDelay = processOutputDrainTimeout

		if err := c.process.Start(); err != nil {
			return c.startFailure(err)
		}

		c.processStdin = stdin
		c.monitorProcess()

		// Create JSON-RPC client immediately
		c.client = jsonrpc2.NewClient(stdin, stdout)
		c.client.SetProcessDone(c.processDone, c.processErrorPtr)
		c.RPC = rpc.NewServerRPC(c.client)
		c.internalRPC = rpc.NewInternalServerRPC(c.client)
		c.setupNotificationHandler()
		c.client.Start()

		return nil
	} else {
		// For TCP mode, capture stdout to get port number
		stdout, err := c.process.StdoutPipe()
		if err != nil {
			return fmt.Errorf("failed to create stdout pipe: %w", err)
		}

		c.process.Stderr = truncbuffer.NewTruncBuffer(stderrBufferSize)

		if err := c.process.Start(); err != nil {
			return c.startFailure(err)
		}

		c.monitorProcess()

		proc := c.process
		scanner := bufio.NewScanner(stdout)
		portRegex := regexp.MustCompile(`listening on port (\d+)`)

		ctx, cancel := context.WithTimeout(ctx, 10*time.Second)
		defer cancel()

		for {
			select {
			case <-ctx.Done():
				killErr := c.killProcess()
				baseErr := fmt.Errorf("failed waiting for CLI server to start: %w", ctx.Err())
				if buf, ok := proc.Stderr.(*truncbuffer.TruncBuffer); ok {
					if stderr := strings.TrimSpace(buf.String()); stderr != "" {
						baseErr = fmt.Errorf("%w; stderr: %s", baseErr, stderr)
					}
				}
				return errors.Join(baseErr, killErr)
			case <-c.processDone:
				killErr := c.killProcess()
				baseErr := errors.New("CLI server process exited before reporting port")
				if buf, ok := proc.Stderr.(*truncbuffer.TruncBuffer); ok {
					if stderr := strings.TrimSpace(buf.String()); stderr != "" {
						baseErr = fmt.Errorf("%w; stderr: %s", baseErr, stderr)
					}
				}
				return errors.Join(baseErr, killErr)
			default:
				if scanner.Scan() {
					line := scanner.Text()
					if matches := portRegex.FindStringSubmatch(line); len(matches) > 1 {
						port, err := strconv.Atoi(matches[1])
						if err != nil {
							killErr := c.killProcess()
							return errors.Join(fmt.Errorf("failed to parse port: %w", err), killErr)
						}
						c.actualPort.Store(int64(port))
						return nil
					}
				}
			}
		}
	}
}

// startInProcess loads the native runtime library and wires the JSON-RPC client
// to its FFI byte streams.
func (c *Client) startInProcess(ctx context.Context) error {
	if !inProcessAvailable {
		return errors.New("in-process transport unavailable: supported hosts are macOS, Linux, and Windows on amd64 or arm64 without the copilot_no_inprocess build tag; select SpawnedStdioConnection, SpawnedTCPConnection, TCPConnection, or set COPILOT_SDK_DEFAULT_CONNECTION=stdio")
	}

	cliEntrypoint := c.cliPath
	if cliEntrypoint == "" {
		cliEntrypoint = getEnvValue(c.environment, "COPILOT_CLI_PATH")
	}
	runtimePath := cliEntrypoint
	if runtimePath == "" {
		var err error
		runtimePath, err = embeddedcli.RuntimePath()
		if err != nil {
			return fmt.Errorf("in-process runtime installation failed: %w", err)
		}
	}
	if runtimePath == "" {
		return errors.New("in-process runtime unavailable: build with the bundled embedded runtime or set COPILOT_CLI_PATH to a compatible runtime package")
	}

	integrationID := strings.TrimSpace(getEnvValue(c.environment, "GITHUB_COPILOT_INTEGRATION_ID"))
	config := c.inProcessHostConfig(integrationID)

	host, err := createInProcessHost(runtimePath, cliEntrypoint, config)
	if err != nil {
		return err
	}
	// Own the host before the blocking handshake so a cancelled or failed start
	// leaves it disposable by Stop/ForceStop rather than leaking (host.Start runs
	// on its own goroutine and cannot be interrupted once the native call begins).
	c.ffiHost = host

	errCh := make(chan error, 1)
	go func() { errCh <- host.Start() }()
	select {
	case err := <-errCh:
		if err != nil {
			host.Dispose()
			c.ffiHost = nil
			return err
		}
	case <-ctx.Done():
		c.ffiHost = nil
		go func() {
			<-errCh
			host.Dispose()
		}()
		return ctx.Err()
	}

	c.client = jsonrpc2.NewClient(host.Writer(), host.Reader())
	c.RPC = rpc.NewServerRPC(c.client)
	c.internalRPC = rpc.NewInternalServerRPC(c.client)
	c.setupNotificationHandler()
	c.client.Start()
	return host.ConfigureUiThread(c.runOnUiThread != nil)
}

func (c *Client) inProcessHostConfig(cliLoginSyncIntegrationID string) inProcessHostConfig {
	args := make([]string, 0, 8)
	if c.options.LogLevel != "" {
		args = append(args, "--log-level", c.options.LogLevel)
	}
	if c.options.GitHubToken != "" {
		args = append(args, "--auth-token-env", "COPILOT_SDK_AUTH_TOKEN")
	}
	useLoggedInUser := true
	if c.options.UseLoggedInUser != nil {
		useLoggedInUser = *c.options.UseLoggedInUser
	} else if c.options.GitHubToken != "" {
		useLoggedInUser = false
	}
	if !useLoggedInUser {
		args = append(args, "--no-auto-login")
	}
	if c.options.SessionIdleTimeoutSeconds > 0 {
		args = append(args, "--session-idle-timeout", strconv.Itoa(c.options.SessionIdleTimeoutSeconds))
	}
	if cliLoginSyncIntegrationID != "" {
		args = append(args, "--cli-login-sync-integration-id="+cliLoginSyncIntegrationID)
	}

	environment := make(map[string]string, len(c.environment))
	for _, entry := range c.environment {
		key, value, ok := strings.Cut(entry, "=")
		if !ok || key == "" || !utf8.ValidString(key) {
			continue
		}
		if runtime.GOOS == "windows" {
			key = strings.ToUpper(key)
		}
		if !utf8.ValidString(value) {
			switch key {
			case "http_proxy", "HTTP_PROXY", "https_proxy", "HTTPS_PROXY", "COPILOT_PROXY_KERBEROS_SPN", "TMPDIR":
				// Preserve explicit-setting precedence, as native capture does.
				value = "\uFFFD"
			default:
				continue
			}
		}
		environment[key] = value
	}
	if c.options.GitHubToken != "" {
		environment["COPILOT_SDK_AUTH_TOKEN"] = c.options.GitHubToken
	}
	if c.options.BaseDirectory != "" {
		environment["COPILOT_HOME"] = c.options.BaseDirectory
		environment["COPILOT_CACHE_HOME"] = filepath.Join(c.options.BaseDirectory, "cache")
	}
	if c.options.Mode == ModeEmpty {
		environment["COPILOT_DISABLE_KEYTAR"] = "1"
	}
	for key, value := range c.options.InternalTestOptions.environment() {
		if runtime.GOOS == "windows" {
			key = strings.ToUpper(key)
		}
		environment[key] = value
	}
	for key, value := range c.options.Telemetry.environment() {
		environment[key] = value
	}

	return inProcessHostConfig{
		Environment: environment,
		Args:        args,
	}
}

func (c *Client) killProcess() error {
	// Tear down the in-process FFI host on error paths that reuse killProcess to
	// abort a start (there is no OS process to kill in that mode).
	if c.ffiHost != nil {
		c.ffiHost.Dispose()
		c.ffiHost = nil
	}
	var killErr error
	if p := c.osProcess.Load(); p != nil {
		if err := p.Kill(); err != nil && !errors.Is(err, os.ErrProcessDone) {
			killErr = fmt.Errorf("failed to kill CLI process: %w", err)
		}
	}
	if c.processStdin != nil {
		_ = c.processStdin.Close()
		c.processStdin = nil
	}
	if killErr == nil {
		c.process = nil
	}
	return killErr
}

func (c *Client) killProcessAndWait() error {
	done := c.processDone
	killErr := c.killProcess()
	if done == nil {
		return killErr
	}

	select {
	case <-done:
		// Reaping confirms exit even when Windows reports a released handle.
		c.osProcess.Store(nil)
		c.process = nil
		return nil
	case <-time.After(processExitTimeout):
		return errors.Join(killErr, fmt.Errorf("timed out waiting for CLI process to exit after kill"))
	}
}

// monitorProcess signals when the CLI process exits and captures any exit error.
// processError is intentionally a local: each process lifecycle gets its own
// error value, so goroutines from previous processes can't overwrite the
// current one. Closing the channel synchronizes with readers, guaranteeing
// they see the final processError value.
func (c *Client) monitorProcess() {
	done := make(chan struct{})
	c.processDone = done
	proc := c.process
	c.osProcess.Store(proc.Process)
	var processError error
	c.processErrorPtr = &processError
	go func() {
		waitErr := proc.Wait()
		if errors.Is(waitErr, exec.ErrWaitDelay) {
			// The runtime exited successfully; only a process it started still
			// held its output.
			waitErr = nil
		}
		var stderrOutput string
		if buf, ok := proc.Stderr.(*truncbuffer.TruncBuffer); ok {
			stderrOutput = strings.TrimSpace(buf.String())
		}
		if waitErr != nil {
			if stderrOutput != "" {
				processError = fmt.Errorf("CLI process exited: %w\nstderr: %s", waitErr, stderrOutput)
			} else {
				processError = fmt.Errorf("CLI process exited: %w", waitErr)
			}
		} else {
			if stderrOutput != "" {
				processError = fmt.Errorf("CLI process exited unexpectedly\nstderr: %s", stderrOutput)
			} else {
				processError = errors.New("CLI process exited unexpectedly")
			}
		}
		close(done)
	}()
}

// connectToServer establishes a connection to the server.
func (c *Client) connectToServer(ctx context.Context) error {
	if c.useStdio || c.useInProcess {
		// Already connected: stdio in startCLIServer, FFI streams in startInProcess.
		return nil
	}

	// Connect via TCP
	return c.connectViaTCP(ctx)
}

// connectViaTCP connects to the CLI server via TCP socket.
func (c *Client) connectViaTCP(ctx context.Context) error {
	port := c.actualPort.Load()
	if port == 0 {
		return fmt.Errorf("server port not available")
	}

	// Merge a 10-second timeout with the caller's context so whichever
	// deadline comes first wins.
	address := net.JoinHostPort(c.actualHost, fmt.Sprintf("%d", port))
	dialCtx, cancel := context.WithTimeout(ctx, 10*time.Second)
	defer cancel()
	var dialer net.Dialer
	conn, err := dialer.DialContext(dialCtx, "tcp", address)
	if err != nil {
		return fmt.Errorf("failed to connect to CLI server at %s: %w", address, err)
	}

	c.conn = conn

	// Create JSON-RPC client with the connection
	c.client = jsonrpc2.NewClient(conn, conn)
	if c.processDone != nil {
		c.client.SetProcessDone(c.processDone, c.processErrorPtr)
	}
	c.RPC = rpc.NewServerRPC(c.client)
	c.internalRPC = rpc.NewInternalServerRPC(c.client)
	c.setupNotificationHandler()
	c.client.Start()

	return nil
}

// setupNotificationHandler configures handlers for session events and RPC requests.
func (c *Client) setupNotificationHandler() {
	transport := c.client
	transport.SetOnClose(func() { c.handleConnectionClose(transport) })
	c.client.SetRequestScope(func(params json.RawMessage) func() {
		var request struct {
			SessionID string `json:"sessionId"`
		}
		if json.Unmarshal(params, &request) == nil && request.SessionID != "" {
			return c.beginSessionCallback(request.SessionID)
		}
		return func() {}
	})
	c.client.SetRequestHandler("session.event", jsonrpc2.NotificationHandlerFor(c.handleSessionEvent))
	c.client.SetRequestHandler("session.lifecycle", jsonrpc2.NotificationHandlerFor(c.handleLifecycleEvent))
	c.client.SetRequestHandler("userInput.request", jsonrpc2.RequestHandlerFor(c.handleUserInputRequest))
	c.client.SetRequestHandlerWithPreparation("shellAuthentication.request", jsonrpc2.RequestHandlerFor(c.handleShellAuthenticationRequest), c.prepareShellAuthenticationRequest)
	c.client.SetRequestHandler("shellAuthentication.cancel", jsonrpc2.RequestHandlerFor(c.handleShellAuthenticationCancel))
	c.client.SetRequestHandler("exitPlanMode.request", jsonrpc2.RequestHandlerFor(c.handleExitPlanModeRequest))
	c.client.SetRequestHandler("autoModeSwitch.request", jsonrpc2.RequestHandlerFor(c.handleAutoModeSwitchRequest))
	c.client.SetRequestHandler("systemMessage.transform", jsonrpc2.RequestHandlerFor(c.handleSystemMessageTransform))
	c.client.SetRequestContextHandler("skillProvider.list", c.handleSkillProviderList)
	c.client.SetRequestContextHandler("skillProvider.read", c.handleSkillProviderRead)
	// Internal experimental callback backing CreateSessionOptions.StoreMemory.
	c.client.SetRequestContextHandler("memoryStore.store", c.handleMemoryStore)
	rpc.RegisterClientSessionAPIHandlers(c.client, func(sessionID string) *rpc.ClientSessionAPIHandlers {
		c.sessionsMux.Lock()
		defer c.sessionsMux.Unlock()
		session := c.sessions[sessionID]
		if session == nil {
			return nil
		}
		return session.clientSessionAPIs
	})
	// hooks.invoke is a client-global RPC method: one connection-level handler
	// receives every hook callback and routes to the owning session via the
	// payload's sessionId. Always register the global handlers so the generated
	// hooks.invoke handler is wired to our dispatcher.
	handlers := &rpc.ClientGlobalAPIHandlers{
		ExtensionLaunchProvider: c.options.ExtensionLaunchProvider,
		Hooks:                   &hooksAdapter{client: c},
		GitHubToken:             &gitHubTokenAdapter{client: c},
		UIThread:                &uiThreadAdapter{host: c.ffiHost, runOnUiThread: c.runOnUiThread},
	}

	if c.options.RequestHandler != nil {
		llmInference := c.RPC.LlmInference
		adapter := newCopilotRequestAdapter(c.options.RequestHandler, func() *rpc.ServerLlmInferenceAPI {
			return llmInference
		})
		c.requestAdapterMux.Lock()
		previous := c.requestAdapter
		c.requestAdapter = adapter
		c.requestAdapterMux.Unlock()
		if previous != nil {
			previous.close()
		}
		handlers.LlmInference = adapter
	}
	if c.options.OnGitHubTelemetry != nil {
		handlers.GitHubTelemetry = &gitHubTelemetryAdapter{callback: c.options.OnGitHubTelemetry}
	}
	rpc.RegisterClientGlobalAPIHandlers(c.client, handlers)
	if c.options.Entra != nil && c.options.Entra.TokenProvider != nil {
		c.client.SetRequestContextHandler("entraToken.getToken",
			entraTokenHandler(c.options.Entra.TokenProvider, c.client.ConnectionClosed()))
	}
	if c.options.Network != nil {
		// Registered directly so the callbacks receive the request's cancellation.
		adapter := &networkAdapter{config: *c.options.Network, connectionClosed: c.client.ConnectionClosed()}
		adapter.register(c.client)
	}
	if c.options.InstallationConfirmationHandler != nil {
		adapter := newInstallationConfirmationAdapter(c.options.InstallationConfirmationHandler, c.client.ConnectionClosed())
		c.installationConfirmationAdapterMux.Lock()
		previous := c.installationConfirmationAdapter
		c.installationConfirmationAdapter = adapter
		c.installationConfirmationAdapterMux.Unlock()
		if previous != nil {
			previous.close()
		}
		c.client.SetRequestContextHandler("installations.confirm", adapter.handle)
	}
	c.client.SetRequestHandler("host.materializeSession", jsonrpc2.RequestHandlerFor(c.materializeAhpSession))
	c.client.SetRequestHandlerWithPreparation("host.validateAuthentication", jsonrpc2.RequestHandlerFor(c.validateAhpAuthentication), func(raw json.RawMessage) *jsonrpc2.Error {
		var request rpc.HostValidateAuthenticationRequest
		if err := json.Unmarshal(raw, &request); err != nil {
			return jsonrpc2.ErrInvalidParams
		}
		return c.prepareAhpAuthentication(&request)
	})
	c.client.SetRequestHandler("host.authenticationCancelled", jsonrpc2.NotificationHandlerFor(c.cancelAhpAuthentication))
	c.client.SetRequestHandler("host.sessionReleased", jsonrpc2.NotificationHandlerFor(c.releaseAhpSession))
	c.client.SetRequestHandler("host.exited", jsonrpc2.NotificationHandlerFor(c.handleAhpExit))
}

func (c *Client) registerGitHubTokenProvider(provider GitHubTokenProvider) string {
	if provider == nil {
		return ""
	}
	registrationID := uuid.NewString()
	c.gitHubTokenProvidersMux.Lock()
	if c.gitHubTokenProviders == nil {
		c.gitHubTokenProviders = make(map[string]GitHubTokenProvider)
	}
	c.gitHubTokenProviders[registrationID] = provider
	c.gitHubTokenProvidersMux.Unlock()
	return registrationID
}

func (c *Client) unregisterGitHubTokenProvider(registrationID string) {
	if registrationID == "" {
		return
	}
	c.gitHubTokenProvidersMux.Lock()
	delete(c.gitHubTokenProviders, registrationID)
	c.gitHubTokenProvidersMux.Unlock()
}

func (c *Client) clearGitHubTokenProviders() {
	c.gitHubTokenProvidersMux.Lock()
	c.gitHubTokenProviders = make(map[string]GitHubTokenProvider)
	c.gitHubTokenProvidersMux.Unlock()
}

func (c *Client) handleConnectionClose(transport *jsonrpc2.Client) {
	c.transportMux.Lock()
	current := c.client == transport
	c.transportMux.Unlock()
	if !current {
		return
	}
	c.disconnectAhpHosts()
	c.closeCopilotRequestAdapter()
	c.closeInstallationConfirmationAdapter()
	c.clearGitHubTokenProviders()
	c.sessionsMux.Lock()
	sessions := make([]*Session, 0, len(c.sessions))
	for _, session := range c.sessions {
		sessions = append(sessions, session)
	}
	c.sessionsMux.Unlock()
	c.cancelPendingShellAuthenticationRequests()
	for _, session := range sessions {
		session.clearSkillProvider()
		session.clearMemoryStore()
		session.cancelPendingExternalTools()
	}
	// Avoid deadlocking with Stop/ForceStop, which hold startStopMux while
	// waiting for the JSON-RPC read loop to finish.
	go func() {
		c.startStopMux.Lock()
		defer c.startStopMux.Unlock()
		if c.client == transport {
			c.state = stateDisconnected
		}
	}()
}

func (c *Client) closeCopilotRequestAdapter() {
	c.requestAdapterMux.Lock()
	adapter := c.requestAdapter
	c.requestAdapterMux.Unlock()
	if adapter != nil {
		adapter.close()
	}
}

func (c *Client) closeInstallationConfirmationAdapter() {
	c.installationConfirmationAdapterMux.Lock()
	adapter := c.installationConfirmationAdapter
	c.installationConfirmationAdapter = nil
	c.installationConfirmationAdapterMux.Unlock()
	if adapter != nil {
		adapter.close()
	}
}

func (c *Client) waitForAutoModeRecoveryCleanup(ctx context.Context, sessionID string) error {
	// Start writes c.client while startup is set (see ensureConnected), so read
	// it only when no Start is in progress. The transport a Start is creating
	// has no cleanups yet, and Start has already retired the one it replaces.
	c.transportMux.Lock()
	var transport *jsonrpc2.Client
	if c.startup == nil {
		transport = c.client
	}
	c.transportMux.Unlock()
	c.sessionsMux.Lock()
	cleanups := c.autoModeRecoveryCleanups[transport]
	pending := make(map[string]<-chan struct{})
	for id, finished := range cleanups {
		if sessionIDsOverlap(id, sessionID) {
			pending[id] = finished
		}
	}
	c.sessionsMux.Unlock()
	for id, finished := range pending {
		select {
		case <-finished:
		default:
			c.sessionOperationsMux.Lock()
			activeCallback := c.hasOverlappingSessionCallback(id)
			c.sessionOperationsMux.Unlock()
			if activeCallback {
				return fmt.Errorf("cannot operate on session %s during an active session callback", sessionID)
			}
		}
		// Detach is ID-only, so aliases must wait even after caller cancellation.
		select {
		case <-finished:
			c.sessionsMux.Lock()
			failed := cleanups[id] == finished
			c.sessionsMux.Unlock()
			if failed {
				return fmt.Errorf("session %s cleanup failed after rejected AutoModeRecovery enrollment; stop and restart the client before reusing this ID", sessionID)
			}
		case <-ctx.Done():
			return ctx.Err()
		}
	}
	return nil
}

func (c *Client) requireAutoModeRecovery(ctx context.Context, s *Session, detachOnFailure, deleteOnFailure bool) error {
	caps := s.Capabilities()
	if caps.UI != nil && caps.UI.AutoModeRecovery {
		return nil
	}
	unsupported := errors.New("the runtime does not support AutoModeRecovery")
	return c.cleanupFailedSession(ctx, s, unsupported, detachOnFailure, deleteOnFailure)
}

func (c *Client) cleanupFailedSession(ctx context.Context, s *Session, failure error, detachOnFailure, deleteOnFailure bool) error {
	c.sessionsMux.Lock()
	// A failed resident resume, or an already replaced owner, must not detach its successor.
	if !detachOnFailure || c.sessions[s.SessionID] != s {
		c.sessionsMux.Unlock()
		if detachOnFailure {
			s.stopEventProcessing()
		}
		return failure
	}
	finished := make(chan struct{})
	if c.autoModeRecoveryCleanups == nil {
		c.autoModeRecoveryCleanups = make(map[*jsonrpc2.Client]map[string]<-chan struct{})
	}
	cleanups := c.autoModeRecoveryCleanups[s.client]
	if cleanups == nil {
		cleanups = make(map[string]<-chan struct{})
		c.autoModeRecoveryCleanups[s.client] = cleanups
	}
	cleanups[s.SessionID] = finished
	c.sessionsMux.Unlock()

	cleanupCtx, cancel := context.WithTimeout(context.WithoutCancel(ctx), 10*time.Second)
	done := make(chan error, 1)
	go func() {
		defer cancel()
		var err error
		defer func() {
			s.stopEventProcessing()
			c.sessionsMux.Lock()
			if c.sessions[s.SessionID] == s {
				delete(c.sessions, s.SessionID)
			}
			if err == nil && cleanups[s.SessionID] == finished {
				delete(cleanups, s.SessionID)
			}
			close(finished)
			c.sessionsMux.Unlock()
			done <- err
		}()
		err = s.detachRuntime(cleanupCtx)
		if deleteOnFailure {
			// Keep same-ID admission fenced through cloud rollback, even if the caller stopped waiting.
			deleteCtx, cancelDelete := context.WithTimeout(context.WithoutCancel(ctx), 10*time.Second)
			err = errors.Join(err, requestSessionDeletion(deleteCtx, s.client, s.SessionID))
			cancelDelete()
		}
		if err != nil {
			log.Printf("Failed to clean up session %s after setup failed: %v", s.SessionID, err)
		}
	}()
	select {
	case err := <-done:
		return errors.Join(failure, err, ctx.Err())
	case <-ctx.Done():
		return errors.Join(failure, ctx.Err())
	}
}

func (c *Client) sessionFSAdapter(sessionID string, provider SessionFSProvider) *sessionFSAdapter {
	adapter := newSessionFSAdapter(provider)
	adapter.beginCallback = func() func() {
		return c.beginSessionCallback(sessionID)
	}
	return adapter
}

func (c *Client) createSessionFSProvider(session *Session, factory func(*Session) SessionFSProvider) SessionFSProvider {
	defer c.beginSessionCallback(session.SessionID)()
	return factory(session)
}

func (c *Client) beginSessionCallback(sessionID string) func() {
	c.sessionOperationsMux.Lock()
	if c.activeSessionCallbacks == nil {
		c.activeSessionCallbacks = make(map[string]int)
	}
	c.activeSessionCallbacks[sessionID]++
	c.sessionOperationsMux.Unlock()
	return func() {
		c.sessionOperationsMux.Lock()
		c.activeSessionCallbacks[sessionID]--
		if c.activeSessionCallbacks[sessionID] == 0 {
			delete(c.activeSessionCallbacks, sessionID)
		}
		c.sessionOperationsMux.Unlock()
	}
}

func (c *Client) lockSessionOperation(ctx context.Context, sessionID string) (func(), error) {
	return c.acquireSessionOperation(ctx, sessionID, true)
}

func (c *Client) acquireSessionOperation(ctx context.Context, sessionID string, wait bool) (func(), error) {
	c.sessionOperationsMux.Lock()
	if err := ctx.Err(); err != nil {
		c.sessionOperationsMux.Unlock()
		return nil, err
	}
	keys := []string{}
	for id := range c.sessionOperations {
		if sessionIDsOverlap(id, sessionID) {
			keys = append(keys, id)
		}
	}
	if c.sessionOperations == nil {
		c.sessionOperations = make(map[string]*sessionOperation)
	}
	operation := c.sessionOperations[sessionID]
	if operation == nil {
		operation = &sessionOperation{permit: make(chan struct{}, 1)}
		operation.permit <- struct{}{}
		c.sessionOperations[sessionID] = operation
		keys = append(keys, sessionID)
	}
	// A short prefix can overlap several independent full IDs. Acquire every
	// matching permit in a stable order instead of merging their live owners.
	sort.Strings(keys)
	operations := make([]*sessionOperation, len(keys))
	for i, id := range keys {
		operations[i] = c.sessionOperations[id]
		operations[i].users++
	}
	acquired := 0
tryPermits:
	for _, operation := range operations {
		select {
		case <-operation.permit:
			acquired++
		default:
			break tryPermits
		}
	}
	activeCallback := false
	for _, id := range keys[acquired:] {
		activeCallback = activeCallback || c.hasOverlappingSessionCallback(id)
	}
	if activeCallback {
		for _, operation := range operations[:acquired] {
			operation.permit <- struct{}{}
		}
		for i, operation := range operations {
			operation.users--
			if operation.users == 0 {
				delete(c.sessionOperations, keys[i])
			}
		}
		c.sessionOperationsMux.Unlock()
		// Reject only a wait that could depend on this callback or provider factory.
		return nil, fmt.Errorf("cannot operate on session %s during an active session callback", sessionID)
	}
	c.sessionOperationsMux.Unlock()

	releaseReference := func() {
		c.sessionOperationsMux.Lock()
		for i, operation := range operations {
			operation.users--
			if operation.users == 0 {
				delete(c.sessionOperations, keys[i])
			}
		}
		c.sessionOperationsMux.Unlock()
	}
	release := func() {
		for _, operation := range operations[:acquired] {
			operation.permit <- struct{}{}
		}
	}
	if !wait && acquired != len(operations) {
		release()
		releaseReference()
		return nil, fmt.Errorf("cannot initialize server-assigned session %s while another session operation is active", sessionID)
	}
	for _, operation := range operations[acquired:] {
		select {
		case <-ctx.Done():
			release()
			releaseReference()
			return nil, ctx.Err()
		case <-operation.permit:
			acquired++
		}
	}
	return func() {
		release()
		releaseReference()
	}, nil
}

func sessionIDsOverlap(left, right string) bool {
	if left == right {
		return true
	}
	if !isUUIDSessionID(left) || !isUUIDSessionID(right) {
		return false
	}
	left, right = strings.ToLower(left), strings.ToLower(right)
	return strings.HasPrefix(left, right) || strings.HasPrefix(right, left)
}

func isUUIDSessionID(sessionID string) bool {
	return isAbbreviatedUUIDPrefix(sessionID) || len(sessionID) == 36 && uuid.Validate(sessionID) == nil
}

func isAbbreviatedUUIDPrefix(sessionID string) bool {
	if len(sessionID) < 7 || len(sessionID) >= 36 {
		return false
	}
	for i := range len(sessionID) {
		character := sessionID[i]
		if character >= '0' && character <= '9' || character >= 'a' && character <= 'f' || character >= 'A' && character <= 'F' {
			continue
		}
		if i >= 7 && character == '-' {
			continue
		}
		return false
	}
	return true
}

// hasOverlappingSessionCallback requires sessionOperationsMux.
func (c *Client) hasOverlappingSessionCallback(sessionID string) bool {
	for id, active := range c.activeSessionCallbacks {
		if active > 0 && sessionIDsOverlap(id, sessionID) {
			return true
		}
	}
	return false
}

func (c *Client) residentSessionID(sessionID string) string {
	if !isUUIDSessionID(sessionID) {
		return sessionID
	}
	c.sessionsMux.Lock()
	defer c.sessionsMux.Unlock()
	if c.sessions[sessionID] != nil {
		return sessionID
	}
	matches := make(map[string]bool)
	for id := range c.sessions {
		if sessionIDsOverlap(id, sessionID) && len(id) >= len(sessionID) {
			matches[id] = true
		}
	}
	for _, cleanups := range c.autoModeRecoveryCleanups {
		for id := range cleanups {
			if sessionIDsOverlap(id, sessionID) && len(id) >= len(sessionID) {
				matches[id] = true
			}
		}
	}
	if len(matches) == 1 {
		for id := range matches {
			return id
		}
	}
	return sessionID
}

type uiThreadAdapter struct {
	host          inProcessHost
	runOnUiThread func(action func() error) error
}

func (a *uiThreadAdapter) Run(request *rpc.UIThreadRunRequest) (*rpc.UIThreadRunResult, error) {
	if a.runOnUiThread == nil || a.host == nil {
		return nil, fmt.Errorf("no in-process RunOnUiThread handler is configured")
	}
	if request.ActionID < 0 || uint64(request.ActionID) > 0xffffffff {
		return nil, fmt.Errorf("UI action ID must be an unsigned 32-bit integer")
	}
	err := a.runOnUiThread(func() error {
		return a.host.RunUiAction(uint32(request.ActionID))
	})
	return nil, err
}

type gitHubTokenAdapter struct {
	client *Client
}

func (a *gitHubTokenAdapter) GetToken(request *rpc.GitHubTokenAcquireRequest) (rpc.GitHubTokenAcquireResult, error) {
	if request == nil {
		return nil, fmt.Errorf("missing GitHub token acquire request")
	}
	a.client.gitHubTokenProvidersMux.RLock()
	provider := a.client.gitHubTokenProviders[request.RegistrationID]
	a.client.gitHubTokenProvidersMux.RUnlock()
	if provider == nil {
		return nil, fmt.Errorf("unknown GitHub token provider registration ID %q", request.RegistrationID)
	}

	result, err := provider(GitHubTokenProviderArgs{
		Host:      request.Host,
		SessionID: request.SessionID,
		Reason:    request.Reason,
	})
	if err != nil {
		return nil, err
	}
	if result != nil && result.Cancelled {
		return &rpc.GitHubTokenAcquireResultCancelled{}, nil
	}
	if result == nil || result.Token == nil {
		return nil, fmt.Errorf("GitHub token provider returned neither a token nor cancellation")
	}
	return &rpc.GitHubTokenAcquireResultToken{
		AccessToken: result.Token.AccessToken,
		TokenType:   result.Token.TokenType,
		ExpiresIn:   result.Token.ExpiresIn,
	}, nil
}

// gitHubTelemetryAdapter adapts the OnGitHubTelemetry option to the generated
// rpc.GitHubTelemetryHandler interface.
type gitHubTelemetryAdapter struct {
	callback func(notification *rpc.GitHubTelemetryNotification)
}

func (a *gitHubTelemetryAdapter) Event(request *rpc.GitHubTelemetryNotification) error {
	defer func() { recover() }() // Ignore handler panics
	a.callback(request)
	return nil
}

func (c *Client) handleSessionEvent(req sessionEventRequest) {
	if req.SessionID == "" {
		return
	}
	// Dispatch to session
	c.sessionsMux.Lock()
	session, ok := c.sessions[req.SessionID]
	c.sessionsMux.Unlock()

	if ok {
		session.dispatchEventWithPermissionEnvelope(req.Event, req.permissionEnvelope, req.sdkHasHandler)
	}
}

func (c *Client) handleSkillProviderList(ctx context.Context, params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
	var req rpc.SkillProviderListRequest
	if err := json.Unmarshal(params, &req); err != nil || req.SessionID == "" {
		return nil, &jsonrpc2.Error{Code: -32602, Message: "invalid skill provider list payload"}
	}

	provider, rpcErr := c.resolveSkillProvider(req.SessionID)
	if rpcErr != nil {
		return nil, rpcErr
	}

	skills, err := callSkillProvider(ctx, req.SessionID, "listSkills", func() ([]rpc.SkillProviderDescriptor, error) {
		return provider.ListSkills(ctx)
	})
	if err != nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: "Skill provider listSkills failed"}
	}
	if skills == nil {
		skills = []rpc.SkillProviderDescriptor{}
	}

	raw, err := json.Marshal(rpc.SkillProviderListResult{Skills: skills})
	if err != nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: fmt.Sprintf("Failed to marshal response: %v", err)}
	}
	return raw, nil
}

func (c *Client) handleSkillProviderRead(ctx context.Context, params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
	var req rpc.SkillProviderReadRequest
	if err := json.Unmarshal(params, &req); err != nil || req.SessionID == "" || req.Name == "" {
		return nil, &jsonrpc2.Error{Code: -32602, Message: "invalid skill provider read payload"}
	}

	provider, rpcErr := c.resolveSkillProvider(req.SessionID)
	if rpcErr != nil {
		return nil, rpcErr
	}

	// Classify not-found inside the guarded call: errors.Is runs provider
	// Is/Unwrap methods, which may panic.
	markdown, err := callSkillProvider(ctx, req.SessionID, "readSkill", func() (*string, error) {
		markdown, err := provider.ReadSkill(ctx, req.Name)
		if errors.Is(err, ErrSkillNotFound) {
			return nil, nil
		}
		if err != nil {
			return nil, err
		}
		return &markdown, nil
	})
	if err != nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: "Skill provider readSkill failed"}
	}

	raw, err := json.Marshal(rpc.SkillProviderReadResult{Markdown: markdown})
	if err != nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: fmt.Sprintf("Failed to marshal response: %v", err)}
	}
	return raw, nil
}

func (c *Client) handleMemoryStore(ctx context.Context, params json.RawMessage) (json.RawMessage, *jsonrpc2.Error) {
	var req rpc.MemoryStoreRequest
	if err := json.Unmarshal(params, &req); err != nil || req.SessionID == "" {
		return nil, &jsonrpc2.Error{Code: -32602, Message: "invalid memory store payload"}
	}

	c.sessionsMux.Lock()
	session := c.sessions[req.SessionID]
	c.sessionsMux.Unlock()
	if session == nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: "Memory store owner is unavailable"}
	}
	store := session.getMemoryStore()
	if store == nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: "No memory store is registered"}
	}
	// $/cancelRequest can be read before this goroutine runs; never write after the runtime gave up.
	if ctx.Err() != nil {
		return nil, &jsonrpc2.Error{Code: -32800, Message: "Request cancelled"}
	}

	if err := callMemoryStore(ctx, store, StoredMemory{
		SessionID: req.SessionID,
		Subject:   req.Subject,
		Fact:      req.Fact,
		Citations: req.Citations,
		Reason:    req.Reason,
	}); err != nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: "Memory store callback failed"}
	}
	return json.RawMessage("null"), nil
}

// callMemoryStore logs callback failures locally and converts panics into an
// error, so the runtime only ever receives a generic failure.
func callMemoryStore(ctx context.Context, store func(context.Context, StoredMemory) error, memory StoredMemory) (err error) {
	defer func() {
		if failure := recover(); failure != nil {
			log.Printf("memory store panicked: session_id=%s panic=%v\n%s", memory.SessionID, failure, debug.Stack())
			err = errors.New("memory store panicked")
		}
	}()
	err = store(ctx, memory)
	if err != nil && ctx.Err() == nil {
		log.Printf("memory store failed: session_id=%s error=%v", memory.SessionID, err)
	}
	return err
}

func (c *Client) resolveSkillProvider(sessionID string) (SkillProvider, *jsonrpc2.Error) {
	c.sessionsMux.Lock()
	session := c.sessions[sessionID]
	c.sessionsMux.Unlock()
	if session == nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: fmt.Sprintf("No skill provider for session: %s", sessionID)}
	}
	provider := session.getSkillProvider()
	if provider == nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: fmt.Sprintf("No skill provider for session: %s", sessionID)}
	}
	return provider, nil
}

var errSkillProviderPanic = errors.New("skill provider panicked")

// callSkillProvider logs provider failures locally and converts panics into
// an error, so the runtime only ever receives a generic failure.
func callSkillProvider[T any](ctx context.Context, sessionID, operation string, call func() (T, error)) (result T, err error) {
	defer func() {
		if failure := recover(); failure != nil {
			log.Printf("skill provider %s panicked: session_id=%s panic=%v\n%s", operation, sessionID, failure, debug.Stack())
			var zero T
			result, err = zero, errSkillProviderPanic
		}
	}()
	result, err = call()
	// A call the runtime cancelled is expected to fail; it isn't a provider failure.
	if err != nil && ctx.Err() == nil {
		log.Printf("skill provider %s failed: session_id=%s error=%v", operation, sessionID, err)
	}
	return result, err
}

// handleUserInputRequest handles a user input request from the CLI server.
func (c *Client) handleUserInputRequest(req userInputRequest) (*userInputResponse, *jsonrpc2.Error) {
	if req.SessionID == "" || req.Question == "" {
		return nil, &jsonrpc2.Error{Code: -32602, Message: "invalid user input request payload"}
	}

	c.sessionsMux.Lock()
	session, ok := c.sessions[req.SessionID]
	c.sessionsMux.Unlock()
	if !ok {
		return nil, &jsonrpc2.Error{Code: -32602, Message: fmt.Sprintf("unknown session %s", req.SessionID)}
	}

	response, err := session.handleUserInputRequest(UserInputRequest{
		Question:      req.Question,
		Choices:       req.Choices,
		AllowFreeform: req.AllowFreeform,
		RequestID:     req.RequestID,
	})
	if err != nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: err.Error()}
	}

	return &userInputResponse{
		Answer:          response.Answer,
		WasFreeform:     response.WasFreeform,
		DecisionContext: response.DecisionContext,
	}, nil
}

// shellAuthenticationKey identifies an open secret prompt in Client.pendingShellAuthenticationRequests,
// which tracks them on the client rather than on a session, because a resume can
// replace the session object while a prompt is open.
type shellAuthenticationKey struct {
	sessionID string
	requestID string
}

// shellAuthenticationTicket is one shellAuthentication.request, registered before the read
// loop reads the next message.
type shellAuthenticationTicket struct {
	ctx     context.Context
	cancel  context.CancelFunc
	claimed bool
}

// prepareShellAuthenticationRequest registers a shellAuthentication.request in read order.
// Each call runs in its own goroutine, so a shellAuthentication.cancel or connection
// close that follows the request on the wire could otherwise run before it is
// registered and miss it. It never fails the request: one it can't register,
// including one without the program that asked or its prompt, is answered cancel
// by its handler.
func (c *Client) prepareShellAuthenticationRequest(raw json.RawMessage) *jsonrpc2.Error {
	var req shellAuthenticationRequest
	if json.Unmarshal(raw, &req) != nil || req.SessionID == "" || req.RequestID == "" || !namesShellAuthenticationPrompt(raw) {
		return nil
	}
	key := shellAuthenticationKey{sessionID: req.SessionID, requestID: req.RequestID}
	ctx, cancel := context.WithCancel(context.Background())
	c.pendingShellAuthenticationRequestsMux.Lock()
	defer c.pendingShellAuthenticationRequestsMux.Unlock()
	if c.pendingShellAuthenticationRequests == nil {
		c.pendingShellAuthenticationRequests = make(map[shellAuthenticationKey][]*shellAuthenticationTicket)
	}
	if len(c.pendingShellAuthenticationRequests[key]) > 0 {
		// A request for a prompt that is already open is answered cancel.
		cancel()
	}
	c.pendingShellAuthenticationRequests[key] = append(c.pendingShellAuthenticationRequests[key], &shellAuthenticationTicket{ctx: ctx, cancel: cancel})
	return nil
}

// namesShellAuthenticationPrompt reports whether a shellAuthentication.request names the
// program that asked and its prompt as strings. Either may be empty, since sudo
// accepts an empty -p prompt, but a request without them is malformed.
func namesShellAuthenticationPrompt(raw json.RawMessage) bool {
	var fields struct {
		Program *string `json:"program"`
		Prompt  *string `json:"prompt"`
	}
	return json.Unmarshal(raw, &fields) == nil && fields.Program != nil && fields.Prompt != nil
}

// claimShellAuthenticationTicket takes a ticket that prepareShellAuthenticationRequest
// registered for key, or returns nil when there is none left, for example
// because the connection closed first.
func (c *Client) claimShellAuthenticationTicket(key shellAuthenticationKey) *shellAuthenticationTicket {
	c.pendingShellAuthenticationRequestsMux.Lock()
	defer c.pendingShellAuthenticationRequestsMux.Unlock()
	for _, ticket := range c.pendingShellAuthenticationRequests[key] {
		if !ticket.claimed {
			ticket.claimed = true
			return ticket
		}
	}
	return nil
}

// releaseShellAuthenticationTicket removes a finished request's ticket.
func (c *Client) releaseShellAuthenticationTicket(key shellAuthenticationKey, ticket *shellAuthenticationTicket) {
	ticket.cancel()
	c.pendingShellAuthenticationRequestsMux.Lock()
	defer c.pendingShellAuthenticationRequestsMux.Unlock()
	tickets := c.pendingShellAuthenticationRequests[key]
	for i, candidate := range tickets {
		if candidate == ticket {
			tickets = append(tickets[:i:i], tickets[i+1:]...)
			break
		}
	}
	if len(tickets) == 0 {
		delete(c.pendingShellAuthenticationRequests, key)
	} else {
		c.pendingShellAuthenticationRequests[key] = tickets
	}
}

// handleShellAuthenticationRequest handles a shellAuthentication.request from the runtime: a
// shell command the agent runs asks for a password or another answer. Anything
// that isn't the handler's answer cancels the prompt.
func (c *Client) handleShellAuthenticationRequest(req shellAuthenticationRequest) (*ShellAuthenticationResponse, *jsonrpc2.Error) {
	cancelled := &ShellAuthenticationResponse{Action: ShellAuthenticationActionCancel}
	key := shellAuthenticationKey{sessionID: req.SessionID, requestID: req.RequestID}
	ticket := c.claimShellAuthenticationTicket(key)
	if ticket == nil {
		return cancelled, nil
	}
	defer c.releaseShellAuthenticationTicket(key, ticket)
	kind := ShellAuthenticationInputKind(req.InputKind)
	if kind != ShellAuthenticationInputKindSecret && kind != ShellAuthenticationInputKindText && kind != ShellAuthenticationInputKindConfirm {
		return cancelled, nil
	}
	c.sessionsMux.Lock()
	session := c.sessions[req.SessionID]
	c.sessionsMux.Unlock()
	if session == nil {
		return cancelled, nil
	}

	response := session.handleShellAuthenticationRequest(ticket.ctx, ShellAuthenticationRequest{
		RequestID: req.RequestID,
		Program:   req.Program,
		Prompt:    req.Prompt,
		InputKind: kind,
		Command:   req.Command,
		Cwd:       req.Cwd,
	})
	return &response, nil
}

// handleShellAuthenticationCancel handles a shellAuthentication.cancel from the runtime: the
// answer to an open prompt is no longer needed.
func (c *Client) handleShellAuthenticationCancel(req shellAuthenticationCancelRequest) (*struct{}, *jsonrpc2.Error) {
	c.pendingShellAuthenticationRequestsMux.Lock()
	tickets := c.pendingShellAuthenticationRequests[shellAuthenticationKey{sessionID: req.SessionID, requestID: req.RequestID}]
	for _, ticket := range tickets {
		ticket.cancel()
	}
	c.pendingShellAuthenticationRequestsMux.Unlock()
	return &struct{}{}, nil
}

// cancelPendingShellAuthenticationRequests closes every open secret prompt once the runtime
// can no longer use an answer.
func (c *Client) cancelPendingShellAuthenticationRequests() {
	c.pendingShellAuthenticationRequestsMux.Lock()
	pending := c.pendingShellAuthenticationRequests
	c.pendingShellAuthenticationRequests = nil
	c.pendingShellAuthenticationRequestsMux.Unlock()
	for _, tickets := range pending {
		for _, ticket := range tickets {
			ticket.cancel()
		}
	}
}

// handleExitPlanModeRequest handles an exitPlanMode.request callback from the CLI server.
// The reply is the handler's result with decisionContext beside its fields when set.
func (c *Client) handleExitPlanModeRequest(req exitPlanModeRequest) (*AttributedExitPlanModeResult, *jsonrpc2.Error) {
	if req.SessionID == "" {
		return nil, &jsonrpc2.Error{Code: -32602, Message: "invalid exit plan mode request payload"}
	}
	recommendedAction := req.RecommendedAction
	if recommendedAction == "" {
		recommendedAction = "autopilot"
	}

	c.sessionsMux.Lock()
	session, ok := c.sessions[req.SessionID]
	c.sessionsMux.Unlock()
	if !ok {
		return nil, &jsonrpc2.Error{Code: -32602, Message: fmt.Sprintf("unknown session %s", req.SessionID)}
	}

	response, err := session.handleExitPlanModeRequest(ExitPlanModeRequest{
		Summary:           req.Summary,
		PlanContent:       req.PlanContent,
		Actions:           req.Actions,
		RecommendedAction: recommendedAction,
		RequestID:         req.RequestID,
	})
	if err != nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: err.Error()}
	}

	return &response, nil
}

// handleAutoModeSwitchRequest handles an autoModeSwitch.request callback from the CLI server.
func (c *Client) handleAutoModeSwitchRequest(req autoModeSwitchRequest) (*autoModeSwitchResponse, *jsonrpc2.Error) {
	if req.SessionID == "" {
		return nil, &jsonrpc2.Error{Code: -32602, Message: "invalid auto mode switch request payload"}
	}

	c.sessionsMux.Lock()
	session, ok := c.sessions[req.SessionID]
	c.sessionsMux.Unlock()
	if !ok {
		return nil, &jsonrpc2.Error{Code: -32602, Message: fmt.Sprintf("unknown session %s", req.SessionID)}
	}

	response, err := session.handleAutoModeSwitchRequest(AutoModeSwitchRequest{
		ErrorCode:         req.ErrorCode,
		RetryAfterSeconds: req.RetryAfterSeconds,
		AutoModeRecovery:  req.AutoModeRecovery,
	})
	if err != nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: err.Error()}
	}

	return &autoModeSwitchResponse{Response: response}, nil
}

// handleHooksInvoke routes a hook callback to its owning session, keyed by the
// payload's sessionId.
func (c *Client) handleHooksInvoke(req hooksInvokeRequest) (map[string]any, *jsonrpc2.Error) {
	if req.SessionID == "" || req.Type == "" {
		return nil, &jsonrpc2.Error{Code: -32602, Message: "invalid hooks invoke payload"}
	}

	c.sessionsMux.Lock()
	session, ok := c.sessions[req.SessionID]
	c.sessionsMux.Unlock()
	if !ok {
		return nil, &jsonrpc2.Error{Code: -32602, Message: fmt.Sprintf("unknown session %s", req.SessionID)}
	}

	output, err := session.handleHooksInvoke(req.Type, req.Input)
	if err != nil {
		return nil, &jsonrpc2.Error{Code: -32603, Message: err.Error()}
	}

	result := make(map[string]any)
	if output != nil {
		result["output"] = output
	}
	return result, nil
}

// hooksAdapter implements the generated rpc.HooksHandler, delegating to the
// client's per-session hook dispatcher.
type hooksAdapter struct {
	client *Client
}

func (a *hooksAdapter) Invoke(request *rpc.HookInvokeRequest) (*rpc.HookInvokeResponse, error) {
	rawInput, err := json.Marshal(request.Input)
	if err != nil {
		return nil, &jsonrpc2.Error{Code: -32602, Message: fmt.Sprintf("invalid hooks invoke payload: %v", err)}
	}

	result, rpcErr := a.client.handleHooksInvoke(hooksInvokeRequest{
		SessionID: request.SessionID,
		Type:      string(request.HookType),
		Input:     rawInput,
	})
	if rpcErr != nil {
		return nil, rpcErr
	}

	response := &rpc.HookInvokeResponse{}
	if result != nil {
		response.Output = result["output"]
	}
	return response, nil
}

// handleSystemMessageTransform handles a system message transform request from the CLI server.
func (c *Client) handleSystemMessageTransform(req systemMessageTransformRequest) (systemMessageTransformResponse, *jsonrpc2.Error) {
	if req.SessionID == "" {
		return systemMessageTransformResponse{}, &jsonrpc2.Error{Code: -32602, Message: "invalid system message transform payload"}
	}

	c.sessionsMux.Lock()
	session, ok := c.sessions[req.SessionID]
	c.sessionsMux.Unlock()
	if !ok {
		return systemMessageTransformResponse{}, &jsonrpc2.Error{Code: -32602, Message: fmt.Sprintf("unknown session %s", req.SessionID)}
	}

	resp, err := session.handleSystemMessageTransform(req.Sections)
	if err != nil {
		return systemMessageTransformResponse{}, &jsonrpc2.Error{Code: -32603, Message: err.Error()}
	}
	return resp, nil
}
