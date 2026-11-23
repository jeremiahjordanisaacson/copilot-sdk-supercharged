/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#include "copilot/client.h"
#include "copilot/sdk_protocol_version.h"

#include <algorithm>
#include <atomic>
#include <cstdio>
#include <cstdlib>
#include <iterator>
#include <random>
#include <sstream>
#include <stdexcept>
#include <utility>
#include <vector>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#else
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

namespace copilot {

namespace {

/// Generates a random UUID-like (version 4) identifier for GitHub token provider registrations.
std::string generateRegistrationId() {
    static std::mutex mtx;
    static std::mt19937_64 gen{std::random_device{}()};
    std::uniform_int_distribution<uint64_t> dist;

    uint64_t a = 0;
    uint64_t b = 0;
    {
        std::lock_guard<std::mutex> lock(mtx);
        a = dist(gen);
        b = dist(gen);
    }

    char buf[40];
    std::snprintf(buf, sizeof(buf), "%08x-%04x-4%03x-%04x-%012llx",
                  static_cast<unsigned>(a >> 32),
                  static_cast<unsigned>((a >> 16) & 0xFFFFu),
                  static_cast<unsigned>(a & 0x0FFFu),
                  static_cast<unsigned>(((b >> 48) & 0x3FFFu) | 0x8000u),
                  static_cast<unsigned long long>(b & 0xFFFFFFFFFFFFULL));
    return std::string(buf);
}

/// Adds the session options shared by session.create and session.resume.
template <typename Config>
void appendSharedSessionParams(nlohmann::json& params, const Config& config) {
    if (config.modelCapabilities) params["modelCapabilities"] = *config.modelCapabilities;

    // CAPI-specific options live under `capi` (github.capi in protocol 4).
    nlohmann::json capi = nlohmann::json::object();
    if (config.capi && config.capi->enabled) capi["enabled"] = *config.capi->enabled;
    if (config.autoTier) capi["autoTier"] = *config.autoTier;
    if (config.enableWebSocketResponses) capi["enableWebSocketResponses"] = *config.enableWebSocketResponses;
    if (!capi.empty()) params["capi"] = capi;

    if (config.askUserVariant) params["askUserVariant"] = *config.askUserVariant;
    if (config.sandboxConfig) params["sandboxConfig"] = *config.sandboxConfig;
    if (config.featureFlags) params["featureFlags"] = *config.featureFlags;
    if (config.allowedModels) params["allowedModels"] = *config.allowedModels;
    if (config.authClientIdMetadataUrl) params["authClientIdMetadataUrl"] = *config.authClientIdMetadataUrl;
    if (config.agentDirectories) params["agentDirectories"] = *config.agentDirectories;
    if (config.hookDirectories) params["hookDirectories"] = *config.hookDirectories;
    if (config.interactiveUser) params["interactiveUser"] = *config.interactiveUser;
    if (config.excludedManagedSettings) params["excludedManagedSettings"] = *config.excludedManagedSettings;
    if (config.enforceManagedModelDefaults) params["enforceManagedModelDefaults"] = *config.enforceManagedModelDefaults;
    if (config.imageGeneration) params["imageGeneration"] = *config.imageGeneration;
    if (config.autopilot) params["autopilot"] = *config.autopilot;

    if (config.includeSubAgentStreamingEvents) params["includeSubAgentStreamingEvents"] = *config.includeSubAgentStreamingEvents;
    if (config.enableConfigDiscovery) params["enableConfigDiscovery"] = *config.enableConfigDiscovery;
    if (config.gitHubToken) params["gitHubToken"] = *config.gitHubToken;
    if (config.instructionDirectories) params["instructionDirectories"] = *config.instructionDirectories;

    // --- v3.0.0 options (SDK protocol 4) ---
    if (config.providers) params["providers"] = *config.providers;
    if (config.models) params["models"] = *config.models;
    if (config.defaultProvider) params["defaultProvider"] = *config.defaultProvider;
    if (config.permissionMode) params["permissionMode"] = *config.permissionMode;
    if (config.onShellAuthenticationRequest) params["requestShellAuthentication"] = true;
    if (config.storeMemory) params["hasMemoryStore"] = true;
}

/// True when the CLI path is a JavaScript entry point (npm-loader.js, legacy index.js).
bool isJavaScriptPath(const std::string& path) {
    return path.size() > 3 && (path.compare(path.size() - 3, 3, ".js") == 0 ||
                               path.compare(path.size() - 3, 3, ".JS") == 0);
}

// ----------------------------------------------------------------------------
// SDK protocol 4 grouped session payloads
// ----------------------------------------------------------------------------

/// Lifecycle methods that accept a key. Keys sent to the wrong method are dropped.
enum class ParamScope { Both, Create, Resume };

/// Maps a flat session.create/resume key to its protocol 4 location (group nullptr = root).
struct SessionParamRule {
    const char* flatKey;
    const char* group;
    const char* member;
    ParamScope scope;
};

const SessionParamRule kSessionParamRules[] = {
    // Root fields (configDir and disableResume are renamed)
    {"sessionId", nullptr, "sessionId", ParamScope::Both},
    {"model", nullptr, "model", ParamScope::Both},
    {"name", nullptr, "name", ParamScope::Create},
    {"clientName", nullptr, "clientName", ParamScope::Both},
    {"workingDirectory", nullptr, "workingDirectory", ParamScope::Both},
    {"configDirectory", nullptr, "configDirectory", ParamScope::Both},
    {"configDir", nullptr, "configDirectory", ParamScope::Both},
    {"enableConfigDiscovery", nullptr, "enableConfigDiscovery", ParamScope::Both},
    {"systemMessage", nullptr, "systemMessage", ParamScope::Both},
    {"diagnostics", nullptr, "diagnostics", ParamScope::Both},
    {"lifecycleDiagnostics", nullptr, "lifecycleDiagnostics", ParamScope::Both},
    {"cloud", nullptr, "cloud", ParamScope::Create},
    {"traceparent", nullptr, "traceparent", ParamScope::Both},
    {"tracestate", nullptr, "tracestate", ParamScope::Both},
    {"suppressResumeEvent", nullptr, "suppressResumeEvent", ParamScope::Resume},
    {"disableResume", nullptr, "suppressResumeEvent", ParamScope::Resume},
    {"continuePendingWork", nullptr, "continuePendingWork", ParamScope::Resume},
    {"allowTranscriptRecovery", nullptr, "allowTranscriptRecovery", ParamScope::Resume},
    {"requestedEnvironmentVariables", nullptr, "requestedEnvironmentVariables", ParamScope::Resume},
    // inference
    {"reasoningEffort", "inference", "reasoningEffort", ParamScope::Both},
    {"reasoningSummary", "inference", "reasoningSummary", ParamScope::Both},
    {"contextTier", "inference", "contextTier", ParamScope::Both},
    {"allowMultipleToolCalls", "inference", "allowMultipleToolCalls", ParamScope::Both},
    {"enableCitations", "inference", "enableCitations", ParamScope::Both},
    {"modelCapabilities", "inference", "capabilities", ParamScope::Both},
    {"verbosity", "inference", "verbosity", ParamScope::Both},
    {"providers", "inference", "providers", ParamScope::Both},
    {"models", "inference", "models", ParamScope::Both},
    {"defaultProvider", "inference", "defaultProvider", ParamScope::Both},
    // tools
    {"tools", "tools", "definitions", ParamScope::Both},
    {"availableTools", "tools", "availableTools", ParamScope::Both},
    {"excludedTools", "tools", "excludedTools", ParamScope::Both},
    {"toolSearch", "tools", "search", ParamScope::Both},
    {"largeOutput", "tools", "largeOutput", ParamScope::Both},
    {"imageGeneration", "tools", "imageGeneration", ParamScope::Both},
    {"manageScheduleEnabled", "tools", "manageScheduleEnabled", ParamScope::Both},
    {"webSearch", "tools", "webSearch", ParamScope::Both},
    {"shell", "tools", "shell", ParamScope::Both},
    {"maxInlineBinaryBytes", "tools", "maxInlineBinaryBytes", ParamScope::Both},
    // permissions
    {"requestPermission", "permissions", "request", ParamScope::Both},
    {"sandboxConfig", "permissions", "sandbox", ParamScope::Both},
    {"sandbox", "permissions", "sandbox", ParamScope::Both},
    {"permissionMode", "permissions", "mode", ParamScope::Both},
    {"sandboxConfigSource", "permissions", "sandboxSource", ParamScope::Both},
    // interaction
    {"commands", "interaction", "commands", ParamScope::Both},
    {"askUserVariant", "interaction", "askUserVariant", ParamScope::Both},
    {"requestUserInput", "interaction", "requestUserInput", ParamScope::Both},
    {"requestElicitation", "interaction", "requestElicitation", ParamScope::Both},
    {"requestShellAuthentication", "interaction", "requestShellAuthentication", ParamScope::Both},
    {"requestExitPlanMode", "interaction", "requestExitPlanMode", ParamScope::Both},
    {"requestAutoModeSwitch", "interaction", "requestAutoModeSwitch", ParamScope::Both},
    {"requestAutoModeRecovery", "interaction", "requestAutoModeRecovery", ParamScope::Both},
    // events
    {"streaming", "events", "streaming", ParamScope::Both},
    {"includeSubAgentStreamingEvents", "events", "includeSubAgentStreamingEvents", ParamScope::Both},
    // agents
    {"customAgents", "agents", "definitions", ParamScope::Both},
    {"defaultAgent", "agents", "default", ParamScope::Both},
    {"agent", "agents", "selected", ParamScope::Both},
    {"includedBuiltinAgents", "agents", "includedBuiltinAgents", ParamScope::Both},
    {"excludedBuiltinAgents", "agents", "excludedBuiltinAgents", ParamScope::Both},
    {"customAgentsLocalOnly", "agents", "localDiscoveryOnly", ParamScope::Both},
    {"agentDirectories", "agents", "directories", ParamScope::Both},
    // skills
    {"skillDirectories", "skills", "directories", ParamScope::Both},
    {"enableSkills", "skills", "enabled", ParamScope::Both},
    {"includedBuiltinSkills", "skills", "includedBuiltinSkills", ParamScope::Both},
    {"disabledSkills", "skills", "disabledSkills", ParamScope::Both},
    {"hasSkillProvider", "skills", "hasProvider", ParamScope::Both},
    {"ignoredSkillsLocations", "skills", "ignoredLocations", ParamScope::Both},
    // mcp
    {"mcpServers", "mcp", "servers", ParamScope::Both},
    {"disabledMcpServers", "mcp", "disabledServers", ParamScope::Both},
    {"requestMcpApps", "mcp", "enableApps", ParamScope::Both},
    {"enableMcpApps", "mcp", "enableApps", ParamScope::Both},
    {"mcpOAuthTokenStorage", "mcp", "oauthTokenStorage", ParamScope::Both},
    {"authClientIdMetadataUrl", "mcp", "authClientIdMetadataUrl", ParamScope::Both},
    {"envValueMode", "mcp", "envValueMode", ParamScope::Both},
    {"managedMcpServers", "mcp", "managedServers", ParamScope::Both},
    {"allowAllMcpServerInstructions", "mcp", "allowAllServerInstructions", ParamScope::Both},
    // github
    {"gitHubToken", "github", "token", ParamScope::Both},
    {"gitHubTokenProviderRegistrationId", "github", "tokenProviderRegistrationId", ParamScope::Both},
    {"githubMcpToolConfig", "github", "mcp", ParamScope::Both},
    {"capi", "github", "capi", ParamScope::Both},
    {"enableSessionTelemetry", "github", "telemetryEnabled", ParamScope::Both},
    {"enableManagedSettings", "github", "fetchManagedSettings", ParamScope::Both},
    {"interactiveUser", "github", "interactiveUser", ParamScope::Both},
    {"enableGitHubTelemetryForwarding", "github", "telemetryForwarding", ParamScope::Both},
    {"authInfo", "github", "authInfo", ParamScope::Both},
    {"copilotUrl", "github", "copilotUrl", ParamScope::Both},
    // filesystem
    {"additionalDirectories", "filesystem", "additionalDirectories", ParamScope::Both},
    {"enableFileChangeTracking", "filesystem", "trackFileChanges", ParamScope::Both},
    {"trustWorkingDirectory", "filesystem", "trustWorkingDirectory", ParamScope::Create},
    // execution
    {"sessionLimits", "execution", "limits", ParamScope::Both},
    {"autopilot", "execution", "autopilot", ParamScope::Both},
    {"autopilotContinuation", "execution", "autopilotContinuation", ParamScope::Both},
    // instructions
    {"instructionDirectories", "instructions", "directories", ParamScope::Both},
    {"organizationCustomInstructions", "instructions", "organizationText", ParamScope::Both},
    {"enableOnDemandInstructionDiscovery", "instructions", "onDemandDiscovery", ParamScope::Both},
    {"skipCustomInstructions", "instructions", "skipCustomInstructions", ParamScope::Both},
    {"refreshCustomInstructions", "instructions", "refreshCustomInstructions", ParamScope::Create},
    // hooks (boolean "hooks" becomes hooks.hasHandlers)
    {"hooks", "hooks", "hasHandlers", ParamScope::Both},
    {"hookDirectories", "hooks", "directories", ParamScope::Both},
    {"enableFileHooks", "hooks", "enableRepositoryHooks", ParamScope::Both},
    // git
    {"coauthorEnabled", "git", "coauthorEnabled", ParamScope::Both},
    {"enableHostGitOperations", "git", "enableHostGitOperations", ParamScope::Both},
    // state
    {"infiniteSessions", "state", "infiniteSessions", ParamScope::Both},
    {"memory", "state", "memory", ParamScope::Both},
    {"hasMemoryStore", "state", "hasMemoryStore", ParamScope::Both},
    {"enableSessionStore", "state", "enableCrossSessionStore", ParamScope::Both},
    {"skipEmbeddingRetrieval", "state", "skipEmbeddingRetrieval", ParamScope::Both},
    {"embeddingCacheStorage", "state", "embeddingCacheStorage", ParamScope::Both},
    // plugins
    {"pluginDirectories", "plugins", "directories", ParamScope::Both},
    {"installedPlugins", "plugins", "installed", ParamScope::Both},
    // canvases
    {"canvases", "canvases", "definitions", ParamScope::Both},
    {"canvasProvider", "canvases", "provider", ParamScope::Both},
    {"requestCanvasRenderer", "canvases", "requestRenderer", ParamScope::Both},
    {"openCanvases", "canvases", "open", ParamScope::Resume},
    // extensions
    {"requestExtensions", "extensions", "enabled", ParamScope::Both},
    {"extensionSdkPath", "extensions", "sdkPath", ParamScope::Both},
    {"extensionInfo", "extensions", "identity", ParamScope::Both},
    // experiments
    {"enableExperimentalMode", "experiments", "enableExperimentalMode", ParamScope::Both},
    {"isExperimentalMode", "experiments", "enableExperimentalMode", ParamScope::Both},
    {"featureFlags", "experiments", "flags", ParamScope::Both},
    {"expAssignments", "experiments", "assignments", ParamScope::Both},
    {"featureFlagConfiguration", "experiments", "configuration", ParamScope::Both},
    // policy
    {"allowedModels", "policy", "allowedModels", ParamScope::Both},
    {"managedSettings", "policy", "managedSettings", ParamScope::Both},
    {"excludedManagedSettings", "policy", "excludedManagedSettings", ParamScope::Both},
    {"enforceManagedModelDefaults", "policy", "enforceManagedModelDefaults", ParamScope::Both},
};

/// Protocol 4 option groups. A key naming a group that holds an object is already grouped.
const char* const kSessionParamGroups[] = {
    "inference", "tools", "permissions", "interaction", "events", "agents", "skills",
    "mcp", "github", "filesystem", "execution", "instructions", "hooks", "git",
    "state", "plugins", "canvases", "extensions", "experiments", "policy",
};

/// Model-specific fields of the legacy single provider (moved to inference.models[]).
const char* const kLegacyProviderModelFields[] = {
    "modelId", "wireModel", "maxPromptTokens", "maxContextWindowTokens", "maxOutputTokens",
    "capabilities",
};

const SessionParamRule* findSessionParamRule(const std::string& key) {
    for (const auto& rule : kSessionParamRules) {
        if (key == rule.flatKey) return &rule;
    }
    return nullptr;
}

template <std::size_t N>
bool containsName(const char* const (&names)[N], const std::string& key) {
    return std::find_if(std::begin(names), std::end(names),
                        [&key](const char* name) { return key == name; }) != std::end(names);
}

/// Returns out[name] as an object, creating it (or replacing a non-object) when needed.
nlohmann::json& childObject(nlohmann::json& out, const std::string& name) {
    nlohmann::json& obj = out[name];
    if (!obj.is_object()) obj = nlohmann::json::object();
    return obj;
}

/// Stores value at out[group][member] unless an explicit grouped member already exists.
void putGroupMember(nlohmann::json& out, const char* group, const char* member, nlohmann::json value) {
    nlohmann::json& obj = childObject(out, group);
    if (!obj.contains(member)) obj[member] = std::move(value);
}

/// Translates the legacy single `provider` (no longer accepted by protocol 4) into
/// inference.providers [{name:"default", ...connection}], inference.defaultProvider
/// "default" and github.capi.enabled false, plus inference.models when model-specific
/// fields are present and the session model is set. Explicit providers,
/// defaultProvider, or capi.enabled win: the legacy provider is then dropped.
void translateLegacyProvider(nlohmann::json& out, const nlohmann::json& provider) {
    const bool explicitRouting =
        (out.contains("inference") && (out["inference"].contains("providers") ||
                                       out["inference"].contains("defaultProvider"))) ||
        (out.contains("github") && out["github"].contains("capi") &&
         out["github"]["capi"].is_object() && out["github"]["capi"].contains("enabled"));
    if (explicitRouting) return;

    nlohmann::json connection = {{"name", "default"}};
    nlohmann::json modelFields = nlohmann::json::object();
    for (auto it = provider.begin(); it != provider.end(); ++it) {
        if (it.value().is_null() || it.key() == "name") continue;
        if (containsName(kLegacyProviderModelFields, it.key())) {
            modelFields[it.key()] = it.value();
        } else {
            connection[it.key()] = it.value();
        }
    }

    nlohmann::json& inference = childObject(out, "inference");
    nlohmann::json providers = nlohmann::json::array();
    providers.push_back(std::move(connection));
    inference["providers"] = std::move(providers);
    inference["defaultProvider"] = "default";
    childObject(childObject(out, "github"), "capi")["enabled"] = false;

    if (!modelFields.empty() && out.contains("model") && out["model"].is_string()) {
        nlohmann::json definition = {{"provider", "default"}, {"id", out["model"]}};
        for (auto it = modelFields.begin(); it != modelFields.end(); ++it) {
            definition[it.key()] = it.value();
        }
        nlohmann::json& models = inference["models"];
        if (!models.is_array()) models = nlohmann::json::array();
        models.push_back(std::move(definition));
    }
}

/// Converts the flat session.create/session.resume payload into the SDK protocol 4
/// grouped payload: root keys stay at the top level, every known flat key moves to
/// its option group, unknown keys stay at the root (forward compatibility), null
/// values are skipped (except defaultProvider: null, which clears it on resume),
/// explicit grouped members win over flat ones, and empty groups are not emitted.
nlohmann::json groupSessionParams(const nlohmann::json& flat, bool isResume) {
    nlohmann::json out = nlohmann::json::object();
    if (!flat.is_object()) return out;

    const nlohmann::json* legacyProvider = nullptr;
    std::vector<std::pair<std::string, const nlohmann::json*>> grouped;
    for (auto it = flat.begin(); it != flat.end(); ++it) {
        const std::string& key = it.key();
        const nlohmann::json& value = it.value();
        if (key == "provider") {
            legacyProvider = &value;
            continue;
        }
        if (value.is_object() && containsName(kSessionParamGroups, key)) {
            grouped.emplace_back(key, &value);
            continue;
        }
        if (value.is_null() && !(isResume && key == "defaultProvider")) continue;

        const SessionParamRule* rule = findSessionParamRule(key);
        if (!rule) {
            out[key] = value;
        } else if ((rule->scope == ParamScope::Create && isResume) ||
                   (rule->scope == ParamScope::Resume && !isResume)) {
            continue;
        } else if (!rule->group) {
            out[rule->member] = value;
        } else {
            putGroupMember(out, rule->group, rule->member, value);
        }
    }

    // Already-grouped objects are merged last so their members win.
    for (const auto& [group, members] : grouped) {
        nlohmann::json& obj = childObject(out, group);
        for (auto it = members->begin(); it != members->end(); ++it) {
            obj[it.key()] = it.value();
        }
    }

    // MCP server definitions use direct environment values.
    if (out.contains("mcp") && out["mcp"].is_object() && out["mcp"].contains("servers")) {
        putGroupMember(out, "mcp", "envValueMode", "direct");
    }

    if (legacyProvider && legacyProvider->is_object()) {
        translateLegacyProvider(out, *legacyProvider);
    }

    for (const char* group : kSessionParamGroups) {
        auto it = out.find(group);
        if (it != out.end() && it->is_object() && it->empty()) out.erase(it);
    }
    return out;
}

/// Client mode "empty": minimal defaults applied underneath the caller's own values.
void applyEmptyModeDefaults(nlohmann::json& params) {
    putGroupMember(params, "experiments", "enableExperimentalMode", false);
    putGroupMember(params, "github", "telemetryEnabled", false);
    putGroupMember(params, "mcp", "oauthTokenStorage", "in-memory");
    putGroupMember(params, "state", "skipEmbeddingRetrieval", true);
    putGroupMember(params, "state", "embeddingCacheStorage", "in-memory");
    putGroupMember(params, "state", "enableCrossSessionStore", false);
    putGroupMember(params, "state", "memory", nlohmann::json::object({{"enabled", false}}));
    putGroupMember(params, "instructions", "onDemandDiscovery", false);
    putGroupMember(params, "hooks", "enableRepositoryHooks", false);
    putGroupMember(params, "git", "enableHostGitOperations", false);
}

} // namespace

// ============================================================================
// Construction / Destruction
// ============================================================================

CopilotClient::CopilotClient(const CopilotClientOptions& options)
    : options_(options) {
    // Validate mutually exclusive options
    if (options.cliUrl && (options.useStdio || !options.cliPath.empty())) {
        // Only throw if cliPath was explicitly set to something non-default
        // and cliUrl is also set
        if (options.cliUrl) {
            // Parse the URL now
            // Supports: "host:port", "http://host:port", "port"
            std::string url = *options.cliUrl;
            // Remove protocol
            if (url.find("https://") == 0) url = url.substr(8);
            else if (url.find("http://") == 0) url = url.substr(7);

            isExternalServer_ = true;
            options_.useStdio = false;
        }
    }

    // Check environment variable for CLI path
    const char* envPath = std::getenv("COPILOT_CLI_PATH");
    if (envPath && options_.cliPath == "copilot") {
        options_.cliPath = envPath;
    }

    // Client mode: "coding-agent" (default; legacy alias "copilot-cli") or "empty"
    if (options_.mode == "copilot-cli") {
        options_.mode = ClientMode::CodingAgent;
    }
    if (options_.mode != ClientMode::CodingAgent && options_.mode != ClientMode::Empty) {
        throw std::invalid_argument("CopilotClientOptions::mode must be 'coding-agent' or 'empty'");
    }
}

CopilotClient::~CopilotClient() {
    try {
        forceStop();
    } catch (...) {
        // Suppress all exceptions in destructor
    }
}

// ============================================================================
// Start / Stop
// ============================================================================

void CopilotClient::start() {
    if (state_ == ConnectionState::Connected) return;

    state_ = ConnectionState::Connecting;

    try {
        if (!isExternalServer_) {
            startCLIServer();
        }
        connectToServer();
        sendConnectHandshake();
        verifyProtocolVersion();
        registerNetworkProvider();
        state_ = ConnectionState::Connected;
    } catch (...) {
        state_ = ConnectionState::Error;
        throw;
    }
}

std::vector<std::string> CopilotClient::stop() {
    std::vector<std::string> errors;

    // Destroy all active sessions
    std::vector<std::shared_ptr<CopilotSession>> sessionList;
    {
        std::lock_guard<std::mutex> lock(sessionsMutex_);
        for (auto& [id, session] : sessions_) {
            sessionList.push_back(session);
        }
    }

    for (auto& session : sessionList) {
        try {
            session->destroy();
        } catch (const std::exception& e) {
            errors.push_back("Failed to destroy session " + session->sessionId + ": " + e.what());
        }
    }

    {
        std::lock_guard<std::mutex> lock(sessionsMutex_);
        sessions_.clear();
    }

    clearGitHubTokenProviders();

    // Stop JSON-RPC client
    if (rpcClient_) {
        rpcClient_->stop();
        rpcClient_.reset();
    }

    // Clear models cache
    {
        std::lock_guard<std::mutex> lock(modelsCacheMutex_);
        modelsCache_.reset();
    }

    // Kill CLI process (only if we spawned it)
    if (!isExternalServer_) {
#ifdef _WIN32
        if (processHandle_) {
            TerminateProcess(processHandle_, 0);
            CloseHandle(processHandle_);
            processHandle_ = nullptr;
        }
        if (stdinWrite_) { CloseHandle(stdinWrite_); stdinWrite_ = nullptr; }
        if (stdoutRead_) { CloseHandle(stdoutRead_); stdoutRead_ = nullptr; }
#else
        if (processPid_ > 0) {
            kill(processPid_, SIGTERM);
            int status;
            waitpid(processPid_, &status, 0);
            processPid_ = -1;
        }
        if (stdinWriteFd_ >= 0) { close(stdinWriteFd_); stdinWriteFd_ = -1; }
        if (stdoutReadFd_ >= 0) { close(stdoutReadFd_); stdoutReadFd_ = -1; }
#endif
    }

    state_ = ConnectionState::Disconnected;
    return errors;
}

void CopilotClient::forceStop() {
    // Clear sessions immediately
    {
        std::lock_guard<std::mutex> lock(sessionsMutex_);
        sessions_.clear();
    }

    clearGitHubTokenProviders();

    // Stop JSON-RPC client
    if (rpcClient_) {
        rpcClient_->stop();
        rpcClient_.reset();
    }

    // Clear models cache
    {
        std::lock_guard<std::mutex> lock(modelsCacheMutex_);
        modelsCache_.reset();
    }

    // Kill process
    if (!isExternalServer_) {
#ifdef _WIN32
        if (processHandle_) {
            TerminateProcess(processHandle_, 0);
            CloseHandle(processHandle_);
            processHandle_ = nullptr;
        }
        if (stdinWrite_) { CloseHandle(stdinWrite_); stdinWrite_ = nullptr; }
        if (stdoutRead_) { CloseHandle(stdoutRead_); stdoutRead_ = nullptr; }
#else
        if (processPid_ > 0) {
            kill(processPid_, SIGKILL);
            int status;
            waitpid(processPid_, &status, WNOHANG);
            processPid_ = -1;
        }
        if (stdinWriteFd_ >= 0) { close(stdinWriteFd_); stdinWriteFd_ = -1; }
        if (stdoutReadFd_ >= 0) { close(stdoutReadFd_); stdoutReadFd_ = -1; }
#endif
    }

    state_ = ConnectionState::Disconnected;
}

// ============================================================================
// Session Management
// ============================================================================

void CopilotClient::ensureConnected() {
    if (rpcClient_) return;
    if (options_.autoStart) {
        start();
    } else {
        throw std::runtime_error("Client not connected. Call start() first.");
    }
}

std::shared_ptr<CopilotSession> CopilotClient::createSession(const SessionConfig& config) {
    if (config.gitHubToken && config.gitHubTokenProvider) {
        throw std::invalid_argument("gitHubToken and gitHubTokenProvider are mutually exclusive");
    }
    if (options_.mode == ClientMode::Empty && !config.availableTools) {
        throw std::invalid_argument(
            "CopilotClient is in mode 'empty' but the session config did not specify availableTools");
    }

    ensureConnected();

    auto params = buildCreateSessionParams(config);

    std::string gitHubTokenRegistrationId;
    if (config.gitHubTokenProvider) {
        gitHubTokenRegistrationId =
            registerGitHubTokenProvider(config.gitHubTokenProvider, config.sessionId.value_or(""));
        params["gitHubTokenProviderRegistrationId"] = gitHubTokenRegistrationId;
    }

    // SDK protocol 4: move the flat options into their option groups.
    params = groupSessionParams(params, false);
    if (options_.mode == ClientMode::Empty) {
        applyEmptyModeDefaults(params);
    }

    nlohmann::json result;
    try {
        result = rpcClient_->request("session.create", params);
    } catch (...) {
        if (!gitHubTokenRegistrationId.empty()) {
            unregisterGitHubTokenProvider(gitHubTokenRegistrationId);
        }
        throw;
    }

    std::string sid = result.value("sessionId", "");
    std::string wp = result.value("workspacePath", "");

    auto session = std::shared_ptr<CopilotSession>(
        new CopilotSession(sid, rpcClient_.get(), wp));

    if (!gitHubTokenRegistrationId.empty()) {
        assignGitHubTokenProviderSession(gitHubTokenRegistrationId, sid);
        session->onDisconnected_ = [this, gitHubTokenRegistrationId]() {
            unregisterGitHubTokenProvider(gitHubTokenRegistrationId);
        };
    }

    session->registerTools(config.tools);
    if (config.onPermissionRequest) {
        session->registerPermissionHandler(config.onPermissionRequest);
    }
    if (config.onUserInputRequest) {
        session->registerUserInputHandler(config.onUserInputRequest);
    }
    if (config.hooks && config.hooks->hasAny()) {
        session->registerHooks(*config.hooks);
    }
    if (config.onExitPlanMode) {
        session->registerExitPlanModeHandler(config.onExitPlanMode);
    }
    if (config.onShellAuthenticationRequest) {
        session->registerShellAuthenticationHandler(config.onShellAuthenticationRequest);
    }
    if (config.storeMemory) {
        session->registerMemoryStoreHandler(config.storeMemory);
    }

    {
        std::lock_guard<std::mutex> lock(sessionsMutex_);
        sessions_[sid] = session;
    }

    return session;
}

std::shared_ptr<CopilotSession> CopilotClient::resumeSession(
    const std::string& sessionId, const ResumeSessionConfig& config) {
    if (config.gitHubToken && config.gitHubTokenProvider) {
        throw std::invalid_argument("gitHubToken and gitHubTokenProvider are mutually exclusive");
    }
    if (options_.mode == ClientMode::Empty && !config.availableTools) {
        throw std::invalid_argument(
            "CopilotClient is in mode 'empty' but the session config did not specify availableTools");
    }

    ensureConnected();

    auto params = buildResumeSessionParams(sessionId, config);

    std::string gitHubTokenRegistrationId;
    if (config.gitHubTokenProvider) {
        gitHubTokenRegistrationId = registerGitHubTokenProvider(config.gitHubTokenProvider, sessionId);
        params["gitHubTokenProviderRegistrationId"] = gitHubTokenRegistrationId;
    }

    // SDK protocol 4: move the flat options into their option groups.
    params = groupSessionParams(params, true);
    if (options_.mode == ClientMode::Empty) {
        applyEmptyModeDefaults(params);
    }

    nlohmann::json result;
    try {
        result = rpcClient_->request("session.resume", params);
    } catch (...) {
        if (!gitHubTokenRegistrationId.empty()) {
            unregisterGitHubTokenProvider(gitHubTokenRegistrationId);
        }
        throw;
    }

    std::string sid = result.value("sessionId", "");
    std::string wp = result.value("workspacePath", "");

    auto session = std::shared_ptr<CopilotSession>(
        new CopilotSession(sid, rpcClient_.get(), wp));

    if (!gitHubTokenRegistrationId.empty()) {
        assignGitHubTokenProviderSession(gitHubTokenRegistrationId, sid.empty() ? sessionId : sid);
        session->onDisconnected_ = [this, gitHubTokenRegistrationId]() {
            unregisterGitHubTokenProvider(gitHubTokenRegistrationId);
        };
    }

    session->registerTools(config.tools);
    if (config.onPermissionRequest) {
        session->registerPermissionHandler(config.onPermissionRequest);
    }
    if (config.onUserInputRequest) {
        session->registerUserInputHandler(config.onUserInputRequest);
    }
    if (config.hooks && config.hooks->hasAny()) {
        session->registerHooks(*config.hooks);
    }
    if (config.onExitPlanMode) {
        session->registerExitPlanModeHandler(config.onExitPlanMode);
    }
    if (config.onShellAuthenticationRequest) {
        session->registerShellAuthenticationHandler(config.onShellAuthenticationRequest);
    }
    if (config.storeMemory) {
        session->registerMemoryStoreHandler(config.storeMemory);
    }

    {
        std::lock_guard<std::mutex> lock(sessionsMutex_);
        sessions_[sid] = session;
    }

    return session;
}

ConnectionState CopilotClient::getState() const {
    return state_;
}

// ============================================================================
// Ping / Status / Models
// ============================================================================

PingResponse CopilotClient::ping(const std::string& message) {
    if (!rpcClient_) throw std::runtime_error("Client not connected");
    auto result = rpcClient_->request("ping", {{"message", message}});
    return result.get<PingResponse>();
}

GetStatusResponse CopilotClient::getStatus() {
    if (!rpcClient_) throw std::runtime_error("Client not connected");
    auto result = rpcClient_->request("status.get", nlohmann::json::object());
    return result.get<GetStatusResponse>();
}

GetAuthStatusResponse CopilotClient::getAuthStatus() {
    if (!rpcClient_) throw std::runtime_error("Client not connected");
    auto result = rpcClient_->request("auth.getStatus", nlohmann::json::object());
    return result.get<GetAuthStatusResponse>();
}

std::vector<ModelInfo> CopilotClient::listModels() {
    if (!rpcClient_) throw std::runtime_error("Client not connected");

    std::lock_guard<std::mutex> lock(modelsCacheMutex_);
    if (modelsCache_) {
        return *modelsCache_;
    }

    auto result = rpcClient_->request("models.list", nlohmann::json::object());
    std::vector<ModelInfo> models;
    if (result.contains("models")) {
        models = result["models"].get<std::vector<ModelInfo>>();
    }
    modelsCache_ = models;
    return models;
}

std::optional<std::string> CopilotClient::getLastSessionId() {
    if (!rpcClient_) throw std::runtime_error("Client not connected");
    auto result = rpcClient_->request("session.getLastId", nlohmann::json::object());
    if (result.contains("sessionId") && !result["sessionId"].is_null()) {
        return result["sessionId"].get<std::string>();
    }
    return std::nullopt;
}

nlohmann::json CopilotClient::getSessionMetadata(const std::string& sessionId) {
    if (!rpcClient_) throw std::runtime_error("Client not connected");
    return rpcClient_->request("session.getMetadata", {{"sessionId", sessionId}});
}

void CopilotClient::deleteSession(const std::string& sessionId) {
    if (!rpcClient_) throw std::runtime_error("Client not connected");
    auto result = rpcClient_->request("session.delete", {{"sessionId", sessionId}});
    bool success = result.value("success", false);
    if (!success) {
        std::string error = result.value("error", "Unknown error");
        throw std::runtime_error("Failed to delete session " + sessionId + ": " + error);
    }
    std::lock_guard<std::mutex> lock(sessionsMutex_);
    sessions_.erase(sessionId);
}

std::vector<SessionMetadata> CopilotClient::listSessions() {
    if (!rpcClient_) throw std::runtime_error("Client not connected");
    auto result = rpcClient_->request("session.list", nlohmann::json::object());
    std::vector<SessionMetadata> sessions;
    if (result.contains("sessions")) {
        sessions = result["sessions"].get<std::vector<SessionMetadata>>();
    }
    return sessions;
}

std::optional<std::string> CopilotClient::getForegroundSessionId() {
    if (!rpcClient_) throw std::runtime_error("Client not connected");
    auto result = rpcClient_->request("session.getForeground", nlohmann::json::object());
    if (result.contains("sessionId") && !result["sessionId"].is_null()) {
        return result["sessionId"].get<std::string>();
    }
    return std::nullopt;
}

void CopilotClient::setForegroundSessionId(const std::string& sessionId) {
    if (!rpcClient_) throw std::runtime_error("Client not connected");
    auto result = rpcClient_->request("session.setForeground", {{"sessionId", sessionId}});
    bool success = result.value("success", false);
    if (!success) {
        std::string error = result.value("error", "Failed to set foreground session");
        throw std::runtime_error(error);
    }
}

// ============================================================================
// Session Filesystem Provider
// ============================================================================

void CopilotClient::setSessionFsProvider(const SessionFsConfig& config) {
    ensureConnected();

    nlohmann::json params = nlohmann::json::object();
    if (!config.initialCwd.empty()) {
        params["initialCwd"] = config.initialCwd;
    }
    if (!config.sessionStatePath.empty()) {
        params["sessionStatePath"] = config.sessionStatePath;
    }
    if (!config.conventions.empty()) {
        params["conventions"] = config.conventions;
    }

    rpcClient_->request("sessionFs.setProvider", params);
}

// ============================================================================
// Managed Settings
// ============================================================================

void CopilotClient::clearManagedSettingsCache() {
    if (!rpcClient_) throw std::runtime_error("Client not connected");
    rpcClient_->request("managedSettings.clearCache", nlohmann::json::object());
}

// ============================================================================
// Lifecycle Event Subscriptions
// ============================================================================

std::function<void()> CopilotClient::onLifecycle(SessionLifecycleHandler handler) {
    std::lock_guard<std::mutex> lock(lifecycleMutex_);
    uint64_t id = nextLifecycleId_++;
    lifecycleHandlers_.push_back({id, "", std::move(handler)});
    return [this, id]() {
        std::lock_guard<std::mutex> lock(lifecycleMutex_);
        lifecycleHandlers_.erase(
            std::remove_if(lifecycleHandlers_.begin(), lifecycleHandlers_.end(),
                [id](const LifecycleEntry& e) { return e.id == id; }),
            lifecycleHandlers_.end());
    };
}

std::function<void()> CopilotClient::onLifecycle(const std::string& eventType,
                                                   SessionLifecycleHandler handler) {
    std::lock_guard<std::mutex> lock(lifecycleMutex_);
    uint64_t id = nextLifecycleId_++;
    lifecycleHandlers_.push_back({id, eventType, std::move(handler)});
    return [this, id]() {
        std::lock_guard<std::mutex> lock(lifecycleMutex_);
        lifecycleHandlers_.erase(
            std::remove_if(lifecycleHandlers_.begin(), lifecycleHandlers_.end(),
                [id](const LifecycleEntry& e) { return e.id == id; }),
            lifecycleHandlers_.end());
    };
}

// ============================================================================
// Protocol Version Verification
// ============================================================================

void CopilotClient::sendConnectHandshake() {
    if (!options_.clientInfo) return;

    // Map the public identity onto the connect wire shape, dropping empty fields.
    const auto& info = *options_.clientInfo;
    nlohmann::json wire = nlohmann::json::object();
    if (info.applicationName && !info.applicationName->empty()) wire["editorName"] = *info.applicationName;
    if (info.applicationVersion && !info.applicationVersion->empty()) wire["editorVersion"] = *info.applicationVersion;
    if (info.integrationName && !info.integrationName->empty()) wire["extensionName"] = *info.integrationName;
    if (info.integrationVersion && !info.integrationVersion->empty()) wire["extensionVersion"] = *info.integrationVersion;
    if (wire.empty()) return;

    try {
        rpcClient_->request("connect", {{"clientInfo", wire}});
    } catch (const std::exception& e) {
        // Older CLIs without `connect` keep their default attribution; the ping
        // handshake that follows still verifies the protocol version.
        if (!isMethodNotFoundError(e)) throw;
    }
}

void CopilotClient::verifyProtocolVersion() {
    auto response = ping();
    if (!response.protocolVersion) {
        throw std::runtime_error(
            "SDK protocol version mismatch: SDK supports version " +
            std::to_string(SDK_PROTOCOL_VERSION) +
            ", but server does not report a protocol version. "
            "Please update your SDK or server to ensure compatibility.");
    }
    if (*response.protocolVersion != SDK_PROTOCOL_VERSION) {
        throw std::runtime_error(
            "SDK protocol version mismatch: SDK supports version " +
            std::to_string(SDK_PROTOCOL_VERSION) +
            ", but server reports version " +
            std::to_string(*response.protocolVersion) +
            ". Please update your SDK or server to ensure compatibility.");
    }
}

void CopilotClient::registerNetworkProvider() {
    if (!options_.network) return;
    const auto& network = *options_.network;
    if (!network.resolveProxy && !network.authenticateNegotiate && !network.authenticateBasic) return;

    // Each present member marks one callback this client answers.
    nlohmann::json params = nlohmann::json::object();
    if (network.scope) params["scope"] = *network.scope;
    if (network.resolveProxy) params["resolveProxy"] = nlohmann::json::object();
    if (network.authenticateNegotiate) params["authenticateNegotiate"] = nlohmann::json::object();
    if (network.authenticateBasic) params["authenticateBasic"] = nlohmann::json::object();
    rpcClient_->request("network.setProvider", params);
}

// ============================================================================
// Process Spawning
// ============================================================================

void CopilotClient::startCLIServer() {
    std::vector<std::string> args;

    // JavaScript entry points (npm-loader.js, legacy index.js) run under node.
    std::string program = options_.cliPath;
    if (isJavaScriptPath(options_.cliPath)) {
        program = "node";
        args.push_back(options_.cliPath);
    }

    // Prepend any custom CLI args
    args.insert(args.end(), options_.cliArgs.begin(), options_.cliArgs.end());

    args.push_back("--headless");
    args.push_back("--no-auto-update");
    args.push_back("--log-level");
    args.push_back(options_.logLevel);

    if (options_.useStdio) {
        args.push_back("--stdio");
    } else if (options_.port > 0) {
        args.push_back("--port");
        args.push_back(std::to_string(options_.port));
    }

    // Auth flags
    if (options_.githubToken) {
        args.push_back("--auth-token-env");
        args.push_back("COPILOT_SDK_AUTH_TOKEN");
    }
    bool useLoggedIn = options_.useLoggedInUser.value_or(!options_.githubToken.has_value());
    if (!useLoggedIn) {
        args.push_back("--no-auto-login");
    }

#ifdef _WIN32
    // Windows: CreateProcess with pipes
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = nullptr;

    HANDLE childStdinRead, childStdinWrite;
    HANDLE childStdoutRead, childStdoutWrite;
    HANDLE childStderrRead, childStderrWrite;

    if (!CreatePipe(&childStdinRead, &childStdinWrite, &sa, 0))
        throw std::runtime_error("Failed to create stdin pipe");
    SetHandleInformation(childStdinWrite, HANDLE_FLAG_INHERIT, 0);

    if (!CreatePipe(&childStdoutRead, &childStdoutWrite, &sa, 0))
        throw std::runtime_error("Failed to create stdout pipe");
    SetHandleInformation(childStdoutRead, HANDLE_FLAG_INHERIT, 0);

    if (!CreatePipe(&childStderrRead, &childStderrWrite, &sa, 0))
        throw std::runtime_error("Failed to create stderr pipe");
    SetHandleInformation(childStderrRead, HANDLE_FLAG_INHERIT, 0);

    // Build command line
    std::string cmdLine = "\"" + program + "\"";
    for (const auto& arg : args) {
        cmdLine += " \"" + arg + "\"";
    }

    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    si.hStdInput = childStdinRead;
    si.hStdOutput = childStdoutWrite;
    si.hStdError = childStderrWrite;
    si.dwFlags |= STARTF_USESTDHANDLES;

    ZeroMemory(&pi, sizeof(pi));

    // Set environment with auth token if needed
    char* envBlock = nullptr;
    std::string envString;
    if (options_.githubToken) {
        // Build environment block by copying current env and adding the token
        // For simplicity, we just use the current environment and set the var
        // via _putenv_s before CreateProcess and restore after
        _putenv_s("COPILOT_SDK_AUTH_TOKEN", options_.githubToken->c_str());
    }

    BOOL success = CreateProcessA(
        nullptr,
        const_cast<char*>(cmdLine.c_str()),
        nullptr, nullptr,
        TRUE, // Inherit handles
        0,
        envBlock,
        options_.cwd.empty() ? nullptr : options_.cwd.c_str(),
        &si, &pi);

    // Close child-side handles
    CloseHandle(childStdinRead);
    CloseHandle(childStdoutWrite);
    CloseHandle(childStderrWrite);

    if (!success) {
        CloseHandle(childStdinWrite);
        CloseHandle(childStdoutRead);
        CloseHandle(childStderrRead);
        throw std::runtime_error("Failed to start CLI server: CreateProcess failed");
    }

    CloseHandle(pi.hThread);
    CloseHandle(childStderrRead); // We don't read stderr for now

    processHandle_ = pi.hProcess;
    stdinWrite_ = childStdinWrite;
    stdoutRead_ = childStdoutRead;

#else
    // POSIX: fork + exec with pipes
    int stdinPipe[2];
    int stdoutPipe[2];
    int stderrPipe[2];

    if (pipe(stdinPipe) < 0 || pipe(stdoutPipe) < 0 || pipe(stderrPipe) < 0) {
        throw std::runtime_error("Failed to create pipes");
    }

    pid_t pid = fork();
    if (pid < 0) {
        throw std::runtime_error("Failed to fork");
    }

    if (pid == 0) {
        // Child process
        close(stdinPipe[1]);   // Close write end of stdin
        close(stdoutPipe[0]);  // Close read end of stdout
        close(stderrPipe[0]);  // Close read end of stderr

        dup2(stdinPipe[0], STDIN_FILENO);
        dup2(stdoutPipe[1], STDOUT_FILENO);
        dup2(stderrPipe[1], STDERR_FILENO);

        close(stdinPipe[0]);
        close(stdoutPipe[1]);
        close(stderrPipe[1]);

        if (!options_.cwd.empty()) {
            if (chdir(options_.cwd.c_str()) != 0) {
                _exit(1);
            }
        }

        // Set auth token env var if needed
        if (options_.githubToken) {
            setenv("COPILOT_SDK_AUTH_TOKEN", options_.githubToken->c_str(), 1);
        }

        // Build argv
        std::vector<const char*> argv;
        argv.push_back(program.c_str());
        for (const auto& arg : args) {
            argv.push_back(arg.c_str());
        }
        argv.push_back(nullptr);

        execvp(argv[0], const_cast<char* const*>(argv.data()));
        // If exec fails
        _exit(1);
    }

    // Parent process
    close(stdinPipe[0]);   // Close read end of stdin
    close(stdoutPipe[1]);  // Close write end of stdout
    close(stderrPipe[0]);  // Close read end of stderr (don't read it)
    close(stderrPipe[1]);  // Close write end too

    processPid_ = pid;
    stdinWriteFd_ = stdinPipe[1];
    stdoutReadFd_ = stdoutPipe[0];

#endif
}

void CopilotClient::connectToServer() {
    int readFd, writeFd;

#ifdef _WIN32
    readFd = _open_osfhandle(reinterpret_cast<intptr_t>(stdoutRead_), 0);
    writeFd = _open_osfhandle(reinterpret_cast<intptr_t>(stdinWrite_), 0);
    if (readFd < 0 || writeFd < 0) {
        throw std::runtime_error("Failed to open file descriptors for pipes");
    }
#else
    readFd = stdoutReadFd_;
    writeFd = stdinWriteFd_;
#endif

    rpcClient_ = std::make_unique<JsonRpcClient>(readFd, writeFd);
    setupHandlers();
    rpcClient_->start();
}

// ============================================================================
// Handler Setup
// ============================================================================

void CopilotClient::setupHandlers() {
    // session.event - notification (no response expected)
    rpcClient_->setRequestHandler("session.event",
        [this](const nlohmann::json& params) -> std::pair<nlohmann::json, std::optional<JsonRpcError>> {
            handleSessionEvent(params);
            return {nullptr, std::nullopt};
        });

    // session.lifecycle - notification
    rpcClient_->setRequestHandler("session.lifecycle",
        [this](const nlohmann::json& params) -> std::pair<nlohmann::json, std::optional<JsonRpcError>> {
            handleSessionLifecycle(params);
            return {nullptr, std::nullopt};
        });

    // tool.call - request (needs response)
    rpcClient_->setRequestHandler("tool.call",
        [this](const nlohmann::json& params) { return handleToolCall(params); });

    // permission.request - request
    rpcClient_->setRequestHandler("permission.request",
        [this](const nlohmann::json& params) { return handlePermissionRequest(params); });

    // userInput.request - request
    rpcClient_->setRequestHandler("userInput.request",
        [this](const nlohmann::json& params) { return handleUserInputRequest(params); });

    // hooks.invoke - request
    rpcClient_->setRequestHandler("hooks.invoke",
        [this](const nlohmann::json& params) { return handleHooksInvoke(params); });

    // exitPlanMode.request - request
    rpcClient_->setRequestHandler("exitPlanMode.request",
        [this](const nlohmann::json& params) { return handleExitPlanModeRequest(params); });

    // gitHubToken.getToken - request (session GitHub token providers)
    rpcClient_->setRequestHandler("gitHubToken.getToken",
        [this](const nlohmann::json& params) { return handleGitHubTokenGetToken(params); });

    // shellAuthentication.request / .cancel - shell prompts answered by the session handler
    rpcClient_->setRequestHandler("shellAuthentication.request",
        [this](const nlohmann::json& params) { return handleShellAuthenticationRequest(params); });
    rpcClient_->setRequestHandler("shellAuthentication.cancel",
        [this](const nlohmann::json& params) { return handleShellAuthenticationCancel(params); });

    // memoryStore.store - memories handed to the session's storeMemory callback
    rpcClient_->setRequestHandler("memoryStore.store",
        [this](const nlohmann::json& params) { return handleMemoryStore(params); });

    // network.* - registered only for the callbacks this client answers
    if (options_.network && options_.network->resolveProxy) {
        rpcClient_->setRequestHandler("network.resolveProxy",
            [this](const nlohmann::json& params) { return handleNetworkResolveProxy(params); });
    }
    if (options_.network && options_.network->authenticateNegotiate) {
        rpcClient_->setRequestHandler("network.authenticateNegotiate",
            [this](const nlohmann::json& params) { return handleNetworkAuthenticateNegotiate(params); });
    }
    if (options_.network && options_.network->authenticateBasic) {
        rpcClient_->setRequestHandler("network.authenticateBasic",
            [this](const nlohmann::json& params) { return handleNetworkAuthenticateBasic(params); });
    }
}

std::shared_ptr<CopilotSession> CopilotClient::findSession(const std::string& sessionId) {
    std::lock_guard<std::mutex> lock(sessionsMutex_);
    auto it = sessions_.find(sessionId);
    return it != sessions_.end() ? it->second : nullptr;
}

// ============================================================================
// Server Request Handlers
// ============================================================================

void CopilotClient::handleSessionEvent(const nlohmann::json& params) {
    if (!params.contains("sessionId") || !params.contains("event")) return;

    std::string sid = params["sessionId"].get<std::string>();
    SessionEvent event = params["event"].get<SessionEvent>();

    std::shared_ptr<CopilotSession> session;
    {
        std::lock_guard<std::mutex> lock(sessionsMutex_);
        auto it = sessions_.find(sid);
        if (it != sessions_.end()) session = it->second;
    }

    if (session) {
        session->dispatchEvent(event);
    }
}

void CopilotClient::handleSessionLifecycle(const nlohmann::json& params) {
    if (!params.contains("type") || !params.contains("sessionId")) return;

    SessionLifecycleEvent event = params.get<SessionLifecycleEvent>();

    std::vector<LifecycleEntry> snapshot;
    {
        std::lock_guard<std::mutex> lock(lifecycleMutex_);
        snapshot = lifecycleHandlers_;
    }

    for (const auto& entry : snapshot) {
        if (entry.eventType.empty() || entry.eventType == event.type) {
            try {
                entry.fn(event);
            } catch (...) {}
        }
    }
}

std::pair<nlohmann::json, std::optional<JsonRpcError>>
CopilotClient::handleToolCall(const nlohmann::json& params) {
    std::string sid = params.value("sessionId", "");
    std::string toolCallId = params.value("toolCallId", "");
    std::string toolName = params.value("toolName", "");

    if (sid.empty() || toolCallId.empty() || toolName.empty()) {
        return {nullptr, JsonRpcError{-32602, "Invalid tool call payload"}};
    }

    std::shared_ptr<CopilotSession> session;
    {
        std::lock_guard<std::mutex> lock(sessionsMutex_);
        auto it = sessions_.find(sid);
        if (it != sessions_.end()) session = it->second;
    }

    if (!session) {
        return {nullptr, JsonRpcError{-32602, "Unknown session " + sid}};
    }

    auto handler = session->getToolHandler(toolName);
    if (!handler) {
        nlohmann::json result;
        result["result"] = buildUnsupportedToolResult(toolName);
        return {result, std::nullopt};
    }

    ToolInvocation invocation{sid, toolCallId, toolName,
                              params.contains("arguments") ? params["arguments"] : nlohmann::json::object()};

    auto toolResult = executeToolCall(handler, invocation);
    nlohmann::json response;
    response["result"] = toolResult;
    return {response, std::nullopt};
}

std::pair<nlohmann::json, std::optional<JsonRpcError>>
CopilotClient::handlePermissionRequest(const nlohmann::json& params) {
    std::string sid = params.value("sessionId", "");
    if (sid.empty() || !params.contains("permissionRequest")) {
        return {nullptr, JsonRpcError{-32602, "Invalid permission request payload"}};
    }

    std::shared_ptr<CopilotSession> session;
    {
        std::lock_guard<std::mutex> lock(sessionsMutex_);
        auto it = sessions_.find(sid);
        if (it != sessions_.end()) session = it->second;
    }

    if (!session) {
        return {nullptr, JsonRpcError{-32602, "Session not found: " + sid}};
    }

    try {
        auto permReq = params["permissionRequest"].get<PermissionRequest>();
        auto result = session->handlePermissionRequest(permReq);
        nlohmann::json response;
        response["result"] = result;
        return {response, std::nullopt};
    } catch (...) {
        nlohmann::json response;
        response["result"] = PermissionRequestResult{
            "denied-no-approval-rule-and-could-not-request-from-user"};
        return {response, std::nullopt};
    }
}

std::pair<nlohmann::json, std::optional<JsonRpcError>>
CopilotClient::handleUserInputRequest(const nlohmann::json& params) {
    std::string sid = params.value("sessionId", "");
    std::string question = params.value("question", "");

    if (sid.empty() || question.empty()) {
        return {nullptr, JsonRpcError{-32602, "Invalid user input request payload"}};
    }

    std::shared_ptr<CopilotSession> session;
    {
        std::lock_guard<std::mutex> lock(sessionsMutex_);
        auto it = sessions_.find(sid);
        if (it != sessions_.end()) session = it->second;
    }

    if (!session) {
        return {nullptr, JsonRpcError{-32602, "Session not found: " + sid}};
    }

    try {
        UserInputRequest req;
        req.question = question;
        if (params.contains("choices")) req.choices = params["choices"].get<std::vector<std::string>>();
        if (params.contains("allowFreeform")) req.allowFreeform = params["allowFreeform"].get<bool>();

        auto response = session->handleUserInputRequest(req);
        return {response, std::nullopt};
    } catch (const std::exception& e) {
        return {nullptr, JsonRpcError{-32603, e.what()}};
    }
}

std::pair<nlohmann::json, std::optional<JsonRpcError>>
CopilotClient::handleHooksInvoke(const nlohmann::json& params) {
    std::string sid = params.value("sessionId", "");
    std::string hookType = params.value("hookType", "");

    if (sid.empty() || hookType.empty()) {
        return {nullptr, JsonRpcError{-32602, "Invalid hooks invoke payload"}};
    }

    std::shared_ptr<CopilotSession> session;
    {
        std::lock_guard<std::mutex> lock(sessionsMutex_);
        auto it = sessions_.find(sid);
        if (it != sessions_.end()) session = it->second;
    }

    if (!session) {
        return {nullptr, JsonRpcError{-32602, "Session not found: " + sid}};
    }

    nlohmann::json input = params.contains("input") ? params["input"] : nlohmann::json::object();
    auto output = session->handleHooksInvoke(hookType, input);

    nlohmann::json response;
    if (!output.is_null()) {
        response["output"] = output;
    }
    return {response, std::nullopt};
}

std::pair<nlohmann::json, std::optional<JsonRpcError>>
CopilotClient::handleExitPlanModeRequest(const nlohmann::json& params) {
    std::string sid = params.value("sessionId", "");

    if (sid.empty()) {
        return {nullptr, JsonRpcError{-32602, "Invalid exit plan mode request payload"}};
    }

    std::shared_ptr<CopilotSession> session;
    {
        std::lock_guard<std::mutex> lock(sessionsMutex_);
        auto it = sessions_.find(sid);
        if (it != sessions_.end()) session = it->second;
    }

    if (!session) {
        // Default: approve if session not found
        nlohmann::json response;
        response["approved"] = true;
        return {response, std::nullopt};
    }

    try {
        ExitPlanModeRequest req;
        req.sessionId = sid;
        auto result = session->handleExitPlanModeRequest(req);
        nlohmann::json response;
        response["approved"] = result.approved;
        return {response, std::nullopt};
    } catch (...) {
        nlohmann::json response;
        response["approved"] = true;
        return {response, std::nullopt};
    }
}

std::pair<nlohmann::json, std::optional<JsonRpcError>>
CopilotClient::handleGitHubTokenGetToken(const nlohmann::json& params) {
    std::string registrationId = params.value("registrationId", "");

    GitHubTokenProvider provider;
    std::string registeredSessionId;
    {
        std::lock_guard<std::mutex> lock(gitHubTokenProvidersMutex_);
        auto it = gitHubTokenProviders_.find(registrationId);
        if (it != gitHubTokenProviders_.end()) {
            provider = it->second.provider;
            registeredSessionId = it->second.sessionId;
        }
    }

    if (!provider) {
        return {nullptr, JsonRpcError{-32603,
            "No GitHub token provider registered for registration ID \"" + registrationId + "\""}};
    }

    GitHubTokenProviderArgs args;
    args.host = params.value("host", "");
    args.reason = params.value("reason", "initial");
    if (params.contains("sessionId") && params["sessionId"].is_string()) {
        args.sessionId = params["sessionId"].get<std::string>();
    } else if (!registeredSessionId.empty()) {
        args.sessionId = registeredSessionId;
    }

    try {
        nlohmann::json result = provider(args);
        return {result, std::nullopt};
    } catch (const std::exception& e) {
        return {nullptr, JsonRpcError{-32603, e.what()}};
    }
}

std::pair<nlohmann::json, std::optional<JsonRpcError>>
CopilotClient::handleShellAuthenticationRequest(const nlohmann::json& params) {
    std::string sid = params.value("sessionId", "");
    std::string requestId = params.value("requestId", "");
    if (sid.empty() || requestId.empty()) {
        return {nullptr, JsonRpcError{-32602, "Invalid shellAuthentication.request payload"}};
    }

    auto session = findSession(sid);
    if (!session) {
        return {nlohmann::json{{"action", "cancel"}}, std::nullopt};
    }

    ShellAuthenticationRequest request;
    request.requestId = requestId;
    request.program = params.value("program", "");
    request.prompt = params.value("prompt", "");
    request.inputKind = params.value("inputKind", ShellAuthenticationInputKind::Secret);
    request.command = params.value("command", "");
    request.cwd = params.value("cwd", "");

    // shellAuthentication.cancel flips this flag while the handler runs.
    const std::string key = sid + "\n" + requestId;
    auto cancelled = std::make_shared<std::atomic<bool>>(false);
    {
        std::lock_guard<std::mutex> lock(shellAuthMutex_);
        pendingShellAuth_[key] = cancelled;
    }

    ShellAuthenticationResponse response;
    try {
        response = session->handleShellAuthenticationRequest(
            request, [cancelled]() { return cancelled->load(); });
    } catch (...) {
        response = ShellAuthenticationResponse::cancel();
    }

    {
        std::lock_guard<std::mutex> lock(shellAuthMutex_);
        pendingShellAuth_.erase(key);
    }

    if (cancelled->load() || response.action != "accept") {
        return {nlohmann::json{{"action", "cancel"}}, std::nullopt};
    }
    return {nlohmann::json{{"action", "accept"}, {"value", response.value}}, std::nullopt};
}

std::pair<nlohmann::json, std::optional<JsonRpcError>>
CopilotClient::handleShellAuthenticationCancel(const nlohmann::json& params) {
    const std::string key = params.value("sessionId", "") + "\n" + params.value("requestId", "");
    std::lock_guard<std::mutex> lock(shellAuthMutex_);
    auto it = pendingShellAuth_.find(key);
    if (it != pendingShellAuth_.end()) {
        it->second->store(true);
    }
    return {nlohmann::json::object(), std::nullopt};
}

std::pair<nlohmann::json, std::optional<JsonRpcError>>
CopilotClient::handleMemoryStore(const nlohmann::json& params) {
    std::string sid = params.value("sessionId", "");
    auto session = sid.empty() ? nullptr : findSession(sid);
    if (!session) {
        return {nullptr, JsonRpcError{-32603, "Memory store owner is unavailable"}};
    }

    StoredMemory memory;
    memory.sessionId = sid;
    memory.subject = params.value("subject", "");
    memory.fact = params.value("fact", "");
    memory.citations = params.value("citations", "");
    memory.reason = params.value("reason", "");

    try {
        if (!session->handleMemoryStore(memory)) {
            return {nullptr, JsonRpcError{-32603, "Memory store owner is unavailable"}};
        }
    } catch (const std::exception& e) {
        return {nullptr, JsonRpcError{-32603, std::string("Memory store callback failed: ") + e.what()}};
    }
    return {nullptr, std::nullopt};
}

std::pair<nlohmann::json, std::optional<JsonRpcError>>
CopilotClient::handleNetworkResolveProxy(const nlohmann::json& params) {
    nlohmann::json result = nlohmann::json::object();
    if (!options_.network || !options_.network->resolveProxy) return {result, std::nullopt};
    try {
        auto proxy = options_.network->resolveProxy(params.value("url", ""));
        if (proxy && !proxy->empty()) result["proxy"] = *proxy;
        return {result, std::nullopt};
    } catch (...) {
        return {nullptr, JsonRpcError{-32603, "Network proxy resolution callback failed"}};
    }
}

std::pair<nlohmann::json, std::optional<JsonRpcError>>
CopilotClient::handleNetworkAuthenticateNegotiate(const nlohmann::json& params) {
    nlohmann::json result = nlohmann::json::object();
    if (!options_.network || !options_.network->authenticateNegotiate) return {result, std::nullopt};
    try {
        auto token = options_.network->authenticateNegotiate(params.value("spn", ""));
        if (token && !token->empty()) result["token"] = *token;
        return {result, std::nullopt};
    } catch (...) {
        // Never echo callback details: they may contain credentials.
        return {nullptr, JsonRpcError{-32603, "Network Negotiate authentication callback failed"}};
    }
}

std::pair<nlohmann::json, std::optional<JsonRpcError>>
CopilotClient::handleNetworkAuthenticateBasic(const nlohmann::json& params) {
    nlohmann::json result = nlohmann::json::object();
    if (!options_.network || !options_.network->authenticateBasic) return {result, std::nullopt};

    NetworkBasicAuthRequest request;
    request.proxy = params.value("proxy", "");
    if (params.contains("realm") && params["realm"].is_string()) {
        request.realm = params["realm"].get<std::string>();
    }
    request.previousRejected = params.value("previousRejected", false);

    try {
        auto credentials = options_.network->authenticateBasic(request);
        if (!credentials.username.empty()) {
            result["username"] = credentials.username;
            if (!credentials.password.empty()) result["password"] = credentials.password;
        }
        return {result, std::nullopt};
    } catch (...) {
        // Never echo callback details: they may contain credentials.
        return {nullptr, JsonRpcError{-32603, "Network Basic authentication callback failed"}};
    }
}

// ============================================================================
// GitHub Token Provider Registry
// ============================================================================

std::string CopilotClient::registerGitHubTokenProvider(const GitHubTokenProvider& provider,
                                                       const std::string& sessionId) {
    std::string registrationId = generateRegistrationId();
    std::lock_guard<std::mutex> lock(gitHubTokenProvidersMutex_);
    gitHubTokenProviders_[registrationId] = GitHubTokenRegistration{provider, sessionId};
    return registrationId;
}

void CopilotClient::assignGitHubTokenProviderSession(const std::string& registrationId,
                                                     const std::string& sessionId) {
    std::lock_guard<std::mutex> lock(gitHubTokenProvidersMutex_);
    auto it = gitHubTokenProviders_.find(registrationId);
    if (it != gitHubTokenProviders_.end()) {
        it->second.sessionId = sessionId;
    }
}

void CopilotClient::unregisterGitHubTokenProvider(const std::string& registrationId) {
    std::lock_guard<std::mutex> lock(gitHubTokenProvidersMutex_);
    gitHubTokenProviders_.erase(registrationId);
}

void CopilotClient::clearGitHubTokenProviders() {
    std::lock_guard<std::mutex> lock(gitHubTokenProvidersMutex_);
    gitHubTokenProviders_.clear();
}

// ============================================================================
// Tool Execution
// ============================================================================

ToolResultObject CopilotClient::executeToolCall(ToolHandler handler,
                                                 const ToolInvocation& invocation) {
    try {
        return handler(invocation.arguments, invocation);
    } catch (const std::exception& e) {
        return buildFailedToolResult(e.what());
    } catch (...) {
        return buildFailedToolResult("Unknown tool error");
    }
}

ToolResultObject CopilotClient::buildFailedToolResult(const std::string& error) {
    ToolResultObject result;
    result.textResultForLlm = "Invoking this tool produced an error. Detailed information is not available.";
    result.resultType = "failure";
    result.error = error;
    result.toolTelemetry = nlohmann::json::object();
    return result;
}

ToolResultObject CopilotClient::buildUnsupportedToolResult(const std::string& toolName) {
    ToolResultObject result;
    result.textResultForLlm = "Tool '" + toolName + "' is not supported by this client instance.";
    result.resultType = "failure";
    result.error = "tool '" + toolName + "' not supported";
    result.toolTelemetry = nlohmann::json::object();
    return result;
}

// ============================================================================
// Request Parameter Building
// ============================================================================

nlohmann::json CopilotClient::buildToolsJson(const std::vector<Tool>& tools) {
    nlohmann::json arr = nlohmann::json::array();
    for (const auto& tool : tools) {
        nlohmann::json t = {{"name", tool.name}};
        if (tool.description) t["description"] = *tool.description;
        if (tool.parameters) t["parameters"] = *tool.parameters;
        if (tool.idempotent) t["idempotent"] = *tool.idempotent;
        arr.push_back(t);
    }
    return arr;
}

nlohmann::json CopilotClient::buildCreateSessionParams(const SessionConfig& config) {
    nlohmann::json params = nlohmann::json::object();

    if (config.model) params["model"] = *config.model;
    if (config.sessionId) params["sessionId"] = *config.sessionId;
    if (config.reasoningEffort) params["reasoningEffort"] = *config.reasoningEffort;
    if (config.configDir) params["configDir"] = *config.configDir;
    if (!config.tools.empty()) params["tools"] = buildToolsJson(config.tools);
    if (config.systemMessage) params["systemMessage"] = *config.systemMessage;
    if (config.availableTools) params["availableTools"] = *config.availableTools;
    if (config.excludedTools) params["excludedTools"] = *config.excludedTools;
    if (config.provider) params["provider"] = *config.provider;
    if (config.onPermissionRequest) params["requestPermission"] = true;
    if (config.onUserInputRequest) params["requestUserInput"] = true;
    if (config.hooks && config.hooks->hasAny()) params["hooks"] = true;
    if (config.onExitPlanMode) params["requestExitPlanMode"] = true;
    if (config.workingDirectory) params["workingDirectory"] = *config.workingDirectory;
    if (config.streaming) params["streaming"] = true;
    if (config.mcpServers) params["mcpServers"] = *config.mcpServers;
    if (config.customAgents) params["customAgents"] = *config.customAgents;
    if (config.skillDirectories) params["skillDirectories"] = *config.skillDirectories;
    if (config.disabledSkills) params["disabledSkills"] = *config.disabledSkills;
    if (config.infiniteSessions) params["infiniteSessions"] = *config.infiniteSessions;

    // --- Upstream-sync session options (parity with @github/copilot-sdk) ---
    if (config.enableCitations) params["enableCitations"] = *config.enableCitations;
    if (config.excludedBuiltinAgents) params["excludedBuiltinAgents"] = *config.excludedBuiltinAgents;
    if (config.sessionLimits) params["sessionLimits"] = *config.sessionLimits;
    if (config.memory) params["memory"] = *config.memory;
    if (config.otlpProtocol) params["otlpProtocol"] = *config.otlpProtocol;
    if (config.expAssignments) params["expAssignments"] = *config.expAssignments;
    if (config.onMcpAuthRequest) params["mcpAuthHandler"] = true;

    // --- 2026-08 upstream-sync session options (parity with @github/copilot-sdk) ---
    if (config.rewindEnabled) params["rewindEnabled"] = *config.rewindEnabled;
    if (config.additionalDirectories) params["additionalDirectories"] = *config.additionalDirectories;
    if (config.disabledMcpServers) params["disabledMcpServers"] = *config.disabledMcpServers;
    if (config.githubMcpToolConfig) params["githubMcpToolConfig"] = *config.githubMcpToolConfig;
    if (config.canvasProvider) params["canvasProvider"] = *config.canvasProvider;
    if (config.customAgentsLocalOnly) params["customAgentsLocalOnly"] = *config.customAgentsLocalOnly;
    if (config.toolSearch) params["toolSearch"] = *config.toolSearch;
    if (config.experimentalMode) params["enableExperimentalMode"] = *config.experimentalMode;
    if (config.contentExclusion) params["contentExclusion"] = *config.contentExclusion;

    // --- v2.6.0 upstream-sync session options (parity with @github/copilot-sdk) ---
    appendSharedSessionParams(params, config);
    if (config.name) params["name"] = *config.name;
    if (config.trustWorkingDirectory) params["trustWorkingDirectory"] = *config.trustWorkingDirectory;
    if (config.refreshCustomInstructions) params["refreshCustomInstructions"] = *config.refreshCustomInstructions;

    return params;
}

nlohmann::json CopilotClient::buildResumeSessionParams(const std::string& sessionId,
                                                         const ResumeSessionConfig& config) {
    nlohmann::json params = {{"sessionId", sessionId}};

    if (config.model) params["model"] = *config.model;
    if (config.reasoningEffort) params["reasoningEffort"] = *config.reasoningEffort;
    if (!config.tools.empty()) params["tools"] = buildToolsJson(config.tools);
    if (config.systemMessage) params["systemMessage"] = *config.systemMessage;
    if (config.availableTools) params["availableTools"] = *config.availableTools;
    if (config.excludedTools) params["excludedTools"] = *config.excludedTools;
    if (config.provider) params["provider"] = *config.provider;
    if (config.streaming) params["streaming"] = true;
    if (config.onPermissionRequest) params["requestPermission"] = true;
    if (config.onUserInputRequest) params["requestUserInput"] = true;
    if (config.hooks && config.hooks->hasAny()) params["hooks"] = true;
    if (config.onExitPlanMode) params["requestExitPlanMode"] = true;
    if (config.workingDirectory) params["workingDirectory"] = *config.workingDirectory;
    if (config.configDir) params["configDir"] = *config.configDir;
    if (config.disableResume) params["disableResume"] = true;
    if (config.mcpServers) params["mcpServers"] = *config.mcpServers;
    if (config.customAgents) params["customAgents"] = *config.customAgents;
    if (config.skillDirectories) params["skillDirectories"] = *config.skillDirectories;
    if (config.disabledSkills) params["disabledSkills"] = *config.disabledSkills;
    if (config.infiniteSessions) params["infiniteSessions"] = *config.infiniteSessions;

    // --- v2.6.0 / v3.0.0 session options (parity with @github/copilot-sdk) ---
    appendSharedSessionParams(params, config);
    if (config.allowTranscriptRecovery) params["allowTranscriptRecovery"] = *config.allowTranscriptRecovery;
    if (config.suppressResumeEvent) params["suppressResumeEvent"] = true;
    if (!config.defaultProvider && config.clearDefaultProvider) params["defaultProvider"] = nullptr;

    return params;
}

} // namespace copilot
