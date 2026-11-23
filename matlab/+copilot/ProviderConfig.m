classdef ProviderConfig
    % ProviderConfig  BYOK model provider (wire: inference.providers[]).
    %
    %   p = copilot.ProviderConfig('Name', 'local', 'Type', 'openai', ...
    %       'BaseUrl', 'http://localhost:11434/v1', ...
    %       'ApiKeyCommand', 'cat ~/.keys/local');
    %   cfg = copilot.SessionConfig('Providers', {p}, 'DefaultProvider', 'local');
    %
    %   The same class describes the legacy single provider
    %   (SessionConfig 'Provider'); the SDK sends that one as the named
    %   provider "default".

    properties
        % Stable provider name referenced by ProviderModelConfig.Provider.
        Name (1,:) char = ''

        % Provider type: 'openai', 'azure' or 'anthropic'.
        Type (1,:) char = ''

        % API endpoint URL.
        BaseUrl (1,:) char = ''

        % API key (optional for local providers).
        ApiKey (1,:) char = ''

        % Command whose output is the API key (wire: apiKeyCommand).
        ApiKeyCommand (1,:) char = ''

        % Static bearer token.
        BearerToken (1,:) char = ''

        % Wire API format: 'completions' or 'responses'.
        WireApi (1,:) char = ''

        % Transport: 'http' or 'websockets'.
        Transport (1,:) char = ''

        % Azure options (struct, e.g. struct('apiVersion', '2024-10-21')).
        Azure = []

        % Extra HTTP headers (struct or containers.Map).
        Headers = []

        % Product serving the models, reported in telemetry (e.g. 'ollama').
        ModelProvider (1,:) char = ''

        % Legacy single provider only: model limits (wire: maxOutputTokens, ...).
        MaxPromptTokens (1,1) double = 0
        MaxOutputTokens (1,1) double = 0
    end

    methods
        function obj = ProviderConfig(varargin)
            p = inputParser;
            p.addParameter('Name',            obj.Name);
            p.addParameter('Type',            obj.Type);
            p.addParameter('BaseUrl',         obj.BaseUrl);
            p.addParameter('ApiKey',          obj.ApiKey);
            p.addParameter('ApiKeyCommand',   obj.ApiKeyCommand);
            p.addParameter('BearerToken',     obj.BearerToken);
            p.addParameter('WireApi',         obj.WireApi);
            p.addParameter('Transport',       obj.Transport);
            p.addParameter('Azure',           []);
            p.addParameter('Headers',         []);
            p.addParameter('ModelProvider',   obj.ModelProvider);
            p.addParameter('MaxPromptTokens', obj.MaxPromptTokens);
            p.addParameter('MaxOutputTokens', obj.MaxOutputTokens);
            p.parse(varargin{:});
            obj.Name            = p.Results.Name;
            obj.Type            = p.Results.Type;
            obj.BaseUrl         = p.Results.BaseUrl;
            obj.ApiKey          = p.Results.ApiKey;
            obj.ApiKeyCommand   = p.Results.ApiKeyCommand;
            obj.BearerToken     = p.Results.BearerToken;
            obj.WireApi         = p.Results.WireApi;
            obj.Transport       = p.Results.Transport;
            obj.Azure           = p.Results.Azure;
            obj.Headers         = p.Results.Headers;
            obj.ModelProvider   = p.Results.ModelProvider;
            obj.MaxPromptTokens = p.Results.MaxPromptTokens;
            obj.MaxOutputTokens = p.Results.MaxOutputTokens;
        end

        function s = toStruct(obj)
            %toStruct  Serialize for JSON-RPC (camelCase wire keys).
            s = struct();
            if ~isempty(obj.Name)
                s.name = obj.Name;
            end
            if ~isempty(obj.Type)
                s.type = obj.Type;
            end
            if ~isempty(obj.BaseUrl)
                s.baseUrl = obj.BaseUrl;
            end
            if ~isempty(obj.ApiKey)
                s.apiKey = obj.ApiKey;
            end
            if ~isempty(obj.ApiKeyCommand)
                s.apiKeyCommand = obj.ApiKeyCommand;
            end
            if ~isempty(obj.BearerToken)
                s.bearerToken = obj.BearerToken;
            end
            if ~isempty(obj.WireApi)
                s.wireApi = obj.WireApi;
            end
            if ~isempty(obj.Transport)
                s.transport = obj.Transport;
            end
            if ~isempty(obj.Azure)
                s.azure = obj.Azure;
            end
            if ~isempty(obj.Headers)
                s.headers = obj.Headers;
            end
            if ~isempty(obj.ModelProvider)
                s.modelProvider = obj.ModelProvider;
            end
            if obj.MaxPromptTokens > 0
                s.maxPromptTokens = obj.MaxPromptTokens;
            end
            if obj.MaxOutputTokens > 0
                s.maxOutputTokens = obj.MaxOutputTokens;
            end
        end
    end
end
