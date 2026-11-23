classdef ProviderModelConfig
    % ProviderModelConfig  Model served by a named provider (wire: inference.models[]).
    %
    %   m = copilot.ProviderModelConfig('Provider', 'local', 'Id', 'llama3', ...
    %       'MaxOutputTokens', 2048);
    %   cfg = copilot.SessionConfig('Providers', {p}, 'Models', {m}, ...
    %       'Model', 'local/llama3');
    %
    %   Select a registered model with "provider/id".

    properties
        % Name of the ProviderConfig that serves this model.
        Provider (1,:) char = ''

        % Provider-local model id.
        Id (1,:) char = ''

        % Well-known base model id used for capability lookup.
        ModelId (1,:) char = ''

        % Model name sent to the provider API.
        WireModel (1,:) char = ''

        % Display name for model pickers.
        Name (1,:) char = ''

        % Token limits (0 = not set).
        MaxPromptTokens (1,1) double = 0
        MaxContextWindowTokens (1,1) double = 0
        MaxOutputTokens (1,1) double = 0

        % Capability overrides (struct or []).
        Capabilities = []
    end

    methods
        function obj = ProviderModelConfig(varargin)
            p = inputParser;
            p.addParameter('Provider',               obj.Provider);
            p.addParameter('Id',                     obj.Id);
            p.addParameter('ModelId',                obj.ModelId);
            p.addParameter('WireModel',              obj.WireModel);
            p.addParameter('Name',                   obj.Name);
            p.addParameter('MaxPromptTokens',        obj.MaxPromptTokens);
            p.addParameter('MaxContextWindowTokens', obj.MaxContextWindowTokens);
            p.addParameter('MaxOutputTokens',        obj.MaxOutputTokens);
            p.addParameter('Capabilities',           []);
            p.parse(varargin{:});
            obj.Provider               = p.Results.Provider;
            obj.Id                     = p.Results.Id;
            obj.ModelId                = p.Results.ModelId;
            obj.WireModel              = p.Results.WireModel;
            obj.Name                   = p.Results.Name;
            obj.MaxPromptTokens        = p.Results.MaxPromptTokens;
            obj.MaxContextWindowTokens = p.Results.MaxContextWindowTokens;
            obj.MaxOutputTokens        = p.Results.MaxOutputTokens;
            obj.Capabilities           = p.Results.Capabilities;
        end

        function s = toStruct(obj)
            %toStruct  Serialize for JSON-RPC (camelCase wire keys).
            s = struct('provider', obj.Provider, 'id', obj.Id);
            if ~isempty(obj.ModelId)
                s.modelId = obj.ModelId;
            end
            if ~isempty(obj.WireModel)
                s.wireModel = obj.WireModel;
            end
            if ~isempty(obj.Name)
                s.name = obj.Name;
            end
            if obj.MaxPromptTokens > 0
                s.maxPromptTokens = obj.MaxPromptTokens;
            end
            if obj.MaxContextWindowTokens > 0
                s.maxContextWindowTokens = obj.MaxContextWindowTokens;
            end
            if obj.MaxOutputTokens > 0
                s.maxOutputTokens = obj.MaxOutputTokens;
            end
            if ~isempty(obj.Capabilities)
                s.capabilities = obj.Capabilities;
            end
        end
    end
end
