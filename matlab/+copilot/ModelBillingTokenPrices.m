classdef ModelBillingTokenPrices
    % ModelBillingTokenPrices  Token prices for model billing.

    properties
        % Batch size for pricing.
        BatchSize

        % Cache price.
        CachePrice

        % Input token price.
        InputPrice

        % Output token price.
        OutputPrice
    end

    methods
        function obj = ModelBillingTokenPrices(varargin)
            p = inputParser;
            p.addParameter('BatchSize', []);
            p.addParameter('CachePrice', []);
            p.addParameter('InputPrice', []);
            p.addParameter('OutputPrice', []);
            p.parse(varargin{:});
            obj.BatchSize = p.Results.BatchSize;
            obj.CachePrice = p.Results.CachePrice;
            obj.InputPrice = p.Results.InputPrice;
            obj.OutputPrice = p.Results.OutputPrice;
        end

        function s = toStruct(obj)
            s = struct();
            if ~isempty(obj.BatchSize)
                s.batchSize = obj.BatchSize;
            end
            if ~isempty(obj.CachePrice)
                s.cachePrice = obj.CachePrice;
            end
            if ~isempty(obj.InputPrice)
                s.inputPrice = obj.InputPrice;
            end
            if ~isempty(obj.OutputPrice)
                s.outputPrice = obj.OutputPrice;
            end
        end
    end
end
