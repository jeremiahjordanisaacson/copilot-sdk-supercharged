classdef ModelBilling
    % ModelBilling  Model billing information.

    properties
        % Billing multiplier.
        Multiplier (1,1) double = 0.0

        % Token prices (copilot.ModelBillingTokenPrices or empty).
        TokenPrices

        % Picker price category ('high', 'low', 'medium', 'very_high').
        PickerPriceCategory (1,:) char = ''
    end

    methods
        function obj = ModelBilling(varargin)
            p = inputParser;
            p.addParameter('Multiplier', obj.Multiplier);
            p.addParameter('TokenPrices', []);
            p.addParameter('PickerPriceCategory', obj.PickerPriceCategory);
            p.parse(varargin{:});
            obj.Multiplier = p.Results.Multiplier;
            obj.TokenPrices = p.Results.TokenPrices;
            obj.PickerPriceCategory = p.Results.PickerPriceCategory;
        end

        function s = toStruct(obj)
            s = struct('multiplier', obj.Multiplier);
            if ~isempty(obj.TokenPrices)
                s.tokenPrices = obj.TokenPrices.toStruct();
            end
            if ~isempty(obj.PickerPriceCategory)
                s.pickerPriceCategory = obj.PickerPriceCategory;
            end
        end
    end
end
