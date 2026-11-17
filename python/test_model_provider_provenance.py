# Copyright (c) Microsoft Corporation. All rights reserved.

import pytest

from copilot import Model


@pytest.mark.parametrize(
    "provider",
    [
        {"id": "copilot", "kind": "copilot", "label": "GitHub Copilot"},
        {"id": "loki:opaque", "kind": "loki", "label": "Microsoft 365 Copilot"},
        None,
    ],
)
def test_model_info_round_trips_optional_native_provider(provider):
    wire = {
        "id": "shared-model",
        "name": "Shared Model",
        "capabilities": {"supports": {}, "limits": {}},
    }
    if provider is not None:
        wire["provider"] = provider

    model = Model.from_dict(wire)
    assert (model.provider.to_dict() if model.provider else None) == provider
    assert {key: value for key, value in model.to_dict().items() if key == "provider"} == {
        key: value for key, value in wire.items() if key == "provider"
    }
