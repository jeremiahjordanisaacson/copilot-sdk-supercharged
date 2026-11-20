/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import { ResponseError } from "vscode-jsonrpc/node";
import type { createInternalServerRpc, createServerRpc } from "../src/generated/rpc.js";
import type { JsonValue, UserSettingsJsonValue } from "../src/index.js";

// Ordinary reads remain compatible with existing JSON consumers and reject unsafe integers.
export async function readJsonCompatibleSettings(
    settings: ReturnType<typeof createServerRpc>["user"]["settings"],
    jsonApi: (value: JsonValue) => void
) {
    const response = await settings.get();
    const metadata = response.settings.statusLine;
    if (metadata) {
        jsonApi(metadata.value);
        jsonApi(metadata.default);
    }
    return JSON.stringify(response);
}

// Internal host example; these methods are not part of the public SDK API.
export async function editUserSettings(
    settings: ReturnType<typeof createInternalServerRpc>["user"]["settings"],
    configDir: string
) {
    const validation = await settings.validate({
        topKey: "voice",
        value: { enabled: true },
    });
    if (!validation.ok) {
        throw new Error(JSON.stringify(validation.issues));
    }

    await settings.update({
        key: "voice",
        value: { enabled: true },
        configDir,
    });
    return settings.update({
        key: "voice",
        value: { selectedModel: null },
        configDir,
    });
}

export async function setHookEnabled(
    settings: ReturnType<typeof createInternalServerRpc>["user"]["settings"],
    settingsPath: string,
    disableKey: string,
    enabled: boolean
) {
    const result = await settings.setHookDisabled({ settingsPath, disableKey, disabled: !enabled });
    if (result.status !== "ok") {
        throw new Error(result.errorMessage ?? `Hook settings update failed: ${result.status}`);
    }
    console.log("Hook enabled:", enabled);
    return result;
}

/**
 * Retain host controls before guarded mutations.
 * Later SDK requests still resolve policy from their credential owner's sources.
 */
export async function retainHostPolicy(
    policy: ReturnType<typeof createInternalServerRpc>["managedSettings"],
    input: Parameters<typeof policy.ingestAndRetain>[0]
) {
    const resolved = await policy.ingestAndRetain(input);
    if (resolved.retentionError !== undefined) {
        throw new Error(resolved.retentionError);
    }
    console.log("Managed host policy:", JSON.stringify(resolved.ingestion.settings ?? null));
    return resolved;
}

export async function conditionallySelectVoiceModel(
    settings: ReturnType<typeof createInternalServerRpc>["user"]["settings"],
    configDir: string
) {
    const selected = await settings.update({
        key: "voice",
        value: { selectedModel: "example-model" },
        ifFieldsMatch: { selectedModel: null },
        configDir,
    });
    const staleCleanup = await settings.update({
        key: "voice",
        value: { enabled: false, selectedModel: null },
        ifFieldsMatch: { selectedModel: null },
        configDir,
    });
    return { selected, staleCleanup };
}

export async function conditionallySelectZeroVoiceModel(
    settings: ReturnType<typeof createInternalServerRpc>["user"]["settings"],
    configDir: string
) {
    const conditions = { count: -0, nested: { values: [-0, { count: -0 }] } };
    const result = await settings.update({
        key: "voice",
        value: { selectedModel: "zero-model" },
        ifFieldsMatch: conditions,
        configDir,
    });
    return { result, conditions };
}

export async function conditionallySelectVoiceModelWithJsonOutcome(
    settings: ReturnType<typeof createInternalServerRpc>["user"]["settings"],
    configDir: string
) {
    try {
        const result = await settings.update({
            key: "voice",
            value: { selectedModel: "outcome-model" },
            ifFieldsMatch: { selectedModel: "old-model" },
            configDir,
        });
        return result.applied;
    } catch (error) {
        const data: unknown = error instanceof ResponseError ? error.data : undefined;
        if (
            data !== null &&
            typeof data === "object" &&
            "code" in data &&
            data.code === "settings_unsafe_integer" &&
            "applied" in data &&
            typeof data.applied === "boolean"
        ) {
            return data.applied;
        }
        throw error;
    }
}

export async function conditionallySelectPreciseVoiceModel(
    settings: ReturnType<typeof createInternalServerRpc>["user"]["settings"],
    configDir: string
) {
    const loaded = await settings.loadForEdit({ configDir }, { numericMode: "exact" });
    const document = loaded.settings;
    if (!document || typeof document !== "object" || !("voice" in document)) {
        throw new Error("The settings document has no voice setting");
    }
    const voice = document.voice;
    if (!voice || typeof voice !== "object" || Array.isArray(voice) || !("nested" in voice)) {
        throw new Error("The voice setting has no revision");
    }
    const nested = voice.nested;
    if (!nested || typeof nested !== "object" || !("revision" in nested)) {
        throw new Error("The voice setting has no revision");
    }
    const revision = nested.revision;
    if (typeof revision !== "bigint" && typeof revision !== "number") {
        throw new Error("The revision is not a number");
    }
    const conditions = { nested: { revision } };
    const result = await settings.update(
        {
            key: "voice",
            value: { ...voice, selectedModel: "precise-model" },
            ifFieldsMatch: conditions,
            configDir,
        },
        { numericMode: "exact" }
    );
    return { result, conditions };
}

export async function editWholeUserSettings(
    settings: ReturnType<typeof createInternalServerRpc>["user"]["settings"]
) {
    const loaded = await settings.loadForEdit(undefined, { numericMode: "exact" });
    const document = loaded.settings;
    if (!document || typeof document !== "object" || Array.isArray(document)) {
        throw new Error("The settings document is not editable");
    }
    const normalized = await settings.normalize({ settings: document }, { numericMode: "exact" });
    const normalizedDocument = normalized.settings;
    if (
        !normalizedDocument ||
        typeof normalizedDocument !== "object" ||
        Array.isArray(normalizedDocument)
    ) {
        throw new Error("The normalized settings document is not editable");
    }
    const voice = normalizedDocument.voice;
    if (!voice || typeof voice !== "object" || Array.isArray(voice)) {
        throw new Error("The settings document has no voice setting");
    }
    const edited = {
        ...normalizedDocument,
        voice: { ...voice, selectedModel: "whole-file-model" },
    };
    const validation = await settings.validate({ topKey: "voice", value: edited.voice });
    if (!validation.ok) {
        throw new Error(JSON.stringify(validation.issues));
    }
    await settings.writeAll({ settings: edited });
    return { before: document, after: edited };
}

export async function editUserSettingsKeys(
    settings: ReturnType<typeof createInternalServerRpc>["user"]["settings"],
    publicSettings: ReturnType<typeof createServerRpc>["user"]["settings"]
) {
    const loaded = await settings.loadForEdit(undefined, { numericMode: "exact" });
    const document = loaded.settings;
    if (!document || typeof document !== "object" || Array.isArray(document)) {
        throw new Error("The settings document is not editable");
    }
    const voice = document.voice;
    if (!voice || typeof voice !== "object" || Array.isArray(voice)) {
        throw new Error("The settings document has no voice setting");
    }
    await publicSettings.set({ settings: { voice: { ...voice, selectedModel: "set-model" } } });
    const afterSet = await settings.loadForEdit(undefined, { numericMode: "exact" });
    await settings.writeKey({
        key: "voice",
        value: { ...voice, selectedModel: "write-key-model" },
    });
    const afterWriteKey = await settings.loadForEdit(undefined, { numericMode: "exact" });
    const applied = await settings.applyTopKey({
        canonicalPath: ["voice"],
        topKey: "voice",
        value: { ...voice, selectedModel: "apply-top-key-model" },
    });
    if (applied.status !== "ok") {
        throw new Error(JSON.stringify(applied));
    }
    const afterApplyTopKey = await settings.loadForEdit(undefined, { numericMode: "exact" });
    const atomicEdit = await settings.edit({
        canonicalPath: ["voice"],
        operation: "set",
        value: { ...voice, selectedModel: "atomic-edit-model" },
    });
    if (atomicEdit.status !== "changed") {
        throw new Error(JSON.stringify(atomicEdit));
    }
    const afterEdit = await settings.loadForEdit(undefined, { numericMode: "exact" });
    return { before: document, afterSet, afterWriteKey, afterApplyTopKey, afterEdit };
}

export async function editPublicUserSettings(
    settings: ReturnType<typeof createServerRpc>["user"]["settings"]
) {
    const before = await settings.get({ numericMode: "exact" });
    const metadata = before.settings.statusLine;
    if (!metadata) {
        throw new Error("The status line has no settings metadata");
    }
    const statusLine = metadata.value;
    if (!statusLine || typeof statusLine !== "object" || Array.isArray(statusLine)) {
        throw new Error("The status line is not configured");
    }
    await settings.set({
        settings: { statusLine: { ...statusLine, command: "echo public-edit" } },
    });
    return { before, after: await settings.get({ numericMode: "exact" }) };
}

export async function displayPublicUserSettings(
    settings: ReturnType<typeof createServerRpc>["user"]["settings"],
    display: ReturnType<typeof createInternalServerRpc>["settings"]
) {
    const snapshot = await settings.get({ numericMode: "exact" });
    const metadata = snapshot.settings.statusLine;
    if (!metadata) {
        throw new Error("The status line has no settings metadata");
    }
    const before: UserSettingsJsonValue = metadata.value;
    const result = await display.redactValueForDisplay(
        {
            value: before,
            basePath: ["statusLine"],
        },
        { numericMode: "exact" }
    );
    return { before, displayed: result.value };
}
