// Copyright (c) Microsoft Corporation. All rights reserved.

import {
    clientOptionMigrations,
    sessionOptionMigrations,
} from "./generated/options-compatibility.js";
import {
    RuntimeConnection,
    type CopilotClientOptions,
    type SessionHooks,
    type SessionOptionsBase,
} from "./types.js";

type Migration = readonly [from: string, to: string];

function isObject(value: unknown): value is Record<string, unknown> {
    return typeof value === "object" && value !== null && !Array.isArray(value);
}

function copyOptions<T extends object>(options: T): T & Record<string, unknown> {
    const copy = {} as T & Record<string, unknown>;
    let prototype: unknown = options;
    while (isObject(prototype) && prototype !== Object.prototype) {
        for (const key of Reflect.ownKeys(prototype)) {
            if (key === "constructor" || Object.hasOwn(copy, key)) continue;
            const descriptor = Object.getOwnPropertyDescriptor(prototype, key);
            if (!descriptor) continue;
            if ("value" in descriptor) {
                Object.defineProperty(copy, key, {
                    ...descriptor,
                    configurable: true,
                    writable: true,
                });
                continue;
            }
            // Keep getters on their original receiver; writes only change the copy.
            Object.defineProperty(copy, key, {
                enumerable: descriptor.enumerable,
                configurable: true,
                get: (): unknown => Reflect.get(options, key),
                set: (value: unknown) => {
                    Object.defineProperty(copy, key, {
                        value,
                        enumerable: descriptor.enumerable,
                        writable: true,
                        configurable: true,
                    });
                },
            });
        }
        prototype = Object.getPrototypeOf(prototype);
    }
    return copy;
}

function insertMissing(object: Record<string, unknown>, path: string, value: unknown): void {
    const dot = path.indexOf(".");
    if (dot === -1) {
        if (object[path] === undefined) object[path] = value;
        return;
    }
    const head = path.slice(0, dot);
    const current = object[head];
    if (current !== undefined && !isObject(current)) {
        throw new TypeError(`Option group '${head}' must be an object`);
    }
    // Clone only the path being changed; callbacks and class instances at leaves
    // must keep their identity, and caller-owned groups may be frozen.
    const group = copyOptions(current ?? {});
    insertMissing(group, path.slice(dot + 1), value);
    object[head] = group;
}

function normalize<T extends object>(options: T, migrations: readonly Migration[]): T {
    const result = copyOptions(options);
    const pending: [string, unknown][] = [];
    for (const [from, to] of migrations) {
        if (!Object.hasOwn(result, from)) continue;
        const value = result[from];
        if (to.startsWith(`${from}.`) && !Array.isArray(value)) continue;
        delete result[from];
        if (value !== undefined) pending.push([to, value]);
    }
    for (const [to, value] of pending) insertMissing(result, to, value);
    return result;
}

const legacyHooks = [
    "onPreToolUse",
    "onPreMcpToolCall",
    "onPostToolUse",
    "onPostToolUseFailure",
    "onUserPromptSubmitted",
    "onUserPromptTransformed",
    "onSessionStart",
    "onSessionEnd",
    "onErrorOccurred",
    "onAgentStop",
    "onSubagentStart",
    "onSubagentStop",
] as const satisfies readonly (keyof SessionHooks)[];

const hookOptionKeys = ["handlers", "directories", "enableRepositoryHooks"] as const;

/** Private type-erasure compatibility, not an alternative public options API. */
export function normalizeSessionOptions<T extends SessionOptionsBase>(options: T): T {
    if (Reflect.get(options, "provider") !== undefined) {
        throw new TypeError(
            "provider is no longer supported; use inference.providers and inference.models"
        );
    }
    const result = normalize(options, sessionOptionMigrations);
    const hookOptions = result.hooks;
    if (isObject(hookOptions)) {
        const handlers: Record<string, unknown> = {};
        for (const key of legacyHooks) {
            if (key in hookOptions) {
                handlers[key] = hookOptions[key];
            }
        }
        if (Object.keys(handlers).length > 0) {
            // Legacy handler objects may also own state or helper methods.
            const hooks: Record<string, unknown> = {};
            for (const key of hookOptionKeys) {
                if (key in hookOptions) hooks[key] = hookOptions[key];
            }
            if (isObject(hooks.handlers)) {
                for (const key of legacyHooks) {
                    const handler = hooks.handlers[key];
                    if (handler !== undefined) handlers[key] = handler;
                }
                hooks.handlers = handlers;
            } else if (hooks.handlers === undefined) {
                hooks.handlers = handlers;
            }
            result.hooks = hooks;
        } else if (
            !hookOptionKeys.some((key) => key in hookOptions) &&
            Object.keys(hookOptions).length > 0
        ) {
            throw new TypeError("Unsupported hooks option; use hooks.handlers for hook callbacks");
        }
    }
    for (const group of new Set([
        "hooks",
        ...sessionOptionMigrations.map(([, to]) => to.split(".")[0]),
    ])) {
        const value: unknown = Reflect.get(result, group);
        if (value !== undefined && !isObject(value)) {
            throw new TypeError(`Option group '${group}' must be an object`);
        }
    }
    return result;
}

export function normalizeClientOptions(options: CopilotClientOptions): CopilotClientOptions {
    let result = normalize(
        options,
        clientOptionMigrations.filter(([, to]) => to === "runtime")
    );
    if (
        result.runtime === undefined &&
        (Reflect.get(result, "baseDirectory") !== undefined ||
            Reflect.get(result, "sessionIdleTimeoutSeconds") !== undefined)
    ) {
        Object.assign(result, { runtime: RuntimeConnection.forDefault() });
    }
    result = normalize(
        result,
        clientOptionMigrations.filter(([, to]) => to !== "runtime")
    );
    if (
        result.runtime &&
        !["inprocess", "spawned-stdio", "spawned-tcp", "tcp"].includes(result.runtime.kind)
    ) {
        throw new TypeError(
            "Unsupported runtime connection kind; use the current RuntimeConnection factories"
        );
    }
    return result;
}
