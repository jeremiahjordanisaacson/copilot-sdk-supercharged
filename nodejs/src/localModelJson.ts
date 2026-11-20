/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import { Message, type ResponseMessage } from "vscode-jsonrpc/node";
import type { LocalModelDescriptor, LocalModelsSnapshot } from "./generated/rpc.js";
import { parseTrajectoryJson, rawHistoryResponse } from "./taskAnalyticsJson.js";
import { exactSettingsResponse, settingsResponseMethods as settings } from "./settingsJson.js";
import { assertStrictJson } from "./jsonValue.js";

/** @internal */
export function createLocalModelJsonCodec() {
    const pending = new Map<
        string | number | null,
        { method: string; exactSettings: boolean; json: typeof import("lossless-json") }
    >();
    const snapshots = new Set([
        "localModels.configure",
        "localModels.inspect",
        "localModels.watch",
    ]);
    const modelRequests = new Set([
        "localModels.prepare",
        "localModels.setup",
        "localModels.deleteModel",
    ]);
    const settingsWrites = new Set([
        "settings.redactValueForDisplay",
        "user.settings.applyTopKey",
        "user.settings.buildRuntime",
        "user.settings.diagnoseProblems",
        "user.settings.edit",
        "user.settings.loadAndMergeRepo",
        "user.settings.mergeManagedAndUser",
        "user.settings.mergeRepoValue",
        "user.settings.normalize",
        "user.settings.set",
        "user.settings.update",
        "user.settings.validate",
        "user.settings.writeAll",
        "user.settings.writeKey",
    ]);

    return {
        async encode(message: Message): Promise<string> {
            const method = Message.isRequest(message) ? message.method : undefined;
            const rawHistory =
                method === "session.getMessages" &&
                (message as { params?: { [rawHistoryResponse]?: boolean } }).params?.[
                    rawHistoryResponse
                ];
            const analytics = method === "taskAnalytics.classifyTrajectory";
            // Warm the decoder before sending: EOF may otherwise overtake its first import.
            const json =
                method &&
                (snapshots.has(method) ||
                    modelRequests.has(method) ||
                    settings.has(method) ||
                    settingsWrites.has(method) ||
                    rawHistory ||
                    analytics)
                    ? await import("lossless-json")
                    : undefined;
            const text =
                method &&
                (modelRequests.has(method) || settingsWrites.has(method) || analytics) &&
                json
                    ? json.stringify(
                          message,
                          analytics || settingsWrites.has(method)
                              ? function replaceValue(_key, value): unknown {
                                    if (
                                        settingsWrites.has(method) &&
                                        value !== null &&
                                        typeof value === "object" &&
                                        "toJSON" in value &&
                                        typeof value.toJSON === "function"
                                    ) {
                                        const toJSON = value.toJSON;
                                        // Lossless JSON skips the replacer on custom encoder results.
                                        return {
                                            toJSON: () =>
                                                replaceValue(_key, toJSON.call(value, _key)),
                                        };
                                    }
                                    if (
                                        settingsWrites.has(method) &&
                                        typeof value === "number" &&
                                        Number.isInteger(value) &&
                                        !Number.isSafeInteger(value)
                                    ) {
                                        const token = String(value);
                                        if (
                                            !token.includes("e") &&
                                            BigInt(token) !== BigInt(value)
                                        ) {
                                            // Integer tokens can change an unsafe float's runtime comparison.
                                            return new json.LosslessNumber(value.toExponential());
                                        }
                                    }
                                    const nativeJson = JSON as typeof JSON & {
                                        isRawJSON?: (value: unknown) => boolean;
                                    };
                                    return nativeJson.isRawJSON?.(value)
                                        ? json.parse(JSON.stringify(value))
                                        : value;
                                }
                              : undefined,
                          undefined,
                          settingsWrites.has(method)
                              ? [
                                    {
                                        // Runs on every serialized value, including bare toJSON() results.
                                        test: (value) =>
                                            typeof value === "bigint" ||
                                            (json.isLosslessNumber(value) &&
                                                /^-?\d+$/.test(String(value))),
                                        stringify: (value) => {
                                            assertStrictJson(
                                                typeof value === "bigint"
                                                    ? value
                                                    : BigInt(String(value)),
                                                {
                                                    code: "settings_value_not_json",
                                                    label: "Settings value",
                                                    allowTopLevelUndefined: false,
                                                    allowBigInt: true,
                                                }
                                            );
                                            return String(value);
                                        },
                                    },
                                ]
                              : undefined
                      )!
                    : JSON.stringify(message);
            if (
                Message.isRequest(message) &&
                json &&
                (snapshots.has(message.method) ||
                    message.method === "localModels.prepare" ||
                    settings.has(message.method) ||
                    rawHistory)
            ) {
                pending.set(message.id, {
                    method: message.method,
                    exactSettings:
                        (message as { params?: { [exactSettingsResponse]?: boolean } }).params?.[
                            exactSettingsResponse
                        ] === true,
                    json,
                });
            }
            return text;
        },
        async decode(text: string): Promise<Message> {
            const message = JSON.parse(text) as Message;
            if (!Message.isResponse(message)) return message;
            const request = pending.get(message.id);
            pending.delete(message.id);
            if (!request || !("result" in message)) return message;

            if (request.method === "session.getMessages") {
                return (await parseTrajectoryJson(text)) as unknown as ResponseMessage;
            }

            const {
                method,
                json: { parse, parseNumberAndBigInt },
            } = request;
            const precise = parse(text, undefined, (token) => {
                const value = parseNumberAndBigInt(token);
                return typeof value === "bigint" && Number.isSafeInteger(Number(value))
                    ? Number(value)
                    : value;
            }) as ResponseMessage;
            if (settings.has(method)) {
                const containsBigInt = (value: unknown): boolean =>
                    typeof value === "bigint" ||
                    (value !== null &&
                        typeof value === "object" &&
                        Object.values(value).some(containsBigInt));
                if (!request.exactSettings && containsBigInt(precise.result)) {
                    // Return an RPC error so the request rejects without stopping the reader.
                    const failure: ResponseMessage = {
                        jsonrpc: "2.0",
                        id: message.id,
                        error: {
                            code: -32603,
                            message:
                                method === "user.settings.update"
                                    ? 'Settings update completed, but its response contains an unsafe integer. Do not retry the update automatically; pass { numericMode: "exact" } for bigint results.'
                                    : 'Settings response contains an unsafe integer; pass { numericMode: "exact" } to read bigint values.',
                            data: {
                                code: "settings_unsafe_integer",
                                ...(method === "user.settings.update" &&
                                precise.result !== null &&
                                typeof precise.result === "object" &&
                                "applied" in precise.result &&
                                typeof precise.result.applied === "boolean"
                                    ? { applied: precise.result.applied }
                                    : {}),
                            },
                        },
                    };
                    return failure;
                }
                message.result = request.exactSettings ? precise.result : message.result;
                return message;
            }
            // Only opaque descriptor fields gain bigint values. Typed sizes, counters,
            // IDs, and all other RPC methods retain their existing number semantics.
            if (method === "localModels.prepare") {
                const result = message.result as LocalModelDescriptor;
                const source = precise.result as LocalModelDescriptor;
                result.metadata = source.metadata;
                result.serveOptions = source.serveOptions;
            } else {
                const result = message.result as LocalModelsSnapshot;
                const source = precise.result as LocalModelsSnapshot;
                for (const [index, row] of result.state.machine.rows.entries()) {
                    row.descriptor.metadata = source.state.machine.rows[index].descriptor.metadata;
                    row.descriptor.serveOptions =
                        source.state.machine.rows[index].descriptor.serveOptions;
                }
            }
            return message;
        },
    };
}
