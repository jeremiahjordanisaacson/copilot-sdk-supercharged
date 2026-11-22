/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import type { JsonValue, UserSettingsJsonValue } from "./generated/rpc.js";

/** @internal */
export const exactSettingsResponse = Symbol("exactSettingsResponse");

/** @internal */
export const settingsResponseMethods: ReadonlySet<string> = new Set([
    "settings.redactValueForDisplay",
    "user.settings.get",
    "user.settings.buildRuntime",
    "user.settings.loadAndMergeRepo",
    "user.settings.loadForConfigDir",
    "user.settings.loadForEdit",
    "user.settings.loadRepo",
    "user.settings.loadWithWarning",
    "user.settings.mergeManagedAndUser",
    "user.settings.mergeRepoValue",
    "user.settings.normalize",
    "user.settings.update",
]);

/**
 * Settings reads reject unsafe integers unless the caller selects exact bigint results.
 * A completed conditional update retains its `applied` flag in the error's data.
 * A response decoding error does not mean the update failed; do not retry it automatically.
 */
export interface SettingsResponseOptions<Mode extends "json" | "exact" = "json" | "exact"> {
    numericMode: Mode;
}

/** Preserve typed fields while exposing exact integers in opaque settings JSON. */
export type ExactSettingsResponse<T> =
    JsonValue extends Exclude<T, undefined>
        ? UserSettingsJsonValue | Extract<T, undefined>
        : T extends object
          ? { [Key in keyof T]: ExactSettingsResponse<T[Key]> }
          : T;

/** The result selected by a settings read's numeric mode. */
export type SettingsResponse<T, Mode extends "json" | "exact"> = Mode extends "exact"
    ? ExactSettingsResponse<T>
    : T;

/** @internal */
export function settingsResponseParams(
    params: object | undefined,
    options?: SettingsResponseOptions<"json" | "exact">
): object | undefined {
    return options?.numericMode === "exact" ? { ...params, [exactSettingsResponse]: true } : params;
}
