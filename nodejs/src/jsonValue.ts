/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import { ErrorCodes, ResponseError } from "vscode-jsonrpc/node";
import type { JsonValue, UserSettingsJsonValue } from "./generated/rpc.js";

type JsonValidationCategory =
    | "unsupported_type"
    | "integer_out_of_range"
    | "non_finite_number"
    | "negative_zero"
    | "cyclic_value"
    | "nested_undefined"
    | "unsupported_object";

interface StrictJsonValidationContext {
    code: string;
    label: string;
    allowTopLevelUndefined: boolean;
    allowNegativeZero?: boolean;
    allowBigInt?: boolean;
}

function strictJsonValidationError(
    context: StrictJsonValidationContext,
    category: JsonValidationCategory,
    message: string,
    path: string
): ResponseError<{ code: string; category: JsonValidationCategory; path: string }> {
    return new ResponseError(ErrorCodes.InternalError, message, {
        code: context.code,
        category,
        path,
    });
}

export function assertStrictJson(
    value: unknown,
    context: StrictJsonValidationContext & { allowBigInt: true }
): asserts value is UserSettingsJsonValue | undefined;
export function assertStrictJson(
    value: unknown,
    context: StrictJsonValidationContext & { allowBigInt?: false }
): asserts value is JsonValue | undefined;
export function assertStrictJson(
    value: unknown,
    context: StrictJsonValidationContext
): asserts value is UserSettingsJsonValue | undefined {
    const ancestors = new Set<object>();

    const visit = (current: unknown, path: string, allowUndefined: boolean): void => {
        if (current === undefined) {
            if (allowUndefined) {
                return;
            }
            throw strictJsonValidationError(
                context,
                "nested_undefined",
                `${context.label} contains nested undefined at ${path}`,
                path
            );
        }
        if (current === null || typeof current === "boolean" || typeof current === "string") {
            return;
        }
        if (typeof current === "bigint" && context.allowBigInt) {
            if (current < -9223372036854775808n || current > 18446744073709551615n) {
                throw strictJsonValidationError(
                    context,
                    "integer_out_of_range",
                    `${context.label} contains an integer outside the runtime's exact range at ${path}`,
                    path
                );
            }
            return;
        }
        if (typeof current === "number") {
            if (!Number.isFinite(current)) {
                throw strictJsonValidationError(
                    context,
                    "non_finite_number",
                    `${context.label} contains a non-finite number at ${path}`,
                    path
                );
            }
            // JSON serializes -0 as "0", so a journaled -0 would come back as 0
            // after a resume and break the lossless replay guarantee.
            if (Object.is(current, -0) && !context.allowNegativeZero) {
                throw strictJsonValidationError(
                    context,
                    "negative_zero",
                    `${context.label} contains negative zero at ${path}; normalize it to 0`,
                    path
                );
            }
            return;
        }
        if (
            typeof current === "function" ||
            typeof current === "symbol" ||
            typeof current === "bigint"
        ) {
            throw strictJsonValidationError(
                context,
                "unsupported_type",
                `${context.label} contains a function, symbol, or BigInt at ${path}`,
                path
            );
        }
        if (typeof current !== "object") {
            throw strictJsonValidationError(
                context,
                "unsupported_type",
                `${context.label} contains a function, symbol, or BigInt at ${path}`,
                path
            );
        }
        if (ancestors.has(current)) {
            throw strictJsonValidationError(
                context,
                "cyclic_value",
                `${context.label} contains a cyclic reference at ${path}`,
                path
            );
        }

        ancestors.add(current);
        try {
            if (Array.isArray(current)) {
                const keys = Reflect.ownKeys(current);
                if (
                    keys.length !== current.length + 1 ||
                    keys.some(
                        (key) =>
                            key !== "length" &&
                            (typeof key !== "string" ||
                                !/^(0|[1-9]\d*)$/.test(key) ||
                                Number(key) >= current.length)
                    )
                ) {
                    throw strictJsonValidationError(
                        context,
                        "unsupported_object",
                        `${context.label} contains a non-JSON array property at ${path}`,
                        path
                    );
                }
                for (let index = 0; index < current.length; index++) {
                    const descriptor = Object.getOwnPropertyDescriptor(current, String(index));
                    if (
                        descriptor === undefined ||
                        !descriptor.enumerable ||
                        !("value" in descriptor)
                    ) {
                        throw strictJsonValidationError(
                            context,
                            "unsupported_object",
                            `${context.label} contains a non-JSON array property at ${path}[${index}]`,
                            `${path}[${index}]`
                        );
                    }
                    visit(descriptor.value, `${path}[${index}]`, false);
                }
                return;
            }

            const prototype = Object.getPrototypeOf(current);
            if (prototype !== Object.prototype && prototype !== null) {
                throw strictJsonValidationError(
                    context,
                    "unsupported_object",
                    `${context.label} contains a non-JSON object at ${path}`,
                    path
                );
            }
            for (const key of Reflect.ownKeys(current)) {
                if (typeof key === "symbol") {
                    throw strictJsonValidationError(
                        context,
                        "unsupported_type",
                        `${context.label} contains a function, symbol, or BigInt at ${path}`,
                        path
                    );
                }
                const propertyPath = /^[A-Za-z_$][\w$]*$/.test(key)
                    ? `${path}.${key}`
                    : `${path}[${JSON.stringify(key)}]`;
                const descriptor = Object.getOwnPropertyDescriptor(current, key);
                if (
                    descriptor === undefined ||
                    !descriptor.enumerable ||
                    !("value" in descriptor)
                ) {
                    throw strictJsonValidationError(
                        context,
                        "unsupported_object",
                        `${context.label} contains a non-JSON property at ${propertyPath}`,
                        propertyPath
                    );
                }
                visit(descriptor.value, propertyPath, false);
            }
        } finally {
            ancestors.delete(current);
        }
    };

    visit(value, "$", context.allowTopLevelUndefined);
}
