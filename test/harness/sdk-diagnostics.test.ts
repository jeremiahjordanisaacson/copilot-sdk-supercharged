/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import { mkdtemp, mkdir, writeFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { describe, expect, it, onTestFinished } from "vitest";
import { readDiagnosticTail, sanitizeDiagnosticLine } from "./sdk-diagnostics.mjs";

const prefix = "2026-10-05T12:00:00Z [DEBUG] [rust:sdk_diagnostics] ";

describe("SDK failure diagnostics", () => {
    it("projects telemetry delivery metadata without payloads, endpoints, or errors", () => {
        const cases = [
            [
                "service",
                "Sending telemetry event: cli.telemetry (kind: session_start)",
                'github telemetry enqueue {"kind":"session_start","restricted":false}',
            ],
            [
                "service",
                "Sending telemetry event: cli.restricted_telemetry (kind: session_model_change)",
                'github telemetry enqueue {"kind":"session_model_change","restricted":true}',
            ],
            [
                "queue_worker",
                "[telemetry-queue] Flushing 3 events (2048 B \u2192 512 B gzipped)",
                'github telemetry flush {"event_count":3,"bytes":2048,"compressed_bytes":512}',
            ],
            [
                "queue_worker",
                '[telemetry-queue] POST accepted {"status":200,"event_count":3,"endpoint":"PRIVATE"}',
                'github telemetry transport {"phase":"post_accepted","event_count":3,"status":200}',
            ],
            [
                "queue_worker",
                '[telemetry-queue] POST was not accepted; events dropped {"error":"PRIVATE","status":null,"event_count":3}',
                'github telemetry transport {"phase":"post_rejected","event_count":3}',
            ],
            [
                "queue_worker",
                '[telemetry-queue] failed to build HTTP client; telemetry POSTs disabled {"error":"PRIVATE"}',
                'github telemetry transport {"phase":"http_client_failed"}',
            ],
            [
                "service",
                '[telemetry] failed to create telemetry queue {"error":"PRIVATE"}',
                'github telemetry transport {"phase":"queue_creation_failed"}',
            ],
            [
                "router",
                'Unable to enqueue native startup telemetry {"error":"PRIVATE"}',
                'github telemetry transport {"phase":"startup_enqueue_failed"}',
            ],
        ];
        for (const [target, message, expected] of cases) {
            const line = `2026-10-05T12:00:00Z [DEBUG] [rust:github_telemetry::${target}] ${message}`;
            expect(sanitizeDiagnosticLine(line)).toBe(expected);
            expect(sanitizeDiagnosticLine(line.replace("[DEBUG]", "[WARN]"))).toBe(expected);
            expect(sanitizeDiagnosticLine(line.replace("[DEBUG]", "[WARNING]"))).toBe(expected);
            expect(expected).not.toContain("PRIVATE");
        }
    });

    it("rejects unaudited telemetry kinds, messages, targets, and invalid counters", () => {
        for (const message of [
            "Sending telemetry event: cli.telemetry (kind: PRIVATE)",
            "PRIVATE",
            '[telemetry-queue] POST accepted {"event_count":-1}',
            '[telemetry-queue] POST accepted {"status":"PRIVATE"}',
            '[telemetry-queue] POST accepted {"event_count":0.5}',
            '[telemetry-queue] POST accepted {"event_count":9007199254740992}',
            "[telemetry-queue] POST accepted {invalid}",
            "[telemetry-queue] Flushing 9007199254740992 events (1 B \u2192 1 B gzipped)",
        ]) {
            expect(
                sanitizeDiagnosticLine(
                    `2026-10-05T12:00:00Z [DEBUG] [rust:github_telemetry::queue_worker] ${message}`
                )
            ).toBeUndefined();
        }
        expect(
            sanitizeDiagnosticLine(
                "2026-10-05T12:00:00Z [DEBUG] [rust:PRIVATE] [telemetry-queue] POST accepted"
            )
        ).toBeUndefined();
        expect(
            sanitizeDiagnosticLine(
                "2026-10-05T12:00:00Z [DEBUG] [rust:github_telemetry::service] Sending telemetry event: cli.telemetry (kind: PRIVATE)"
            )
        ).toBeUndefined();
    });

    it("collects only audited telemetry metadata from the native log file", async () => {
        const home = await mkdtemp(join(tmpdir(), "sdk-telemetry-diagnostics-"));
        onTestFinished(() => rm(home, { recursive: true, force: true }));
        await mkdir(join(home, "logs"));
        await writeFile(
            join(home, "logs", "process.log"),
            [
                '2026-10-05T12:00:00Z [DEBUG] [rust:github_telemetry::service] Sending telemetry event: cli.telemetry (kind: session_start) {"payload":"PRIVATE_PAYLOAD"}',
                '2026-10-05T12:00:00Z [WARN] [rust:github_telemetry::queue_worker] [telemetry-queue] POST was not accepted; events dropped {"error":"PRIVATE_ERROR","endpoint":"PRIVATE_ENDPOINT","status":503,"event_count":2}',
                "2026-10-05T12:00:00Z [DEBUG] [rust:github_telemetry::queue_worker] PRIVATE_LOG",
            ].join("\n")
        );
        expect(await readDiagnosticTail(home)).toEqual([
            'github telemetry enqueue {"kind":"session_start","restricted":false}',
            'github telemetry transport {"phase":"post_rejected","event_count":2,"status":503}',
        ]);
    });

    it.each([
        {
            record: "sdk telemetry auth",
            fields: { source: "session", changed: true, user_present: true },
        },
        {
            record: "sdk telemetry auth",
            fields: { source: "manager", changed: true, user_present: false },
        },
        {
            record: "sdk telemetry route",
            fields: {
                kind: "session_start",
                host_only: false,
                credential_resolved: true,
                endpoint_present: true,
                buffering: false,
                service_present: true,
                event_count: 1,
            },
        },
        {
            record: "sdk telemetry enqueue",
            fields: { phase: "queue", restricted: false, pending_count: 0 },
        },
        {
            record: "sdk telemetry queue",
            fields: { phase: "client", client_present: true },
        },
        {
            record: "sdk telemetry queue",
            fields: { phase: "flush", client_present: false, event_count: 2 },
        },
        {
            record: "sdk telemetry queue",
            fields: { phase: "accepted", event_count: 2, http_status: 204 },
        },
        {
            record: "sdk telemetry queue",
            fields: {
                phase: "rejected",
                event_count: 2,
                http_status: 0,
                status_present: false,
                is_connect: true,
                is_timeout: false,
                is_body: false,
                is_request: true,
            },
        },
    ])("projects $record without auth, destination or event content", ({ record, fields }) => {
        const safe = sanitizeDiagnosticLine(
            `${prefix}${record} ${JSON.stringify({
                ...fields,
                session_id: "PRIVATE_SESSION",
                login: "PRIVATE_LOGIN",
                token: "PRIVATE_TOKEN",
                endpoint: "PRIVATE_ENDPOINT",
                proxy: "PRIVATE_PROXY",
                path: "PRIVATE_PATH",
                payload: { content: "PRIVATE_PAYLOAD" },
                error: "PRIVATE_ERROR",
            })}`
        );
        expect(safe).toBeDefined();
        expect(JSON.parse(safe!.slice(record.length + 1))).toEqual(fields);
        expect(safe).not.toContain("PRIVATE");
    });

    it.each([
        ["sdk telemetry auth", { source: "PRIVATE_SOURCE" }],
        ["sdk telemetry auth", { user_present: "PRIVATE_USER" }],
        ["sdk telemetry auth", { changed: null }],
        ["sdk telemetry route", { kind: "PRIVATE_KIND" }],
        ["sdk telemetry route", { event_count: -1 }],
        ["sdk telemetry route", { endpoint_present: "PRIVATE_ENDPOINT" }],
        ["sdk telemetry enqueue", { phase: "PRIVATE_PHASE" }],
        ["sdk telemetry enqueue", { pending_count: 0.5 }],
        ["sdk telemetry enqueue", { restricted: "PRIVATE_STATE" }],
        ["sdk telemetry queue", { phase: "PRIVATE_PHASE" }],
        ["sdk telemetry queue", { event_count: Number.MAX_SAFE_INTEGER + 1 }],
        ["sdk telemetry queue", { http_status: "PRIVATE_STATUS" }],
        ["sdk telemetry queue", { is_connect: "PRIVATE_ERROR" }],
        ["sdk telemetry queue", { token: "PRIVATE_TOKEN" }],
    ])("rejects invalid or unaudited %s metadata", (record, fields) => {
        expect(
            sanitizeDiagnosticLine(`${prefix}${record} ${JSON.stringify(fields)}`)
        ).toBeUndefined();
    });

    it("projects model admission phases, authoritative sequences and policy metadata without content", () => {
        const cases = [
            { phase: "received" },
            { phase: "no_tracker" },
            {
                phase: "fenced",
                event_sequence: 17,
                policy_revision: 3,
                authority_fenced: false,
                preparation_pending: true,
                policy_enabled: true,
            },
            { phase: "pending", event_sequence: 17 },
            { phase: "submitted", event_sequence: 17, projected_event_count: 1, submitted: true },
            { phase: "submitted", projected_event_count: 0, submitted: false },
        ];
        for (const fields of cases) {
            const safe = sanitizeDiagnosticLine(
                `${prefix}sdk otel admission ${JSON.stringify({
                    ...fields,
                    session_id: "PRIVATE_SESSION",
                    event_id: "PRIVATE_EVENT",
                    model: "PRIVATE_MODEL",
                    payload: { content: "PRIVATE_PAYLOAD" },
                    error: "PRIVATE_ERROR",
                    path: "PRIVATE_PATH",
                })}`
            );
            expect(safe).toBeDefined();
            expect(JSON.parse(safe!.slice("sdk otel admission ".length))).toEqual(fields);
            expect(safe).not.toContain("PRIVATE");
        }
        for (const fields of [
            { phase: "PRIVATE_PHASE" },
            { event_sequence: -1 },
            { event_sequence: 0.5 },
            { event_sequence: Number.MAX_SAFE_INTEGER + 1 },
            { event_sequence: "PRIVATE_SEQUENCE" },
            { event_sequence: null },
            { policy_revision: -1 },
            { policy_revision: 0.5 },
            { projected_event_count: -1 },
            { projected_event_count: 0.5 },
            { authority_fenced: "PRIVATE_STATE" },
            { preparation_pending: null },
            { policy_enabled: "PRIVATE_STATE" },
            { submitted: "PRIVATE_STATE" },
            { session_id: "PRIVATE_SESSION" },
        ]) {
            expect(
                sanitizeDiagnosticLine(`${prefix}sdk otel admission ${JSON.stringify(fields)}`)
            ).toBeUndefined();
        }
    });

    it.each([
        {
            label: "native authority fence",
            record: "sdk otel policy state",
            fields: {
                origin: "native_pending",
                kind: "fence",
                expected_some: false,
                previous_incarnation: 2,
                previous_revision: 10,
                current_incarnation: 2,
                current_revision: 11,
                previous_authority_fenced: false,
                authority_fenced: true,
                previous_preparation_pending: true,
                preparation_pending: true,
                previous_native_owned: true,
                native_owned: true,
                policy_enabled: true,
                returned_some: true,
            },
        },
        {
            label: "resident credential publication",
            record: "sdk otel core policy publication",
            fields: {
                origin: "standalone_provider_rebind",
                kind: "pending_publish_returned",
                registry_generation: 1,
                previous_auth_generation: 2,
                auth_generation: 3,
                auth_generation_changed: true,
                previous_server_pending: false,
                server_pending: true,
                policy_identity_present: true,
                source_recovery: false,
            },
        },
        {
            label: "superseded host reservation",
            record: "sdk otel policy settlement",
            fields: {
                origin: "standalone_commit",
                kind: "reservation",
                succeeded: true,
                reservation_some: false,
            },
        },
        {
            label: "successfully disabled application",
            record: "sdk otel policy settlement",
            fields: {
                origin: "standalone_commit",
                kind: "application",
                expected_incarnation: 2,
                expected_revision: 3,
                succeeded: true,
                applied: false,
            },
        },
    ])("projects $label without credential or policy content", ({ record, fields }) => {
        const safe = sanitizeDiagnosticLine(
            `${prefix}${record} ${JSON.stringify({
                ...fields,
                session_id: "PRIVATE_SESSION",
                registration: "PRIVATE_REGISTRATION",
                account: "PRIVATE_ACCOUNT",
                policy: { endpoint: "PRIVATE_ENDPOINT", headers: { token: "PRIVATE_TOKEN" } },
                error: "PRIVATE_ERROR",
            })}`
        );
        expect(safe).toBeDefined();
        expect(JSON.parse(safe!.slice(record.length + 1))).toEqual(fields);
        expect(safe).not.toContain("PRIVATE");
    });

    it.each([
        ["sdk otel policy state", "current_revision", "authority_fenced"],
        ["sdk otel core policy publication", "auth_generation", "server_pending"],
        ["sdk otel policy settlement", "committed_revision", "reservation_some"],
    ])("rejects unaudited or invalid %s fields", (record, count, flag) => {
        for (const fields of [
            { origin: "PRIVATE_ORIGIN" },
            { kind: "PRIVATE_KIND" },
            ...[-1, 0.5, Number.MAX_SAFE_INTEGER + 1, "PRIVATE_COUNT", null].map((value) => ({
                [count]: value,
            })),
            { [flag]: "PRIVATE_STATE" },
            { [flag]: null },
            { session_id: "PRIVATE_SESSION" },
        ]) {
            expect(
                sanitizeDiagnosticLine(`${prefix}${record} ${JSON.stringify(fields)}`)
            ).toBeUndefined();
        }
    });

    it("preserves policy attribution through the audited file collector", async () => {
        const home = await mkdtemp(join(tmpdir(), "sdk-policy-diagnostics-"));
        onTestFinished(() => rm(home, { recursive: true, force: true }));
        await mkdir(join(home, "logs"));
        await writeFile(
            join(home, "logs", "policy.log"),
            [
                `${prefix}sdk otel core policy publication {"origin":"standalone_provider_rebind","kind":"pending_publish_returned","auth_generation":3,"account":"PRIVATE"}`,
                `${prefix}sdk otel policy state {"origin":"native_pending","kind":"fence","current_revision":11,"authority_fenced":true,"policy":"PRIVATE"}`,
                `${prefix}sdk otel policy settlement {"origin":"standalone_commit","kind":"reservation","succeeded":true,"reservation_some":false,"session_id":"PRIVATE"}`,
            ].join("\n")
        );
        expect(await readDiagnosticTail(home)).toEqual([
            'sdk otel core policy publication {"origin":"standalone_provider_rebind","kind":"pending_publish_returned","auth_generation":3}',
            'sdk otel policy state {"origin":"native_pending","kind":"fence","current_revision":11,"authority_fenced":true}',
            'sdk otel policy settlement {"origin":"standalone_commit","kind":"reservation","succeeded":true,"reservation_some":false}',
        ]);
    });

    it("projects fixed transport teardown phases and bounded counts without SDK content", () => {
        for (const phase of [
            "shutdown_writer",
            "launcher",
            "launcher_complete",
            "driver",
            "driver_complete",
            "driver_failed",
            "driver_timeout",
            "driver_abort_complete",
            "writer_shutdown_complete",
            "peer_eof",
            "notification_worker",
            "notification_worker_complete",
            "notification_worker_failed",
            "notification_worker_timed_out",
            "active_request_grace",
            "handle_close",
            "handle_close_complete",
            "active_requests",
            "active_requests_complete",
            "active_requests_timed_out",
            "flush",
            "flush_complete",
            "flush_cancelled",
            "flush_timed_out",
            "flush_unavailable",
            "writer_join",
            "writer_join_complete",
            "complete",
        ]) {
            const safe = sanitizeDiagnosticLine(
                `${prefix}sdk transport teardown ${JSON.stringify({
                    connection: 4,
                    active_requests: 2,
                    driver_finished: false,
                    phase,
                    method: "PRIVATE_METHOD",
                    request: { token: "PRIVATE_TOKEN" },
                    sdkLog: "PRIVATE_LOG",
                    error: "PRIVATE_ERROR",
                })}`
            );
            expect(safe).toBeDefined();
            expect(JSON.parse(safe!.slice("sdk transport teardown ".length))).toEqual({
                connection: 4,
                active_requests: 2,
                driver_finished: false,
                phase,
            });
            expect(safe).not.toContain("PRIVATE");
        }
        for (const fields of [
            { phase: "PRIVATE_PHASE" },
            { active_requests: -1 },
            { active_requests: 0.5 },
            { active_requests: Number.MAX_SAFE_INTEGER + 1 },
            { active_requests: "PRIVATE_COUNT" },
            { connection: -1 },
            { connection: null },
            { driver_finished: "PRIVATE_STATE" },
            { method: "PRIVATE_METHOD" },
        ]) {
            expect(
                sanitizeDiagnosticLine(`${prefix}sdk transport teardown ${JSON.stringify(fields)}`)
            ).toBeUndefined();
        }
        expect(
            sanitizeDiagnosticLine(
                `${prefix}sdk transport teardown {"phase":"complete","active_requests":0}`
            )
        ).toBe('sdk transport teardown {"active_requests":0,"phase":"complete"}');
    });

    it("projects only audited fields and never falls back to raw data", () => {
        expect(
            sanitizeDiagnosticLine(
                `${prefix}sdk pending tool response ${JSON.stringify({
                    generation: 9,
                    request_event: 3,
                    reason: "completed_request",
                    requestId: "PRIVATE_REQUEST",
                    sessionId: "PRIVATE_SESSION",
                    tool: "PRIVATE_TOOL",
                    path: "/PRIVATE_PATH",
                    error: "PRIVATE_ERROR",
                    payload: "PRIVATE_PROMPT",
                    credential: "PRIVATE_CREDENTIAL",
                    endpoint: "PRIVATE_ENDPOINT",
                })}`
            )
        ).toBe('sdk pending tool response {"generation":9,"reason":"completed_request"}');
        expect(
            sanitizeDiagnosticLine(
                `${prefix}sdk pending transition ${JSON.stringify({
                    generation: 9,
                    request_event: 3,
                    event_count: 4,
                    completion_present: true,
                    kind: "permission",
                    origin: "client_response",
                    terminal_cancelled: false,
                    requestId: "PRIVATE_REQUEST",
                })}`
            )
        ).toBe(
            'sdk pending transition {"generation":9,"event_count":4,"kind":"permission","origin":"client_response","terminal_cancelled":false}'
        );
        expect(
            sanitizeDiagnosticLine(
                `${prefix}sdk pending tool delivery ${JSON.stringify({
                    generation: 9,
                    origin: "client_response",
                    accepted: true,
                    success: true,
                    failed: false,
                    requestId: "PRIVATE_REQUEST",
                    result: "PRIVATE_RESULT",
                    error: "PRIVATE_ERROR",
                })}`
            )
        ).toBe(
            'sdk pending tool delivery {"generation":9,"origin":"client_response","accepted":true,"success":true,"failed":false}'
        );
        for (const line of [
            `${prefix}PRIVATE_MESSAGE {"generation":9}`,
            `${prefix}sdk pending tool response {"reason":"PRIVATE_REASON"}`,
            `${prefix}sdk pending tool response {"generation":"PRIVATE_ID"}`,
            `${prefix}sdk pending tool response {"reason":"accepted"} PRIVATE_TRAILER`,
            `${prefix}sdk pending tool response {PRIVATE_MALFORMED`,
            `${prefix}sdk pending tool response {"PRIVATE_FIELD":"PRIVATE_VALUE"}`,
            `${prefix}sdk pending tool response {"generation":null}`,
            `${prefix}sdk pending tool response {"generation":{}}`,
            `${prefix}sdk pending tool response {"generation":1e100}`,
            `${prefix}sdk pending tool delivery {"origin":"PRIVATE_ORIGIN"}`,
            `PRIVATE_PREFIX ${prefix}sdk pending tool response {"generation":9}`,
        ]) {
            expect(sanitizeDiagnosticLine(line)).toBeUndefined();
        }
    });

    it("bounds the combined tail across files and preserves only audited native records", async () => {
        const home = await mkdtemp(join(tmpdir(), "sdk-diagnostics-"));
        onTestFinished(() => rm(home, { recursive: true, force: true }));
        await mkdir(join(home, "logs"));
        for (const file of ["first.log", "second.log"]) {
            await writeFile(
                join(home, "logs", file),
                Array.from(
                    { length: 150 },
                    (_, generation) =>
                        `${prefix}sdk pending stale drain ${JSON.stringify({ generation, expected_generation: 1, secret: "PRIVATE" })}`
                ).join("\n")
            );
        }
        await writeFile(
            join(home, "logs", "third.log"),
            [
                `${prefix}sdk detach {"phase":"received","connection":2,"dispatch":1,"content":"PRIVATE"}`,
                `${prefix}sdk detach {"phase":"dispatch_complete","connection":2,"dispatch":1,"failed":false,"elapsed_ms":4}`,
                '[sdk_diagnostics] sdk detach {"phase":"received","content":"PRIVATE"}',
            ].join("\n")
        );
        const tail = await readDiagnosticTail(home);
        expect(tail).toHaveLength(200);
        expect(tail[0]).toContain('"generation":102');
        expect(tail.at(-2)).toBe('sdk detach {"connection":2,"dispatch":1,"phase":"received"}');
        expect(tail.at(-1)).toBe(
            'sdk detach {"connection":2,"dispatch":1,"elapsed_ms":4,"failed":false,"phase":"dispatch_complete"}'
        );
        expect(tail.join("\n")).not.toContain("PRIVATE");
        expect(tail.join("\n")).not.toContain(home);
    });

    it("preserves telemetry correlation and fractional shell timings without content", () => {
        const cases = [
            [
                "sdk otel dispatch",
                {
                    event_kind: "assistant_message",
                    event_sequence: 8,
                    scope: "child",
                    child_present: true,
                    chat_present: true,
                    child_span_handle: 2,
                    chat_span_handle: 3,
                },
            ],
            [
                "sdk assistant message",
                {
                    scope: "child",
                    child_present: true,
                    chat_present: true,
                    parent_chat_present: true,
                    child_span_handle: 2,
                    chat_span_handle: 3,
                    part_count: 2,
                    text_count: 1,
                    reasoning_count: 1,
                    capture_responses: true,
                },
            ],
            [
                "sdk chat close",
                {
                    scope: "child",
                    operation: "chat",
                    reason: "subagent_completed",
                    status: "unset",
                    child_span_handle: 2,
                    chat_span_handle: 3,
                    part_count: 2,
                    text_count: 1,
                    reasoning_count: 1,
                    capture_responses: true,
                },
            ],
            [
                "sdk child completion",
                {
                    phase: "closed",
                    operation: "invoke_agent",
                    reason: "subagent_completed",
                    status: "unset",
                    child_span_handle: 2,
                },
            ],
            [
                "sdk shell wait",
                {
                    generation: 2,
                    current_generation: 2,
                    wait_generation: 1,
                    current_wait_generation: 1,
                    sandbox: true,
                    pid: 123,
                    status: "running",
                    exit_code_present: false,
                    output_bytes: 7,
                    reader_backlog_count: 2,
                    pending_stdout_bytes: 4,
                    pending_stderr_bytes: 3,
                    publications_closed: false,
                    phase: "deadline",
                    elapsed_ms: 30000.125,
                    timeout_ms: 30000,
                },
            ],
            [
                "sdk shell exit",
                {
                    generation: 2,
                    sandbox: false,
                    phase: "observed",
                    exit_code_present: true,
                    exit_code: -1,
                    elapsed_ms: 0.125,
                },
            ],
            [
                "sdk shell reader",
                {
                    generation: 2,
                    sandbox: false,
                    phase: "finished",
                    stream: "stderr",
                    elapsed_ms: 0.25,
                },
            ],
            [
                "sdk shell drain",
                {
                    generation: 2,
                    sandbox: false,
                    phase: "aborted",
                    reader_count: 2,
                    reader_unjoined: 1,
                    backpressured: true,
                    elapsed_ms: 1500.125,
                },
            ],
            [
                "sdk shell completion",
                {
                    generation: 2,
                    current_generation: 3,
                    sandbox: false,
                    status: "running",
                    phase: "finish_stale_generation",
                    lifecycle_error: false,
                },
            ],
        ] as const;
        for (const [message, fields] of cases) {
            const safe = sanitizeDiagnosticLine(
                `${prefix}${message} ${JSON.stringify({
                    ...fields,
                    command: "PRIVATE_COMMAND",
                    output: "PRIVATE_OUTPUT",
                    message: "PRIVATE_MESSAGE",
                    span: { content: "PRIVATE_SPAN" },
                    sessionId: "PRIVATE_SESSION",
                    childId: "PRIVATE_CHILD",
                })}`
            );
            expect(safe).toBeDefined();
            expect(JSON.parse(safe!.slice(message.length + 1))).toEqual(fields);
            expect(safe).not.toContain("PRIVATE");
        }
        for (const fields of [
            { elapsed_ms: -1 },
            { elapsed_ms: "PRIVATE_TIME" },
            { elapsed_ms: null },
            { phase: "PRIVATE_PHASE" },
            { stream: "PRIVATE_STREAM" },
        ]) {
            expect(
                sanitizeDiagnosticLine(`${prefix}sdk shell reader ${JSON.stringify(fields)}`)
            ).toBeUndefined();
        }
    });
});
