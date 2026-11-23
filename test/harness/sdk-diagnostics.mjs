/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import { createReadStream } from "node:fs";
import { readdir } from "node:fs/promises";
import { join, resolve } from "node:path";
import { createInterface } from "node:readline";
import { fileURLToPath } from "node:url";

// This is a finite projection, not a redactor: target names alone are not a privacy boundary.
const presence = {
    generation: "number",
    event_count: "number",
    resolved_live: "boolean",
    request_present: "boolean",
    completion_present: "boolean",
    tool_present: "boolean",
    orphaned: "boolean",
};
const childParts = {
    child_span_handle: "number",
    chat_span_handle: "number",
    part_count: "number",
    text_count: "number",
    reasoning_count: "number",
    capture_responses: "boolean",
};
const childCloseReasons = ["subagent_completed", "subagent_failed", "agent_turn_close", "dispose"];
const shell = {
    generation: "number",
    sandbox: "boolean",
    pid: "number",
};
const shellState = {
    ...shell,
    current_generation: "number",
    status: ["idle", "running", "completed", "failed"],
    exit_code_present: "boolean",
    exit_code: "number",
    output_bytes: "number",
    reader_backlog_count: "number",
    pending_stdout_bytes: "number",
    pending_stderr_bytes: "number",
    publications_closed: "boolean",
};
const records = {
    "sdk telemetry auth": {
        source: ["session", "manager"],
        changed: "boolean",
        user_present: "boolean",
    },
    "sdk telemetry route": {
        kind: ["sandbox_session_state", "session_start", "session_model_change", "other"],
        host_only: "boolean",
        credential_resolved: "boolean",
        endpoint_present: "boolean",
        buffering: "boolean",
        service_present: "boolean",
        event_count: "count",
    },
    "sdk telemetry enqueue": {
        phase: ["disposed", "logged_out", "queue", "pending_auth"],
        restricted: "boolean",
        pending_count: "count",
    },
    "sdk telemetry queue": {
        phase: ["worker_failed", "client", "flush", "gzip_failed", "accepted", "rejected"],
        client_present: "boolean",
        event_count: "count",
        http_status: "count",
        status_present: "boolean",
        is_connect: "boolean",
        is_timeout: "boolean",
        is_body: "boolean",
        is_request: "boolean",
    },
    "workflow availability decision": { available: "boolean" },
    "sdk transport teardown": {
        connection: "count",
        active_requests: "count",
        driver_finished: "boolean",
        phase: [
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
        ],
    },
    "sdk pending permission response": {
        ...presence,
        prepared_replay: "boolean",
        aborting: "boolean",
        reason: [
            "prepared_replay",
            "completed_request",
            "unknown_request",
            "resolved_tool",
            "accepted",
        ],
    },
    "sdk pending tool response": {
        ...presence,
        reason: [
            "completed_request",
            "unknown_request",
            "non_pending_request",
            "completed_tool",
            "accepted",
        ],
    },
    "sdk pending permission delivery": {
        generation: "number",
        settled: "boolean",
        invariant_failure: "boolean",
    },
    "sdk pending tool delivery": {
        generation: "number",
        origin: ["client_response"],
        accepted: "boolean",
        success: "boolean",
        failed: "boolean",
    },
    "sdk pending permission drain planned": {
        generation: "number",
        requests: "number",
        origin: ["suspend", "abort"],
    },
    "sdk pending permission drain completed": {
        generation: "number",
        generation_known: "boolean",
        origin: ["client_response", "cancellation"],
        settled: "boolean",
        invariant_failure: "boolean",
    },
    "sdk pending stale drain": { generation: "number", expected_generation: "number" },
    "sdk pending transition": {
        generation: "number",
        event_count: "number",
        kind: ["permission", "external_tool"],
        origin: [
            "registered",
            "client_response",
            "delivery_failure",
            "suspend",
            "abort",
            "timeout",
            "terminal_response_rejected",
            "prepared_conflict",
            "invalid_response",
            "already_claimed",
        ],
        terminal_cancelled: "boolean",
    },
    "sdk pending resume": {
        connection: "number",
        dispatch: "number",
        warm: "boolean",
        phase: ["begin", "accepted"],
    },
    "sdk pending reap": {
        registration: "number",
        phase: [
            "begin",
            "callbacks_drained",
            "lifecycle_acquired",
            "owner_present",
            "generation_removed",
            "complete",
            "generation_changed",
        ],
    },
    "sdk session shutdown": {
        generation: "number",
        phase: ["begin", "tasks_drained", "terminal_drain_begin", "terminal_drain_complete"],
        failed: "boolean",
        elapsed_ms: "number",
    },
    "sdk detach": {
        connection: "number",
        dispatch: "number",
        elapsed_ms: "number",
        failed: "boolean",
        phase: [
            "received",
            "lifecycle_acquired",
            "already_detached",
            "close_already_started",
            "shared_owner_begin",
            "shared_owner_complete",
            "host_destroy_begin",
            "host_destroy_complete",
            "finalized",
            "dispatch_complete",
        ],
    },
    "sdk detach state": {
        connection: "number",
        dispatch: "number",
        generation: "number",
        active: "boolean",
        attached: "boolean",
    },
    "sdk otel dispatch": {
        event_kind: [
            "assistant_message",
            "model_call_start",
            "assistant_turn_end",
            "subagent_completed",
            "subagent_failed",
        ],
        event_sequence: "number",
        scope: ["parent", "child"],
        child_present: "boolean",
        chat_present: "boolean",
        child_span_handle: "number",
        chat_span_handle: "number",
    },
    "sdk otel admission": {
        phase: ["received", "no_tracker", "fenced", "pending", "submitted"],
        event_sequence: "count",
        policy_revision: "count",
        authority_fenced: "boolean",
        preparation_pending: "boolean",
        policy_enabled: "boolean",
        projected_event_count: "count",
        submitted: "boolean",
    },
    "sdk otel policy state": {
        origin: [
            "host_prepare",
            "host_commit",
            "host_commit_initial",
            "host_bind",
            "host_rollback",
            "explicit_fence",
            "legacy_settled",
            "native_pending",
            "native_settled",
            "native_apply",
            "reserved_apply",
            "direct_apply",
        ],
        kind: [
            "transition",
            "restore",
            "fence",
            "apply_stale_source",
            "apply_retry",
            "apply_superseded",
            "apply_settled",
        ],
        expected_some: "boolean",
        expected_incarnation: "count",
        expected_revision: "count",
        previous_incarnation: "count",
        previous_revision: "count",
        current_incarnation: "count",
        current_revision: "count",
        previous_authority_fenced: "boolean",
        authority_fenced: "boolean",
        previous_preparation_pending: "boolean",
        preparation_pending: "boolean",
        previous_native_owned: "boolean",
        native_owned: "boolean",
        policy_enabled: "boolean",
        returned_some: "boolean",
        applied: "boolean",
        build_succeeded: "boolean",
    },
    "sdk otel core policy publication": {
        origin: [
            "clear_account_policy",
            "retained_device_update",
            "retained_server_update",
            "retained_signed_out_update",
            "registry_rotation",
            "signed_out_policy_clear",
            "refresh_start",
            "ingest_settlement",
            "standalone_provider_rebind",
            "standalone_provider_restore",
            "construction_options_auth",
            "manager_auth_changed",
            "resolved_session_auth",
            "hydrated_session_auth",
            "set_credentials",
            "clear_credentials",
            "other_auth_mutation",
        ],
        kind: ["pending_publish", "pending_publish_returned"],
        registry_generation: "count",
        previous_auth_generation: "count",
        auth_generation: "count",
        auth_generation_changed: "boolean",
        previous_server_pending: "boolean",
        server_pending: "boolean",
        policy_identity_present: "boolean",
        source_recovery: "boolean",
        clear_retained: "boolean",
        composition_succeeded: "boolean",
        refresh_generation: "count",
        has_resolved_managed_settings: "boolean",
        has_eligible_current_otel_policy: "boolean",
        expected_auth_generation: "count",
    },
    "sdk otel policy settlement": {
        origin: ["standalone_commit"],
        kind: ["reservation", "application"],
        succeeded: "boolean",
        reservation_some: "boolean",
        committed_incarnation: "count",
        committed_revision: "count",
        expected_incarnation: "count",
        expected_revision: "count",
        applied: "boolean",
    },
    "sdk assistant message": {
        ...childParts,
        scope: ["parent", "child"],
        child_present: "boolean",
        chat_present: "boolean",
        parent_chat_present: "boolean",
    },
    "sdk chat close": {
        ...childParts,
        scope: ["parent", "child"],
        operation: ["chat"],
        reason: [
            ...childCloseReasons,
            "model_call_start",
            "assistant_turn_end",
            "parent_turn_close",
        ],
        status: ["unset", "error"],
    },
    "sdk child completion": {
        ...childParts,
        phase: ["begin", "closed"],
        operation: ["invoke_agent"],
        reason: childCloseReasons,
        status: ["unset", "error"],
        child_present: "boolean",
        chat_present: "boolean",
    },
    "sdk shell exit": {
        ...shell,
        phase: [
            "scheduled",
            "started",
            "observed",
            "observe_failed",
            "finalize_started",
            "finalized",
            "finalize_failed",
        ],
        exit_code_present: "boolean",
        exit_code: "number",
        elapsed_ms: "elapsed",
    },
    "sdk shell reader": {
        ...shell,
        phase: ["eof", "error", "finished"],
        stream: ["stdout", "stderr"],
        elapsed_ms: "elapsed",
    },
    "sdk shell drain": {
        ...shell,
        phase: ["drained", "aborted"],
        reader_count: "number",
        reader_unjoined: "number",
        elapsed_ms: "elapsed",
        backpressured: "boolean",
    },
    "sdk shell completion": {
        ...shellState,
        phase: [
            "finish_stale_generation",
            "finish_not_running",
            "finish_publications_closed",
            "publishing",
            "complete_stale_generation",
            "complete_not_running",
            "completed",
        ],
        lifecycle_error: "boolean",
    },
    "sdk shell wait": {
        ...shellState,
        phase: [
            "interrupted",
            "stale_generation",
            "completion",
            "cancelled",
            "deadline",
            "completion_after_deadline",
        ],
        wait_generation: "number",
        current_wait_generation: "number",
        elapsed_ms: "elapsed",
        timeout_ms: "number",
    },
};

function sanitizeTelemetryLine(line) {
    const match =
        /^\S+ \[(?:DEBUG|WARN|WARNING)\] \[rust:github_telemetry::(service|queue_worker|router)\] (.*?)(?: (\{.*\}))?$/.exec(
            line
        );
    if (!match) return undefined;
    const [, target, message, fields] = match;
    if (target === "service") {
        const enqueue =
            /^Sending telemetry event: cli\.(telemetry|restricted_telemetry) \(kind: (sandbox_session_state|session_start|session_model_change|copilot_user_info|memory_usage|tgrep_startup|session_shutdown|subagent_parallelism_triggered)\)$/.exec(
                message
            );
        if (enqueue) {
            return `github telemetry enqueue ${JSON.stringify({
                kind: enqueue[2],
                restricted: enqueue[1] === "restricted_telemetry",
            })}`;
        }
    }
    if (target === "queue_worker") {
        const flush =
            /^\[telemetry-queue\] Flushing (\d+) events \((\d+) B \u2192 (\d+) B gzipped\)$/.exec(
                message
            );
        if (flush) {
            const counts = flush.slice(1).map(Number);
            if (!counts.every((count) => Number.isSafeInteger(count) && count >= 0)) {
                return undefined;
            }
            return `github telemetry flush ${JSON.stringify({
                event_count: counts[0],
                bytes: counts[1],
                compressed_bytes: counts[2],
            })}`;
        }
    }
    const phases = {
        service: {
            "[telemetry] failed to create telemetry queue": "queue_creation_failed",
        },
        queue_worker: {
            "[telemetry-queue] failed to start worker runtime": "worker_runtime_failed",
            "[telemetry-queue] invalid proxy URL; telemetry POSTs disabled": "invalid_proxy",
            "[telemetry-queue] failed to assemble explicit root certificates; telemetry POSTs disabled":
                "root_certificates_failed",
            "[telemetry-queue] invalid extra CA certificate; telemetry POSTs disabled":
                "invalid_extra_certificate",
            "[telemetry-queue] failed to build HTTP client; telemetry POSTs disabled":
                "http_client_failed",
            "[telemetry-queue] POST accepted": "post_accepted",
            "[telemetry-queue] POST was not accepted; events dropped": "post_rejected",
        },
        router: {
            "Unable to enqueue native startup telemetry": "startup_enqueue_failed",
            "Native startup telemetry buffer full; dropping oldest event": "startup_buffer_full",
        },
    };
    if (!Object.hasOwn(phases[target], message)) return undefined;
    const output = { phase: phases[target][message] };
    if (fields) {
        let input;
        try {
            input = JSON.parse(fields);
        } catch {
            return undefined;
        }
        if (!input || Array.isArray(input) || typeof input !== "object") return undefined;
        for (const key of ["event_count", "status"]) {
            if (!Object.hasOwn(input, key) || input[key] === null) continue;
            if (!Number.isSafeInteger(input[key]) || input[key] < 0) return undefined;
            output[key] = input[key];
        }
    }
    return `github telemetry transport ${JSON.stringify(output)}`;
}

export function sanitizeDiagnosticLine(line) {
    if (line.length > 16384) return undefined;
    const telemetry = sanitizeTelemetryLine(line);
    if (telemetry !== undefined) return telemetry;
    const match = /^\S+ \[DEBUG\] \[rust:sdk_diagnostics\] ([a-z ]+) (\{.*\})$/.exec(line);
    if (!match || !Object.hasOwn(records, match[1])) return undefined;
    let input;
    try {
        input = JSON.parse(match[2]);
    } catch {
        return undefined;
    }
    if (!input || Array.isArray(input) || typeof input !== "object") return undefined;
    const output = {};
    for (const [key, rule] of Object.entries(records[match[1]])) {
        if (!Object.hasOwn(input, key)) continue;
        const value = input[key];
        const valid = Array.isArray(rule)
            ? rule.includes(value)
            : rule === "number"
              ? Number.isSafeInteger(value)
              : rule === "count"
                ? Number.isSafeInteger(value) && value >= 0
                : rule === "elapsed"
                  ? Number.isFinite(value) && value >= 0
                  : typeof value === rule;
        if (!valid) return undefined;
        output[key] = value;
    }
    if (!Object.keys(output).length) return undefined;
    return `${match[1]} ${JSON.stringify(output)}`;
}

export async function readDiagnosticTail(home) {
    const tail = [];
    const append = (line) => {
        const safe = sanitizeDiagnosticLine(line);
        if (safe !== undefined) {
            tail.push(safe);
            if (tail.length > 200) tail.shift();
        }
    };
    let files;
    try {
        files = await readdir(join(home, "logs"));
    } catch (error) {
        if (error.code !== "ENOENT") throw error;
        files = [];
    }
    for (const file of files.sort()) {
        if (!file.endsWith(".log")) continue;
        const lines = createInterface({
            input: createReadStream(join(home, "logs", file)),
            crlfDelay: Infinity,
        });
        for await (const line of lines) append(line);
    }
    return tail;
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
    try {
        const tail = await readDiagnosticTail(process.argv[2]);
        console.log(`[SDK diagnostics: last ${tail.length} audited records]\n${tail.join("\n")}`);
    } catch {
        console.error("SDK diagnostics unavailable");
        process.exitCode = 1;
    }
}
