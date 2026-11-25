# Copilot CLI Extensions

Extensions add custom tools, hooks, and behaviors to the Copilot CLI. They run as separate Node.js processes that communicate with the CLI over JSON-RPC via stdio.

## How Extensions Work

```
┌─────────────────────┐          JSON-RPC / stdio           ┌──────────────────────┐
│   Copilot CLI        │ ◄──────────────────────────────────► │  Extension Process   │
│   (parent process)   │    tool calls, events, hooks        │  (forked child)      │
│                      │                                      │                      │
│  • Discovers exts    │                                      │  • Registers tools   │
│  • Forks processes   │                                      │  • Registers hooks   │
│  • Routes tool calls │                                      │  • Listens to events │
│  • Manages lifecycle │                                      │  • Uses SDK APIs     │
└─────────────────────┘                                      └──────────────────────┘
```

1. **Discovery**: The CLI scans `.github/extensions/` (project) and the user's copilot config extensions directory for subdirectories containing an `extension.mjs`, `extension.cjs` or `extension.js` entry file.
2. **Launch**: Legacy extensions start eagerly. Extensions with a valid opt-in `extension.json` advertise native tools, canvases, and slash commands without starting, and launch on first admitted demand. Each launched extension is a child process with `@microsoft/copilot-sdk` available via an automatic module resolver.
3. **Connection**: The extension calls `joinSession()` which establishes a JSON-RPC connection over stdio to the CLI and attaches to the user's current foreground session.
4. **Registration**: Tools and hooks declared in the session options are registered with the CLI and become available to the agent.
5. **Lifecycle**: Extensions reload on `/clear` or foreground-session replacement and stop on CLI exit. On macOS and Linux, ordinary stop/reload sends SIGTERM, then SIGKILL after 5s if still running. CLI exit and session/service teardown use a shared 1s grace period, including disabling extensions on SDK resume; Windows kills immediately without a grace period.

On Windows, the sandboxed extension launcher restores the granted temporary directory. For an absolute `.exe` other than `cmd.exe` with `ELECTRON_RUN_AS_NODE=1` set by the launch provider, the wrapper starts the executable in the session working directory and waits for it, not its descendants. This preserves stdio and normally returns the extension's exit code. In Windows PowerShell 5.1, the wrapper reports an error and exits with code `1` if the host exits before the wrapper reads its process handle. The sandbox still cleans up descendants. Other launches use PowerShell's call operator.

## File Structure

```
.github/extensions/
  my-extension/
    extension.mjs      ← Entry point (required)
```

- The entry file must be named `extension.mjs`, `extension.cjs` or `extension.js`. If a directory holds more than one, the first in that order is used.
- `.mjs` (ES modules) is the recommended form and is what the scaffold generates. `.cjs` is CommonJS, and `.js` is loaded under Node's usual module-type rules. TypeScript (`.ts`) is not supported.
- Each extension lives in its own subdirectory.
- The `@microsoft/copilot-sdk` import is resolved automatically — you don't install it.

In Protected Mode, local sessions that opt into extensions add filesystem access for the
configured user extensions directory, validated project `.github/extensions/`, and their own
session's `extensions/` directory. Agent file tools, newly launched shell
commands, and extension provider processes share these additions.

These are additive permissions, not visibility restrictions. Independent workspace
or user grants remain effective. Loading and management modes control extension
capabilities, not source-file confidentiality. Managed policy still applies, and an automatic scope grant is withheld
if its subtree overlaps an explicit read-only or denied path, including absent
descendants and future glob matches.

Redirected scope children do not add permissions to their link targets. Scaffold
and workflow source writes check the effective filesystem policy and use pinned,
no-follow directory handles so directory replacement cannot redirect a write.

## Opting Into Request-Driven Activation

Place a version 1 `extension.json` beside `extension.mjs`, with
`"initialization": "module"` and nonempty `tools`, `canvases`, and/or `commands`
arrays using the existing native-tool, canvas, and command definitions. Command
declarations contain `name` and optional `description`; handlers remain in the
live `joinSession()` registration. Import the same declarations in the entrypoint
and add handlers so the advertised metadata cannot drift.
Omit absent optional fields rather than writing null. Missing metadata preserves
eager startup; invalid metadata also produces a diagnostic.

Only native-tool, canvas, and declared slash-command demand is supported.
Background event observers, hooks, MCP-only entrypoints, workflows, skills,
agents, and system-message changes must not depend on this activation mechanism.
An activated extension can register additional contributions during
`joinSession()`, including MCP servers and Dynamic Workflows. Do not declare
workflows in `extension.json`.

Finish `joinSession()` and required listener setup before awaited module import
completes. The bootstrap explicitly signals that boundary; the first operation and
its tool-start event wait for it. Consent-aware join waits are preserved, followed
by a bounded module-completion wait. Missing or mismatched actual registrations
reject the first call and withdraw the stale advertisement; failed operations are
never automatically replayed.

The sidecar is limited to 256 KiB. Unknown fields or unsupported versions also
produce diagnostics and preserve eager startup.

## The SDK

Extensions use `@microsoft/copilot-sdk` for all interactions with the CLI:

```js
import { joinSession } from "@microsoft/copilot-sdk/extension";

const session = await joinSession({
    tools: [
        /* ... */
    ],
    hooks: {
        /* ... */
    },
});
```

The `session` object provides methods for sending messages, logging to the timeline, listening to events, and accessing the RPC API. See the `.d.ts` files in the SDK package for full type information.

## Selecting sources from an SDK host

Session creation and owning resume accept `extensionSources` (`extension_sources` in Python and Rust).
Its optional `user`, `project`, and `plugins` booleans select extension sources without enabling extensions.
Keep the existing extension enablement, launch provider, and SDK-injection configuration.

- `user: false` excludes automatic personal-extension discovery.
- `project: false` excludes automatic project-extension discovery.
- `user: true` or `project: true` requests that extension source independently of general configuration discovery.
- `plugins: false` excludes extension contributions from all otherwise-active plugins, including host bundles. Their independently eligible servers, hooks, skills, agents, and instructions remain unchanged.

Omission or `{}` preserves existing source behavior. In particular, an explicit extension request
still discovers personal and project extensions with `enableConfigDiscovery: false` when those
source fields are omitted. `plugins: true` and omission permit eligible contributions but never
activate a plugin or read additional plugin configuration.

Source choices stay fixed while loaded. A warm resume with different overrides, including an
omitted reset, fails. Cold resume accepts new choices; omission restores defaults. Participants cannot configure sources.
Reload and reconciliation retain the effective session selection without rewriting shared preferences.
Session-owned extension authoring remains available when the subsystem permits it.

Existing managed policy, trust, permissions, and individual disables still apply. A supporting
runtime is required: hosts must pin or establish runtime support before relying on an exclusion.
An older runtime ignoring an unknown option is not a safe fallback.

## Requesting Sensitive Environment Variables

The CLI strips sensitive environment variables (for example `GITHUB_TOKEN`) from every extension process before it starts. An extension that needs one asks for it by name:

```js
import { joinSession } from "@microsoft/copilot-sdk/extension";

const session = await joinSession({
    requestedEnvironmentVariables: ["GITHUB_TOKEN"],
});

// Granted values are in process.env once joinSession resolves.
const token = process.env.GITHUB_TOKEN;
```

The CLI prompts the user with the extension's name and the exact list of variables requested. If the user approves, only those variables reach this extension and their values are written into `process.env` before `joinSession()` resolves. If the user denies, `joinSession()` rejects, the extension does not load, and its tools never reach the model.

An approval is remembered against the exact set of names the user saw, so an extension that later asks for an additional variable prompts again. Names that are unset, or that the CLI does not filter from extensions, are not prompted for.

An approved extension can pass a granted value to anything it starts, so ask only for what the extension genuinely needs.

## Further Reading

- `examples.md` — Practical code examples for tools, hooks, events, and complete extensions
- `workflows.md` — Authoring, running, resuming, and observing Dynamic Workflows
- `agent-author.md` — Step-by-step workflow for agents authoring extensions programmatically
