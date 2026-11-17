<!-- Copyright (c) Microsoft Corporation. All rights reserved. -->

# Rust SDK releases

The [`copilot-sdk` crate](https://crates.io/crates/copilot-sdk)
is published alongside the selected language SDKs. SDK and
runtime versions are numbered independently; each published crate pins the
runtime version it uses.

## Release channels

| Channel | Rust publication |
| --- | --- |
| Stable | Stable crate, released with selected SDKs and the CLI |
| Prerelease | Prerelease crate, released with selected SDKs, optionally with the CLI |
| Unstable | Development crate, released with selected SDKs and public runtime acquisition assets |

Find available crate versions on
[crates.io](https://crates.io/crates/copilot-sdk/versions).

## Runtime pins and release outputs

Published crates include `cli-version.txt` and `cli-version-in-process.txt`
with the exact runtime version, release location, and archive hashes.

Stable/prerelease runtime pins use
[`github/copilot-cli` releases](https://github.com/github/copilot-cli/releases).
Unstable runtime pins use the exact `runtime-<runtime-version>` release in
[`microsoft/copilot-sdk`](https://github.com/microsoft/copilot-sdk/releases).
Default consumer builds acquire both
the runtime platform package and standalone CLI archive without private-feed
credentials.

The `rust/v<SDK-version>` and `v<SDK-version>` tags identify the corresponding
public SDK source snapshot for stable, prerelease, and unstable versions.
Stable/prerelease versions also have a combined `v<SDK-version>` GitHub release.
Unstable source tags do not advance SDK `main` or create an SDK GitHub release
announcement. Their runtime assets remain available under the separate
`runtime-<runtime-version>` release.

## Cargo prerelease semantics

Cargo does not have npm distribution tags. Consumers opt into a prerelease
or unstable crate by explicitly requesting its version, for example:

```toml
copilot-sdk = "=1.0.0-unstable.123456.gabcdef0"
```

This is an illustrative version; select an available version from crates.io.
Stable requirements do not automatically select prereleases.

## Yanked versions

A crate version with a critical defect can be yanked. Yanking does not delete
the version or invalidate existing lockfiles, but Cargo excludes it from new
dependency resolutions. Consult the release notes and update affected
applications to a fixed version.
