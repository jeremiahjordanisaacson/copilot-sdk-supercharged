/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/// Manages a replaying CAPI proxy for E2E tests.
///
/// Spawns the shared test harness server (`test/harness/server.ts`) as a child
/// process and exposes its URL so SDK clients can connect to it during tests.
@interface CPCapiProxy : NSObject

/// The base URL of the running proxy, or nil when stopped.
@property (nonatomic, readonly, nullable) NSString *proxyUrl;

/// Start the replay proxy server and return the URL.
/// Also sets the `COPILOT_API_URL` environment variable.
- (NSString *)start;

/// Stop the proxy server gracefully.
- (void)stop;

/// Configure the proxy with a snapshot file path and working directory.
/// @param filePath Relative path to the snapshot YAML (e.g. @"test/snapshots/session/...").
/// @param workDir  Absolute path used as the working directory for the CLI.
- (void)configureWithFilePath:(NSString *)filePath workDir:(NSString *)workDir;

/// Locates the Copilot CLI for E2E runs, in order:
/// 1. `COPILOT_CLI_PATH`, when it names an existing file (CI points it at
///    `nodejs/node_modules/.bin/copilot`, which runs directly);
/// 2. `nodejs/node_modules/@github/copilot/npm-loader.js` (CLI 1.0.113 and newer);
/// 3. the legacy `nodejs/node_modules/@github/copilot-*/index.js` and
///    `nodejs/node_modules/@github/copilot/index.js` entry points.
/// The SDK runs `.js` entry points with node. Returns nil when no CLI is found.
+ (nullable NSString *)cliPathWithRepoRoot:(NSString *)repoRoot;

@end

NS_ASSUME_NONNULL_END
