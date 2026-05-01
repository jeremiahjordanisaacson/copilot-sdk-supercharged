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

@end

NS_ASSUME_NONNULL_END
