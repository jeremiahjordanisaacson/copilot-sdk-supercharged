/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/// Converts the flat session.create / session.resume options built by
/// `-[CPSessionConfig toDictionary]` into the grouped SDK protocol 4 payload.
///
/// Root keys stay at the top level (`configDir` becomes `configDirectory`, `disableResume`
/// becomes `suppressResumeEvent`), every other known key moves to its option group (for
/// example `streaming` becomes `events.streaming`), unknown keys stay at the root, null
/// values are skipped (except `defaultProvider`, which clears the default on resume),
/// keys meant only for the other method are dropped, already-grouped objects are merged
/// with their members winning, and empty groups are not emitted. The legacy single
/// `provider` becomes the named provider "default" with Copilot API models disabled.
FOUNDATION_EXPORT NSDictionary<NSString *, id> *CPGroupSessionParams(NSDictionary<NSString *, id> *flat,
                                                                     BOOL isResume);

/// Applies the client mode "empty" defaults underneath the caller's own grouped values.
FOUNDATION_EXPORT NSDictionary<NSString *, id> *CPApplyEmptyModeDefaults(NSDictionary<NSString *, id> *params);

NS_ASSUME_NONNULL_END
