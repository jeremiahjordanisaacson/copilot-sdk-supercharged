/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/// The SDK protocol version this SDK speaks; the startup check accepts exactly this version.
/// Protocol 4 (grouped session payloads) requires Copilot CLI 1.0.113 or newer.
static NSInteger const CPSdkProtocolVersion = 4;

NS_ASSUME_NONNULL_END
