/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import <Foundation/Foundation.h>
#import "CPTypes.h"

NS_ASSUME_NONNULL_BEGIN

/// Helper to define a tool with a name, description, JSON Schema parameters, and a handler block.
///
/// Usage:
/// ```objc
/// NSDictionary *tool = CPDefineTool(
///     @"get_weather",
///     @"Get weather for a city",
///     @{
///         @"type": @"object",
///         @"properties": @{
///             @"city": @{@"type": @"string", @"description": @"City name"}
///         },
///         @"required": @[@"city"]
///     },
///     ^(CPToolInvocation *invocation, void (^done)(CPToolResult *)) {
///         NSString *city = invocation.arguments[@"city"];
///         done([CPToolResult successWithText:
///             [NSString stringWithFormat:@"Weather in %@: 22C sunny", city]]);
///     }
/// );
/// ```
///
/// @param name The tool name.
/// @param description A description of what the tool does.
/// @param parameters A JSON Schema dictionary for the parameters (or nil for no parameters).
/// @param handler The handler block invoked when the tool is called.
/// @return A dictionary suitable for inclusion in a `CPSessionConfig.tools` array.
NSDictionary<NSString *, id> *CPDefineTool(
    NSString *name,
    NSString * _Nullable description,
    NSDictionary<NSString *, id> * _Nullable parameters,
    CPToolHandler handler
);

/// Defines a tool with no parameters.
///
/// @param name The tool name.
/// @param description A description of what the tool does.
/// @param handler The handler block invoked when the tool is called.
/// @return A dictionary suitable for inclusion in a `CPSessionConfig.tools` array.
NSDictionary<NSString *, id> *CPDefineSimpleTool(
    NSString *name,
    NSString * _Nullable description,
    CPToolHandler handler
);

NS_ASSUME_NONNULL_END
