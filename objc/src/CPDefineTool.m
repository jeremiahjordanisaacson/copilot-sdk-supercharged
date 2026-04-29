/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import "CPDefineTool.h"

NSDictionary<NSString *, id> *CPDefineTool(
    NSString *name,
    NSString * _Nullable description,
    NSDictionary<NSString *, id> * _Nullable parameters,
    CPToolHandler handler
) {
    NSMutableDictionary *tool = [NSMutableDictionary dictionary];
    tool[@"name"] = name;
    if (description) tool[@"description"] = description;
    if (parameters) tool[@"parameters"] = parameters;
    tool[@"_handler"] = [handler copy];
    return [tool copy];
}

NSDictionary<NSString *, id> *CPDefineSimpleTool(
    NSString *name,
    NSString * _Nullable description,
    CPToolHandler handler
) {
    return CPDefineTool(name, description, nil, handler);
}
