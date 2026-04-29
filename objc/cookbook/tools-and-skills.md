# Tools and Skills

Define custom tools that the Copilot assistant can invoke.

## Defining a Tool

Use `CPDefineTool()` with a JSON Schema for parameters:

```objc
#import "CPDefineTool.h"

NSDictionary *searchTool = CPDefineTool(
    @"search_docs",
    @"Search internal documentation",
    @{
        @"type": @"object",
        @"properties": @{
            @"query": @{
                @"type": @"string",
                @"description": @"Search query"
            },
            @"limit": @{
                @"type": @"integer",
                @"description": @"Max results to return"
            }
        },
        @"required": @[@"query"]
    },
    ^(CPToolInvocation *invocation, void (^done)(CPToolResult *)) {
        NSString *query = invocation.arguments[@"query"];
        NSNumber *limit = invocation.arguments[@"limit"] ?: @10;

        // Perform your search logic
        NSString *results = [NSString stringWithFormat:
            @"Found %@ results for '%@'", limit, query];
        done([CPToolResult successWithText:results]);
    }
);
```

## Tools With No Parameters

```objc
NSDictionary *timeTool = CPDefineSimpleTool(
    @"get_current_time",
    @"Get the current date and time",
    ^(CPToolInvocation *invocation, void (^done)(CPToolResult *)) {
        NSDateFormatter *fmt = [[NSDateFormatter alloc] init];
        fmt.dateFormat = @"yyyy-MM-dd'T'HH:mm:ssZ";
        done([CPToolResult successWithText:[fmt stringFromDate:[NSDate date]]]);
    }
);
```

## Async Tool Handlers

Tool handlers can perform async work before calling the completion block:

```objc
NSDictionary *fetchTool = CPDefineTool(
    @"fetch_url",
    @"Fetch content from a URL",
    @{
        @"type": @"object",
        @"properties": @{
            @"url": @{@"type": @"string", @"description": @"URL to fetch"}
        },
        @"required": @[@"url"]
    },
    ^(CPToolInvocation *invocation, void (^done)(CPToolResult *)) {
        NSString *urlStr = invocation.arguments[@"url"];
        NSURL *url = [NSURL URLWithString:urlStr];

        NSURLSessionDataTask *task = [[NSURLSession sharedSession]
            dataTaskWithURL:url
            completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
                if (error) {
                    done([CPToolResult failureWithError:error.localizedDescription]);
                    return;
                }
                NSString *body = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
                done([CPToolResult successWithText:body ?: @"(empty)"]);
            }];
        [task resume];
    }
);
```

## Error Results

Return failures with descriptive error messages:

```objc
^(CPToolInvocation *invocation, void (^done)(CPToolResult *)) {
    NSString *path = invocation.arguments[@"path"];

    NSError *readError = nil;
    NSString *content = [NSString stringWithContentsOfFile:path
                                                  encoding:NSUTF8StringEncoding
                                                     error:&readError];
    if (readError) {
        done([CPToolResult failureWithError:
            [NSString stringWithFormat:@"Cannot read file: %@", readError.localizedDescription]]);
        return;
    }

    done([CPToolResult successWithText:content]);
}
```

## Registering Tools with a Session

```objc
CPSessionConfig *config = [[CPSessionConfig alloc] init];
config.tools = @[searchTool, timeTool, fetchTool];

[client createSession:config completion:^(CPCopilotSession *session, NSError *error) {
    // Tools are now available to the assistant
}];
```

## Tool Invocation Context

The `CPToolInvocation` object provides context about the call:

```objc
^(CPToolInvocation *invocation, void (^done)(CPToolResult *)) {
    NSLog(@"Tool: %@", invocation.toolName);
    NSLog(@"Call ID: %@", invocation.toolCallId);
    NSLog(@"Session: %@", invocation.sessionId);
    NSLog(@"Arguments: %@", invocation.arguments);

    done([CPToolResult successWithText:@"OK"]);
}
```
