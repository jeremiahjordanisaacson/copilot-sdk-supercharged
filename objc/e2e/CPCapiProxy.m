/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import "CPCapiProxy.h"

@interface CPCapiProxy ()
@property (nonatomic, strong, nullable) NSTask *task;
@property (nonatomic, copy, nullable) NSString *proxyUrl;
@end

@implementation CPCapiProxy

- (NSString *)start {
    if (self.proxyUrl) {
        return self.proxyUrl;
    }

    // Resolve paths relative to this source file: objc/e2e/ -> repo root -> test/harness/
    NSString *e2eDir = [[NSString stringWithUTF8String:__FILE__] stringByDeletingLastPathComponent];
    NSString *repoRoot = [[e2eDir stringByAppendingPathComponent:@"../.."] stringByStandardizingPath];
    NSString *harnessDir = [repoRoot stringByAppendingPathComponent:@"test/harness"];
    NSString *serverPath = [harnessDir stringByAppendingPathComponent:@"server.ts"];

    // Spawn "npx tsx server.ts" inside the harness directory
    self.task = [[NSTask alloc] init];
#ifdef _WIN32
    self.task.launchPath = @"cmd.exe";
    self.task.arguments = @[@"/c", @"npx", @"tsx", serverPath];
#else
    self.task.launchPath = @"/usr/bin/env";
    self.task.arguments = @[@"npx", @"tsx", serverPath];
#endif
    self.task.currentDirectoryPath = harnessDir;

    NSPipe *stdoutPipe = [NSPipe pipe];
    self.task.standardOutput = stdoutPipe;
    // Inherit stderr so proxy diagnostics are visible during test runs

    [self.task launch];

    // Read stdout until we find "Listening: http://..."
    NSFileHandle *handle = stdoutPipe.fileHandleForReading;
    NSMutableData *buffer = [NSMutableData data];

    while (YES) {
        NSData *chunk = [handle availableData];
        if (chunk.length == 0) {
            break; // EOF
        }
        [buffer appendData:chunk];

        NSString *output = [[NSString alloc] initWithData:buffer encoding:NSUTF8StringEncoding];
        if (!output) continue;

        NSRegularExpression *regex =
            [NSRegularExpression regularExpressionWithPattern:@"Listening: (http://[^\\s]+)"
                                                     options:0
                                                       error:nil];
        NSTextCheckingResult *match =
            [regex firstMatchInString:output options:0 range:NSMakeRange(0, output.length)];

        if (match && [match rangeAtIndex:1].location != NSNotFound) {
            self.proxyUrl = [output substringWithRange:[match rangeAtIndex:1]];
            break;
        }
    }

    if (!self.proxyUrl) {
        [self.task terminate];
        @throw [NSException exceptionWithName:@"CPCapiProxyStartFailed"
                                       reason:@"Failed to read proxy URL from harness stdout"
                                     userInfo:nil];
    }

    // Export so the CLI can discover the proxy
    setenv("COPILOT_API_URL", self.proxyUrl.UTF8String, 1);

    return self.proxyUrl;
}

- (void)stop {
    if (!self.proxyUrl || !self.task) {
        return;
    }

    // Best-effort: POST /stop?skipWritingCache=true
    @try {
        NSString *stopUrlStr =
            [NSString stringWithFormat:@"%@/stop?skipWritingCache=true", self.proxyUrl];
        NSURL *stopUrl = [NSURL URLWithString:stopUrlStr];
        NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:stopUrl];
        request.HTTPMethod = @"POST";
        request.timeoutInterval = 5;

        dispatch_semaphore_t sem = dispatch_semaphore_create(0);
        [[NSURLSession.sharedSession dataTaskWithRequest:request
                                       completionHandler:^(NSData *d, NSURLResponse *r, NSError *e) {
            dispatch_semaphore_signal(sem);
        }] resume];
        dispatch_semaphore_wait(sem, dispatch_time(DISPATCH_TIME_NOW, 5 * NSEC_PER_SEC));
    } @catch (NSException *e) {
        // Ignore – best effort
    }

    // Wait briefly then terminate
    if (self.task.isRunning) {
        [self.task terminate];
        [self.task waitUntilExit];
    }

    self.task = nil;
    self.proxyUrl = nil;
    unsetenv("COPILOT_API_URL");
}

- (void)configureWithFilePath:(NSString *)filePath workDir:(NSString *)workDir {
    NSAssert(self.proxyUrl != nil, @"Proxy not started – call -start first");

    NSString *configUrlStr = [NSString stringWithFormat:@"%@/config", self.proxyUrl];
    NSURL *configUrl = [NSURL URLWithString:configUrlStr];

    NSDictionary *body = @{@"filePath": filePath, @"workDir": workDir};
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:body options:0 error:nil];

    NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:configUrl];
    request.HTTPMethod = @"POST";
    request.HTTPBody = jsonData;
    [request setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];
    request.timeoutInterval = 10;

    // Synchronous request (tests run serially)
    dispatch_semaphore_t sem = dispatch_semaphore_create(0);
    __block NSHTTPURLResponse *httpResponse = nil;
    __block NSError *reqError = nil;

    [[NSURLSession.sharedSession dataTaskWithRequest:request
                                   completionHandler:^(NSData *data,
                                                       NSURLResponse *response,
                                                       NSError *error) {
        httpResponse = (NSHTTPURLResponse *)response;
        reqError = error;
        dispatch_semaphore_signal(sem);
    }] resume];

    dispatch_semaphore_wait(sem, dispatch_time(DISPATCH_TIME_NOW, 10 * NSEC_PER_SEC));

    if (reqError) {
        @throw [NSException exceptionWithName:@"CPCapiProxyConfigFailed"
                                       reason:[NSString stringWithFormat:@"Config request failed: %@",
                                               reqError.localizedDescription]
                                     userInfo:nil];
    }
    if (httpResponse.statusCode != 200) {
        @throw [NSException exceptionWithName:@"CPCapiProxyConfigFailed"
                                       reason:[NSString stringWithFormat:@"Config returned HTTP %ld",
                                               (long)httpResponse.statusCode]
                                     userInfo:nil];
    }
}

@end
