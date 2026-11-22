/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

namespace Microsoft.Copilot;

internal static class ExecutionContextHelpers
{
    internal static AsyncFlowControl? SuppressFlow() =>
        ExecutionContext.IsFlowSuppressed() ? null : ExecutionContext.SuppressFlow();
}
