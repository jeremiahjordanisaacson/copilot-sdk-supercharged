/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.ffi;

/**
 * JDK 25 multi-release variant of {@link ReaderThreadFactory}.
 */
final class ReaderThreadFactory {

    Thread create(Runnable task, String name) {
        return Thread.ofVirtual().name(name).unstarted(task);
    }
}
