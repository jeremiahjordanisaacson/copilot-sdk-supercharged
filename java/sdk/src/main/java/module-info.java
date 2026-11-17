/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * Microsoft Copilot SDK for Java.
 */
module com.microsoft.copilot.java {
    requires transitive com.fasterxml.jackson.annotation;
    requires com.fasterxml.jackson.core;
    requires transitive com.fasterxml.jackson.databind;
    requires com.fasterxml.jackson.datatype.jsr310;
    requires static org.jspecify;
    requires static java.compiler;
    requires com.sun.jna;
    requires java.net.http;
    requires java.logging;

    exports com.microsoft.copilot;
    exports com.microsoft.copilot.generated;
    exports com.microsoft.copilot.generated.rpc;
    exports com.microsoft.copilot.rpc;
    exports com.microsoft.copilot.tool;

    opens com.microsoft.copilot to com.fasterxml.jackson.databind;
    opens com.microsoft.copilot.generated to com.fasterxml.jackson.databind;
    opens com.microsoft.copilot.generated.rpc to com.fasterxml.jackson.databind;
    opens com.microsoft.copilot.rpc to com.fasterxml.jackson.databind;
    opens com.microsoft.copilot.ffi to com.sun.jna;

    provides javax.annotation.processing.Processor with com.microsoft.copilot.CopilotExperimentalProcessor,
            com.microsoft.copilot.tool.CopilotToolProcessor, com.microsoft.copilot.tool.CopilotResponseProcessor;
}
