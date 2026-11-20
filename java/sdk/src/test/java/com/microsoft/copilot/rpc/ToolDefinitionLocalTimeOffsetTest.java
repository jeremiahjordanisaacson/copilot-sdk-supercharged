/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.concurrent.ExecutionException;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.OptBoolean;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.microsoft.copilot.AllowCopilotExperimental;
import com.microsoft.copilot.tool.Param;

/**
 * Decoding of {@link LocalDateTime} and {@link LocalTime} tool arguments, whose
 * schemas advertise RFC 3339 {@code date-time} and {@code time}, through
 * {@link ToolDefinition#from} and the SDK's tool argument mapper.
 */
@AllowCopilotExperimental
class ToolDefinitionLocalTimeOffsetTest {

    private static final ObjectMapper JSON = new ObjectMapper();
    private static final String FAILS = "<fails>";

    record Meeting(LocalDateTime start, LocalTime alarm) {
    }

    record PatternMeeting(@JsonFormat(pattern = "dd/MM/yyyy HH:mm") LocalDateTime start) {
    }

    record StrictMeeting(@JsonFormat(lenient = OptBoolean.FALSE) LocalDateTime start) {
    }

    static Stream<Arguments> rows() {
        return Stream.of(
                // Values Jackson decodes itself
                Arguments.of(LocalDateTime.class, "\"2026-11-15T10:20:30\"", "2026-11-15T10:20:30"),
                Arguments.of(LocalDateTime.class, "\"2026-11-15T10:20:30Z\"", "2026-11-15T10:20:30"),
                Arguments.of(LocalDateTime.class, "\"2026-11-15T10:20:30.123456789Z\"",
                        "2026-11-15T10:20:30.123456789"),
                Arguments.of(LocalDateTime.class, "[2026,11,15,10,20]", "2026-11-15T10:20"),
                Arguments.of(LocalTime.class, "\"10:20:30\"", "10:20:30"),
                Arguments.of(LocalTime.class, "\"10:20\"", "10:20"),
                Arguments.of(LocalTime.class, "\"2026-11-15T10:20:30\"", "10:20:30"),
                Arguments.of(Meeting.class, "{\"start\":\"2026-11-15T10:20:30\",\"alarm\":\"10:20:30\"}",
                        "Meeting[start=2026-11-15T10:20:30, alarm=10:20:30]"),
                Arguments.of(PatternMeeting.class, "{\"start\":\"15/11/2026 10:20\"}",
                        "PatternMeeting[start=2026-11-15T10:20]"),
                Arguments.of(StrictMeeting.class, "{\"start\":\"2026-11-15T10:20:30\"}",
                        "StrictMeeting[start=2026-11-15T10:20:30]"),

                // RFC 3339 offsets Jackson rejects: decoded to their local part
                Arguments.of(LocalDateTime.class, "\"2026-11-15T10:20:30+01:00\"", "2026-11-15T10:20:30"),
                Arguments.of(LocalDateTime.class, "\"2026-11-15T10:20:30-05:00\"", "2026-11-15T10:20:30"),
                Arguments.of(LocalDateTime.class, "\"2026-11-15T10:20:30+00:00\"", "2026-11-15T10:20:30"),
                Arguments.of(LocalDateTime.class, "\"2026-11-15T10:20:30.5+01:00\"", "2026-11-15T10:20:30.500"),
                Arguments.of(LocalTime.class, "\"10:20:30Z\"", "10:20:30"),
                Arguments.of(LocalTime.class, "\"10:20:30+01:00\"", "10:20:30"),
                Arguments.of(LocalTime.class, "\"10:20:30-05:00\"", "10:20:30"),
                Arguments.of(LocalTime.class, "\"10:20:30.25+01:00\"", "10:20:30.250"),
                Arguments.of(Meeting.class, "{\"start\":\"2026-11-15T10:20:30+01:00\",\"alarm\":\"10:20:30Z\"}",
                        "Meeting[start=2026-11-15T10:20:30, alarm=10:20:30]"),

                // Still rejected: not RFC 3339, invalid, or a format the property chose
                Arguments.of(LocalDateTime.class, "\"2026-11-15T10:20:30+0100\"", FAILS),
                Arguments.of(LocalDateTime.class, "\"2026-11-15T10:20:30+01:00[Europe/Prague]\"", FAILS),
                Arguments.of(LocalDateTime.class, "\"2026-02-30T10:20:30+01:00\"", FAILS),
                Arguments.of(LocalDateTime.class, "\"2026-11-15\"", FAILS),
                Arguments.of(LocalDateTime.class, "\"tomorrow\"", FAILS),
                Arguments.of(LocalDateTime.class, "1763202030", FAILS),
                Arguments.of(LocalTime.class, "\"10:20:30+0100\"", FAILS),
                Arguments.of(LocalTime.class, "\"2026-11-15T10:20:30+01:00\"", FAILS),
                Arguments.of(LocalTime.class, "\"noon\"", FAILS),
                Arguments.of(PatternMeeting.class, "{\"start\":\"2026-11-15T10:20:30+01:00\"}", FAILS),
                Arguments.of(StrictMeeting.class, "{\"start\":\"2026-11-15T10:20:30Z\"}", FAILS),
                Arguments.of(StrictMeeting.class, "{\"start\":\"2026-11-15T10:20:30+01:00\"}", FAILS));
    }

    @ParameterizedTest(name = "{0} {1} -> {2}")
    @MethodSource("rows")
    void decodesArgument(Class<?> type, String value, String expected) throws Exception {
        assertEquals(expected, decode(type, JSON.readTree(value)));
    }

    private static <T> String decode(Class<T> type, JsonNode value) throws InterruptedException {
        ToolDefinition tool = ToolDefinition.from("probe", "Decodes one argument", Param.of(type, "v", "value"),
                v -> String.valueOf(v));
        ObjectNode args = JsonNodeFactory.instance.objectNode();
        args.set("v", value);
        try {
            return (String) tool.handler().invoke(new ToolInvocation().setArguments(args)).get();
        } catch (IllegalArgumentException | ExecutionException e) {
            return FAILS;
        }
    }
}
