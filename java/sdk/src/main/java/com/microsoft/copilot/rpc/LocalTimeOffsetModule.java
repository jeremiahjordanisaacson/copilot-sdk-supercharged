/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.BeanProperty;
import com.fasterxml.jackson.databind.DeserializationConfig;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.deser.BeanDeserializerModifier;
import com.fasterxml.jackson.databind.deser.std.DelegatingDeserializer;
import com.fasterxml.jackson.databind.exc.InvalidFormatException;
import com.fasterxml.jackson.databind.module.SimpleModule;

/**
 * Lets {@link LocalDateTime} and {@link LocalTime} tool arguments carry a UTC
 * offset.
 * <p>
 * Tool schemas advertise these types as {@code date-time} and {@code time}, so
 * models send RFC 3339 values such as {@code 2026-11-15T10:20:30+01:00}.
 * Jackson's deserializers for these types reject an offset, except a trailing
 * {@code Z} on a date-time, which they drop. When Jackson rejects a value that
 * is an RFC 3339 date-time or time with an offset, this decodes its local part
 * instead, the same result a trailing {@code Z} already gets. Jackson still
 * decodes every other value, and every property that sets its own
 * {@code @JsonFormat} pattern or leniency.
 */
final class LocalTimeOffsetModule extends SimpleModule {

    private static final long serialVersionUID = 1L;

    LocalTimeOffsetModule() {
        super("CopilotLocalTimeOffsetModule");
        setDeserializerModifier(new OffsetTolerantModifier());
    }

    private static final class OffsetTolerantModifier extends BeanDeserializerModifier {

        private static final long serialVersionUID = 1L;

        @Override
        public JsonDeserializer<?> modifyDeserializer(DeserializationConfig config, BeanDescription beanDesc,
                JsonDeserializer<?> deserializer) {
            Class<?> type = beanDesc.getBeanClass();
            if (type == LocalDateTime.class || type == LocalTime.class) {
                return new OffsetTolerantDeserializer(deserializer);
            }
            return deserializer;
        }
    }

    private static final class OffsetTolerantDeserializer extends DelegatingDeserializer {

        private static final long serialVersionUID = 1L;

        OffsetTolerantDeserializer(JsonDeserializer<?> delegate) {
            super(delegate);
        }

        @Override
        protected JsonDeserializer<?> newDelegatingInstance(JsonDeserializer<?> newDelegatee) {
            return new OffsetTolerantDeserializer(newDelegatee);
        }

        @Override
        public JsonDeserializer<?> createContextual(DeserializationContext ctxt, BeanProperty property)
                throws JsonMappingException {
            JsonDeserializer<?> contextual = super.createContextual(ctxt, property);
            JsonFormat.Value format = findFormatOverrides(ctxt, property, handledType());
            if (format != null && (format.hasPattern() || format.hasLenient())) {
                return ((DelegatingDeserializer) contextual).getDelegatee();
            }
            return contextual;
        }

        @Override
        public Object deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
            String text = p.hasToken(JsonToken.VALUE_STRING) ? p.getText() : null;
            try {
                return _delegatee.deserialize(p, ctxt);
            } catch (InvalidFormatException e) {
                Object local = text == null ? null : localPart(text.trim());
                if (local == null) {
                    throw e;
                }
                return local;
            }
        }

        private Object localPart(String text) {
            try {
                if (handledType() == LocalDateTime.class) {
                    return OffsetDateTime.parse(text, DateTimeFormatter.ISO_OFFSET_DATE_TIME).toLocalDateTime();
                }
                return OffsetTime.parse(text, DateTimeFormatter.ISO_OFFSET_TIME).toLocalTime();
            } catch (DateTimeParseException e) {
                return null;
            }
        }
    }
}
