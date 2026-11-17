/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collections;
import java.util.List;
/** Grouped SessionToolRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionToolRequestOptions {

    @JsonProperty("definitions")
    private List<ToolDefinition> definitions;

    @JsonProperty("availableTools")
    private List<String> availableTools;

    @JsonProperty("excludedTools")
    private List<String> excludedTools;

    @JsonProperty("largeOutput")
    private LargeToolOutputConfig largeOutput;

    @JsonProperty("search")
    private ToolSearchConfig search;

    @JsonProperty("imageGeneration")
    private ImageGenerationConfig imageGeneration;

    /** Gets the definitions. @return the tool definitions */
    public List<ToolDefinition> getDefinitions() {
        return definitions == null ? null : Collections.unmodifiableList(definitions);
    }

    /** Sets the definitions. @param definitions the tool definitions */
    public SessionToolRequestOptions setDefinitions(List<ToolDefinition> definitions) {
        this.definitions = definitions;
        return this;
    }

    /** Gets available tools. @return the available tool names */
    public List<String> getAvailableTools() {
        return availableTools == null ? null : Collections.unmodifiableList(availableTools);
    }

    /** Sets available tools. @param availableTools the available tool names */
    public SessionToolRequestOptions setAvailableTools(List<String> availableTools) {
        this.availableTools = availableTools;
        return this;
    }

    /** Gets excluded tools. @return the excluded tool names */
    public List<String> getExcludedTools() {
        return excludedTools == null ? null : Collections.unmodifiableList(excludedTools);
    }

    /** Sets excluded tools. @param excludedTools the excluded tool names */
    public SessionToolRequestOptions setExcludedTools(List<String> excludedTools) {
        this.excludedTools = excludedTools;
        return this;
    }

    /** Gets large output config. @return the large output config */
    public LargeToolOutputConfig getLargeOutput() {
        return largeOutput;
    }

    /** Sets large output config. @param largeOutput the large output config */
    public SessionToolRequestOptions setLargeOutput(LargeToolOutputConfig largeOutput) {
        this.largeOutput = largeOutput;
        return this;
    }

    /** Gets tool-search config. @return the tool-search config */
    public ToolSearchConfig getSearch() {
        return search;
    }

    /** Gets image generation config. @return the image generation config */
    public ImageGenerationConfig getImageGeneration() {
        return imageGeneration;
    }

    /**
     * Sets image generation config. @param imageGeneration the image generation
     * config
     */
    public SessionToolRequestOptions setImageGeneration(ImageGenerationConfig imageGeneration) {
        this.imageGeneration = imageGeneration;
        return this;
    }

    /** Sets tool-search config. @param search the tool-search config */
    public SessionToolRequestOptions setSearch(ToolSearchConfig search) {
        this.search = search;
        return this;
    }
}
