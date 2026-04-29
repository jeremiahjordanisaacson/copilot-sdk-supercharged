# Copyright (c) Microsoft Corporation. All rights reserved.

# Tool definition helpers for the Copilot SDK.
#
# Provides a convenient builder API for defining tools that can be exposed
# to the Copilot CLI, following the same pattern as other language SDKs.

require "json"

module CopilotSDK
  # Builder for defining tools with a block-based DSL.
  #
  # ```
  # tool = CopilotSDK.define_tool("get_weather") do |t|
  #   t.description = "Get the current weather for a location"
  #   t.parameter("location", "string", "City name", required: true)
  #   t.handler do |args, invocation|
  #     city = args["location"].as_s
  #     JSON::Any.new("Sunny in #{city}")
  #   end
  # end
  # ```
  class ToolBuilder
    getter name : String
    property description : String?

    @parameters : Hash(String, JSON::Any)
    @required : Array(String)
    @handler : ToolHandler?

    def initialize(@name : String)
      @parameters = {} of String => JSON::Any
      @required = [] of String
    end

    # Add a parameter to the tool schema.
    def parameter(name : String, type : String, description : String? = nil, required : Bool = false) : Nil
      prop = JSON.parse(JSON.build { |json|
        json.object do
          json.field "type", type
          json.field "description", description if description
        end
      })
      @parameters[name] = prop
      @required << name if required
    end

    # Set the handler block for the tool.
    def handler(&block : JSON::Any, ToolInvocation -> JSON::Any) : Nil
      @handler = block
    end

    # Build the ToolDefinition from the current builder state.
    def build_definition : ToolDefinition
      params = if @parameters.empty?
                 nil
               else
                 schema = JSON.parse(JSON.build { |json|
                   json.object do
                     json.field "type", "object"
                     json.field "properties" do
                       json.object do
                         @parameters.each do |key, value|
                           json.field key, value
                         end
                       end
                     end
                     unless @required.empty?
                       json.field "required" do
                         json.array do
                           @required.each { |r| json.string r }
                         end
                       end
                     end
                   end
                 })
                 schema
               end

      ToolDefinition.new(
        name: @name,
        description: @description,
        parameters: params
      )
    end

    # Returns the registered handler, or raises if none was set.
    def build_handler : ToolHandler
      @handler || raise "No handler defined for tool '#{@name}'"
    end
  end

  # Convenience method to define a tool with a block-based DSL.
  # Returns a tuple of {ToolDefinition, ToolHandler}.
  def self.define_tool(name : String, &block : ToolBuilder ->) : {ToolDefinition, ToolHandler}
    builder = ToolBuilder.new(name)
    yield builder
    {builder.build_definition, builder.build_handler}
  end
end
