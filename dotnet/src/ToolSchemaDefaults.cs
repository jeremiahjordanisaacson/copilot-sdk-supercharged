/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using System.Globalization;
using System.Text.Json;

namespace Microsoft.Copilot;

/// <summary>
/// Leaves out of a tool's parameter schema each <c>default</c> the runtime would read back as a different number.
/// </summary>
/// <remarks>
/// The runtime reads a JSON integer exactly only within the signed and unsigned 64-bit ranges, and any other
/// number as a double. A default outside that, such as a <see cref="decimal"/> of 2^64 or with more significant
/// digits than a double holds, would reach the model rounded while the handler still applies the exact value
/// when the argument is omitted. Leaving it out keeps the model from being told a value the tool does not use.
/// </remarks>
internal static class ToolSchemaDefaults
{
    private const long MaxExponent = 1_000_000_000_000_000;

    // JSON Schema keywords whose values are subschemas, grouped by how they hold them.
    private static readonly HashSet<string> s_subschemaMaps = new(StringComparer.Ordinal)
    {
        "properties", "patternProperties", "$defs", "definitions", "dependentSchemas",
    };

    private static readonly HashSet<string> s_subschemaLists = new(StringComparer.Ordinal)
    {
        "allOf", "anyOf", "oneOf", "prefixItems",
    };

    private static readonly HashSet<string> s_subschemaValues = new(StringComparer.Ordinal)
    {
        "items", "additionalItems", "additionalProperties", "unevaluatedItems", "unevaluatedProperties",
        "contains", "propertyNames", "not", "if", "then", "else",
    };

    /// <summary>
    /// Returns <paramref name="schema"/> itself when no default would change, otherwise a copy without those defaults.
    /// </summary>
    public static JsonElement WithoutRoundedDefaults(JsonElement schema)
    {
        if (!HasRoundedDefault(schema))
        {
            return schema;
        }

        using var stream = new MemoryStream();
        using (var writer = new Utf8JsonWriter(stream))
        {
            WriteSchema(schema, writer);
        }

        using var document = JsonDocument.Parse(stream.ToArray());
        return document.RootElement.Clone();
    }

    private static bool HasRoundedDefault(JsonElement schema)
    {
        if (schema.ValueKind != JsonValueKind.Object)
        {
            return false;
        }

        foreach (var property in schema.EnumerateObject())
        {
            if (property.NameEquals("default"))
            {
                if (ContainsRoundedNumber(property.Value))
                {
                    return true;
                }
            }
            else if (s_subschemaMaps.Contains(property.Name) && property.Value.ValueKind == JsonValueKind.Object)
            {
                foreach (var subschema in property.Value.EnumerateObject())
                {
                    if (HasRoundedDefault(subschema.Value))
                    {
                        return true;
                    }
                }
            }
            else if ((s_subschemaLists.Contains(property.Name) || s_subschemaValues.Contains(property.Name))
                && property.Value.ValueKind == JsonValueKind.Array)
            {
                foreach (var subschema in property.Value.EnumerateArray())
                {
                    if (HasRoundedDefault(subschema))
                    {
                        return true;
                    }
                }
            }
            else if (s_subschemaValues.Contains(property.Name) && HasRoundedDefault(property.Value))
            {
                return true;
            }
        }

        return false;
    }

    private static void WriteSchema(JsonElement schema, Utf8JsonWriter writer)
    {
        if (schema.ValueKind != JsonValueKind.Object)
        {
            schema.WriteTo(writer);
            return;
        }

        writer.WriteStartObject();
        foreach (var property in schema.EnumerateObject())
        {
            if (property.NameEquals("default"))
            {
                if (!ContainsRoundedNumber(property.Value))
                {
                    property.WriteTo(writer);
                }
            }
            else if (s_subschemaMaps.Contains(property.Name) && property.Value.ValueKind == JsonValueKind.Object)
            {
                writer.WritePropertyName(property.Name);
                writer.WriteStartObject();
                foreach (var subschema in property.Value.EnumerateObject())
                {
                    writer.WritePropertyName(subschema.Name);
                    WriteSchema(subschema.Value, writer);
                }
                writer.WriteEndObject();
            }
            else if ((s_subschemaLists.Contains(property.Name) || s_subschemaValues.Contains(property.Name))
                && property.Value.ValueKind == JsonValueKind.Array)
            {
                writer.WritePropertyName(property.Name);
                writer.WriteStartArray();
                foreach (var subschema in property.Value.EnumerateArray())
                {
                    WriteSchema(subschema, writer);
                }
                writer.WriteEndArray();
            }
            else if (s_subschemaValues.Contains(property.Name))
            {
                writer.WritePropertyName(property.Name);
                WriteSchema(property.Value, writer);
            }
            else
            {
                property.WriteTo(writer);
            }
        }
        writer.WriteEndObject();
    }

    private static bool ContainsRoundedNumber(JsonElement value)
    {
        switch (value.ValueKind)
        {
            case JsonValueKind.Number:
                return IsRounded(value);
            case JsonValueKind.Object:
                foreach (var property in value.EnumerateObject())
                {
                    if (ContainsRoundedNumber(property.Value))
                    {
                        return true;
                    }
                }
                return false;
            case JsonValueKind.Array:
                foreach (var item in value.EnumerateArray())
                {
                    if (ContainsRoundedNumber(item))
                    {
                        return true;
                    }
                }
                return false;
            default:
                return false;
        }
    }

    /// <summary>
    /// Whether the runtime would read <paramref name="number"/> back as a different finite number.
    /// </summary>
    private static bool IsRounded(JsonElement number)
    {
        if (number.TryGetInt64(out _) || number.TryGetUInt64(out _))
        {
            return false;
        }

        // Anything else is read as the nearest double and written back as its shortest round-trip form.
        var literal = number.GetRawText();
        if (!double.TryParse(literal, NumberStyles.Float, CultureInfo.InvariantCulture, out var value)
            || double.IsInfinity(value)
            || double.IsNaN(value))
        {
            return false;
        }

        return !SameDecimalValue(literal, ShortestRoundTrip(value));
    }

    /// <summary>
    /// Formats <paramref name="value"/> with the fewest significant digits that read back as the same double.
    /// </summary>
    /// <remarks>
    /// <c>"R"</c> is not used because .NET Framework tries 15 digits and then jumps to 17, so
    /// <c>3.141592653589793</c> there comes out as <c>3.1415926535897931</c>, unlike the runtime's form.
    /// For each precision the correctly rounded digits are tried first, then their two neighbours, because the
    /// interval of decimals that read back as a double is narrower below it than above at a power of two.
    /// </remarks>
    private static string ShortestRoundTrip(double value)
    {
        var sign = value < 0 ? "-" : string.Empty;
        var magnitude = Math.Abs(value);
        for (var digits = 1; digits <= 17; digits++)
        {
            var scientific = magnitude.ToString("E" + (digits - 1).ToString(CultureInfo.InvariantCulture), CultureInfo.InvariantCulture);
            var parts = scientific.Split('E');
            var significand = long.Parse(parts[0].Replace(".", string.Empty), CultureInfo.InvariantCulture);
            var exponent = int.Parse(parts[1], NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) - (digits - 1);
            foreach (var candidate in new[] { significand, significand - 1, significand + 1 })
            {
                var text = sign + candidate.ToString(CultureInfo.InvariantCulture) + "E" + exponent.ToString(CultureInfo.InvariantCulture);
                if (double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture, out var parsed) && parsed == value)
                {
                    return text;
                }
            }
        }

        return value.ToString("R", CultureInfo.InvariantCulture);
    }

    private static bool SameDecimalValue(string left, string right) => Normalize(left) == Normalize(right);

    /// <summary>
    /// Writes a JSON number literal as sign, significant digits and power of ten, so equal values compare equal.
    /// </summary>
    private static (bool Negative, string Digits, long Exponent) Normalize(string literal)
    {
        var negative = literal.Length > 0 && literal[0] == '-';
        var unsigned = negative ? literal.Substring(1) : literal;
        var exponentIndex = unsigned.IndexOfAny(['e', 'E']);
        var mantissa = exponentIndex < 0 ? unsigned : unsigned.Substring(0, exponentIndex);
        long exponent = 0;
        if (exponentIndex >= 0)
        {
            var index = exponentIndex + 1;
            var exponentNegative = unsigned[index] == '-';
            if (unsigned[index] is '-' or '+')
            {
                index++;
            }

            for (; index < unsigned.Length; index++)
            {
                // Saturate far beyond any power of ten a double can be written with.
                exponent = Math.Min(exponent * 10 + (unsigned[index] - '0'), MaxExponent);
            }

            if (exponentNegative)
            {
                exponent = -exponent;
            }
        }

        var pointIndex = mantissa.IndexOf('.');
        if (pointIndex >= 0)
        {
            exponent -= mantissa.Length - pointIndex - 1;
            mantissa = mantissa.Remove(pointIndex, 1);
        }

        var digits = mantissa.TrimStart('0');
        if (digits.Length == 0)
        {
            return (false, string.Empty, 0);
        }

        var significant = digits.TrimEnd('0');
        return (negative, significant, exponent + digits.Length - significant.Length);
    }
}
