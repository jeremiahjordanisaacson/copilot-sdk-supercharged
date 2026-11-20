use serde::Deserializer;
use serde::de::{DeserializeSeed, EnumAccess, Error, Expected, Unexpected, VariantAccess, Visitor};
use std::fmt;

// Generated unit enums share these visitors instead of expanding a pair of
// visitors and an identifier enum for each wire type.
pub(crate) fn deserialize_variant<'de, D: Deserializer<'de>>(
    deserializer: D,
    name: &'static str,
    variants: &'static [&'static str],
    other: Option<usize>,
) -> Result<usize, D::Error> {
    deserializer.deserialize_enum(
        name,
        variants,
        EnumVisitor {
            name,
            variants,
            other,
        },
    )
}

struct EnumVisitor {
    name: &'static str,
    variants: &'static [&'static str],
    other: Option<usize>,
}

impl<'de> Visitor<'de> for EnumVisitor {
    type Value = usize;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "enum {}", self.name)
    }

    fn visit_enum<A: EnumAccess<'de>>(self, data: A) -> Result<usize, A::Error> {
        let (index, variant) = data.variant_seed(Identifier {
            variants: self.variants,
            other: self.other,
        })?;
        variant.unit_variant()?;
        Ok(index)
    }
}

struct Identifier {
    variants: &'static [&'static str],
    other: Option<usize>,
}

impl<'de> DeserializeSeed<'de> for Identifier {
    type Value = usize;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<usize, D::Error> {
        deserializer.deserialize_identifier(self)
    }
}

struct VariantRange(usize);

impl Expected for VariantRange {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "variant index 0 <= i < {}", self.0)
    }
}

impl<'de> Visitor<'de> for Identifier {
    type Value = usize;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("variant identifier")
    }

    fn visit_u64<E: Error>(self, value: u64) -> Result<usize, E> {
        if value < self.variants.len() as u64 {
            Ok(value as usize)
        } else if let Some(other) = self.other {
            Ok(other)
        } else {
            Err(E::invalid_value(
                Unexpected::Unsigned(value),
                &VariantRange(self.variants.len()),
            ))
        }
    }

    fn visit_str<E: Error>(self, value: &str) -> Result<usize, E> {
        self.variants
            .iter()
            .position(|variant| *variant == value)
            .or(self.other)
            .ok_or_else(|| E::unknown_variant(value, self.variants))
    }

    fn visit_bytes<E: Error>(self, value: &[u8]) -> Result<usize, E> {
        self.variants
            .iter()
            .position(|variant| variant.as_bytes() == value)
            .or(self.other)
            .ok_or_else(|| E::unknown_variant(&String::from_utf8_lossy(value), self.variants))
    }
}

#[cfg(test)]
mod tests;
