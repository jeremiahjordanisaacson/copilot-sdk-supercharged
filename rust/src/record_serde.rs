use serde::de::{DeserializeSeed, Error, Expected, IgnoredAny, MapAccess, SeqAccess, Visitor};
use serde::ser::SerializeStruct;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::{fmt, marker::PhantomData};

// The schema owns the names; the field tuple owns the decoding logic. A
// matching tuple can be reused across RPCs without changing their Rust types.
pub(crate) struct Record<T> {
    name: &'static str,
    fields: &'static [&'static str],
    marker: PhantomData<fn() -> T>,
}

pub(crate) fn read<'de, D, T>(
    deserializer: D,
    name: &'static str,
    fields: &'static [&'static str],
) -> Result<T, D::Error>
where
    D: Deserializer<'de>,
    Record<T>: Visitor<'de, Value = T>,
{
    deserializer.deserialize_struct(
        name,
        fields,
        Record {
            name,
            fields,
            marker: PhantomData,
        },
    )
}

#[derive(Clone, Copy)]
struct FieldIndex(&'static [&'static str]);

impl FieldIndex {
    fn find(self, value: &[u8]) -> Option<usize> {
        // Larger layouts are shared only when the schema's existing wire order
        // is lexical. Never reorder fields: sequence formats observe that order.
        if self.0.len() > 8 {
            self.0
                .binary_search_by(|key| key.as_bytes().cmp(value))
                .ok()
        } else {
            self.0.iter().position(|key| key.as_bytes() == value)
        }
    }
}

impl<'de> DeserializeSeed<'de> for FieldIndex {
    type Value = Option<usize>;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        deserializer.deserialize_identifier(self)
    }
}

impl Visitor<'_> for FieldIndex {
    type Value = Option<usize>;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("field identifier")
    }

    fn visit_u64<E: Error>(self, v: u64) -> Result<Self::Value, E> {
        Ok(usize::try_from(v).ok().filter(|&i| i < self.0.len()))
    }

    fn visit_str<E: Error>(self, v: &str) -> Result<Self::Value, E> {
        Ok(self.find(v.as_bytes()))
    }

    fn visit_bytes<E: Error>(self, v: &[u8]) -> Result<Self::Value, E> {
        Ok(self.find(v))
    }
}

// On a missing map key, Serde lets the *field's* decoder decide if absence
// is legal (including through aliases). An explicit null is decoded normally.
struct Absent<E>(&'static str, PhantomData<E>);

impl<'de, E: Error> Deserializer<'de> for Absent<E> {
    type Error = E;

    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, E> {
        visitor.visit_none()
    }

    fn deserialize_any<V: Visitor<'de>>(self, _: V) -> Result<V::Value, E> {
        Err(E::missing_field(self.0))
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf unit unit_struct newtype_struct seq tuple tuple_struct map
        struct enum identifier ignored_any
    }
}

struct RecordLength(&'static str, usize);

impl Expected for RecordLength {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "struct {} with {} element{}",
            self.0,
            self.1,
            if self.1 == 1 { "" } else { "s" }
        )
    }
}

pub(crate) struct OutputField<'a, T> {
    pub key: &'static str,
    pub value: &'a T,
    pub omit: bool,
}

pub(crate) trait WriteRecord {
    fn write<S: Serializer>(self, name: &'static str, serializer: S) -> Result<S::Ok, S::Error>;
}

macro_rules! record_visitors {
    ($(($ty:ident, $slot:ident, $index:literal)),+) => {
        impl<'de, $($ty: Deserialize<'de>),+> Visitor<'de> for Record<($($ty,)+)> {
            type Value = ($($ty,)+);

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "struct {}", self.name)
            }

            fn visit_seq<S: SeqAccess<'de>>(self, mut seq: S) -> Result<Self::Value, S::Error> {
                Ok(($(seq.next_element::<$ty>()?.ok_or_else(|| {
                    S::Error::invalid_length($index, &RecordLength(self.name, self.fields.len()))
                })?,)+))
            }

            fn visit_map<M: MapAccess<'de>>(self, mut map: M) -> Result<Self::Value, M::Error> {
                $(let mut $slot = None;)+
                while let Some(key) = map.next_key_seed(FieldIndex(self.fields))? {
                    match key {
                        $(Some($index) => {
                            if $slot.is_some() {
                                return Err(M::Error::duplicate_field(self.fields[$index]));
                            }
                            $slot = Some(map.next_value::<$ty>()?);
                        },)+
                        _ => { map.next_value::<IgnoredAny>()?; }
                    }
                }
                Ok(($(match $slot {
                    Some(value) => value,
                    None => $ty::deserialize(Absent(self.fields[$index], PhantomData))?,
                },)+))
            }
        }

        impl<$($ty: Serialize),+> WriteRecord for ($(OutputField<'_, $ty>,)+) {
            fn write<S: Serializer>(self, name: &'static str, serializer: S) -> Result<S::Ok, S::Error> {
                let ($($slot,)+) = self;
                let count = 0 $(+ usize::from(!$slot.omit))+;
                let mut record = serializer.serialize_struct(name, count)?;
                $(if $slot.omit {
                    record.skip_field($slot.key)?;
                } else {
                    record.serialize_field($slot.key, $slot.value)?;
                })+
                record.end()
            }
        }
    };
}

macro_rules! record_arities {
    (($($first:tt),*); $next:tt $(, $rest:tt)*) => {
        record_visitors!($($first,)* $next);
        record_arities!(($($first,)* $next); $($rest),*);
    };
    (($($first:tt),*);) => {};
}

record_arities!(() ;
    (A, a, 0), (B, b, 1), (C, c, 2), (D, d, 3), (E, e, 4), (F, f, 5), (G, g, 6), (H, h, 7),
    (T8, i, 8), (T9, j, 9), (T10, k, 10), (T11, l, 11), (T12, m, 12), (T13, n, 13),
    (T14, o, 14), (T15, p, 15), (T16, q, 16), (T17, r, 17), (T18, s, 18), (T19, t, 19),
    (T20, u, 20), (T21, v, 21), (T22, w, 22), (T23, x, 23), (T24, y, 24), (T25, z, 25),
    (T26, v26, 26), (T27, v27, 27), (T28, v28, 28), (T29, v29, 29), (T30, v30, 30), (T31, v31, 31)
);

macro_rules! wire_record {
    (@omit $value:expr) => { false };
    (@omit $value:expr, $when:ident) => { $value.$when() };
    ($name:ident { $($field:ident: $key:literal $(if $when:ident)?),+ $(,)? }) => {
        impl<'de> serde::Deserialize<'de> for $name {
            fn deserialize<De: serde::Deserializer<'de>>(deserializer: De) -> Result<Self, De::Error> {
                let ($($field,)+) = $crate::record_serde::read(deserializer, stringify!($name), &[$($key),+])?;
                Ok(Self { $($field),+ })
            }
        }

        impl serde::Serialize for $name {
            fn serialize<Ser: serde::Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
                $crate::record_serde::WriteRecord::write(
                    ($($crate::record_serde::OutputField {
                        key: $key,
                        value: &self.$field,
                        omit: $crate::record_serde::wire_record!(@omit self.$field $(, $when)?),
                    },)+),
                    stringify!($name),
                    serializer,
                )
            }
        }
    };
}
pub(crate) use wire_record;

#[cfg(test)]
mod tests;
