---
title: "Serde"
slug: "serde"
draft: false
images: []
weight: 9738
type: docs
toc: true
---

[Serde](https://serde.rs/) is a popular **ser**ialization and **de**serialization framework for Rust, used to convert *serialized data* (e.g. JSON and XML) to Rust structures and vice versa.

Serde supports many formats, including: JSON, YAML, TOML, BSON, Pickle and XML.

## Struct ↔ JSON
# main.rs

    extern crate serde;
    extern crate serde_json;

    // Import this crate to derive the Serialize and Deserialize traits.
    #[macro_use] extern crate serde_derive;

    #[derive(Serialize, Deserialize, Debug)]
    struct Point {
        x: i32,
        y: i32,
    }

    fn main() {
        let point = Point { x: 1, y: 2 };

        // Convert the Point to a packed JSON string. To convert it to
        // pretty JSON with indentation, use `to_string_pretty` instead.
        let serialized = serde_json::to_string(&point).unwrap();

        // Prints serialized = {"x":1,"y":2}
        println!("serialized = {}", serialized);

        // Convert the JSON string back to a Point.
        let deserialized: Point = serde_json::from_str(&serialized).unwrap();

        // Prints deserialized = Point { x: 1, y: 2 }
        println!("deserialized = {:?}", deserialized);
    }

# Cargo.toml

    [package]
    name = "serde-example"
    version = "0.1.0"
    build = "build.rs"

    [dependencies]
    serde = "0.9"
    serde_json = "0.9"
    serde_derive = "0.9"

## Default value for field
    extern crate serde;
    extern crate serde_json;
    #[macro_use] extern crate serde_derive;

    #[derive(Deserialize, Debug)]
    struct Request {
        // Use the result of a function as the default if "resource" is
        // not included in the input.
        #[serde(default="default_resource")]
        resource: String,

        // Use the type's implementation of std::default::Default if
        // "timeout" is not included in the input.
        #[serde(default)]
        timeout: Timeout,

        // Use a method from the type as the default if "priority" is not
        // included in the input. This may also be a trait method.
        #[serde(default="Priority::lowest")]
        priority: Priority,
    }

    fn default_resource() -> String {
        "/".to_string()
    }

    /// Timeout in seconds.
    #[derive(Deserialize, Debug)]
    struct Timeout(u32);
    impl Default for Timeout {
        fn default() -> Self {
            Timeout(30)
        }
    }

    #[derive(Deserialize, Debug)]
    enum Priority { ExtraHigh, High, Normal, Low, ExtraLow }
    impl Priority {
        fn lowest() -> Self { Priority::ExtraLow }
    }

    fn main() {
        let json = r#"
            [
              {
                "resource": "/users"
              },
              {
                "timeout": 5,
                "priority": "High"
              }
            ]
        "#;

        let requests: Vec<Request> = serde_json::from_str(json).unwrap();

        // The first request has resource="/users", timeout=30, priority=ExtraLow
        println!("{:?}", requests[0]);

        // The second request has resource="/", timeout=5, priority=High
        println!("{:?}", requests[1]);
    }


## Process an array of values without buffering them into a Vec
Suppose we have an array of integers and we want to figure out the maximum value without holding the whole array in memory all at once. This approach can be adapted to handle a variety of other situations in which data needs to be processed while being deserialized instead of after.

    extern crate serde;
    extern crate serde_json;
    #[macro_use] extern crate serde_derive;
    use serde::{de, Deserialize, Deserializer};

    use std::cmp;
    use std::fmt;
    use std::marker::PhantomData;
    
    #[derive(Deserialize)]
    struct Outer {
        id: String,

        // Deserialize this field by computing the maximum value of a sequence
        // (JSON array) of values.
        #[serde(deserialize_with = "deserialize_max")]
        // Despite the struct field being named `max_value`, it is going to come
        // from a JSON field called `values`.
        #[serde(rename(deserialize = "values"))]
        max_value: u64,
    }

    /// Deserialize the maximum of a sequence of values. The entire sequence
    /// is not buffered into memory as it would be if we deserialize to Vec<T>
    /// and then compute the maximum later.
    ///
    /// This function is generic over T which can be any type that implements
    /// Ord. Above, it is used with T=u64.
    fn deserialize_max<T, D>(deserializer: D) -> Result<T, D::Error>
        where T: Deserialize + Ord,
              D: Deserializer
    {
        struct MaxVisitor<T>(PhantomData<T>);

        impl<T> de::Visitor for MaxVisitor<T>
            where T: Deserialize + Ord
        {
            /// Return type of this visitor. This visitor computes the max of a
            /// sequence of values of type T, so the type of the maximum is T.
            type Value = T;
            
            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "a sequence of numbers")
            }

            fn visit_seq<V>(self, mut visitor: V) -> Result<T, V::Error>
                where V: de::SeqVisitor
            {
                // Start with max equal to the first value in the seq.
                let mut max = match visitor.visit()? {
                    Some(value) => value,
                    None => {
                        // Cannot take the maximum of an empty seq.
                        let msg = "no values in seq when looking for maximum";
                        return Err(de::Error::custom(msg));
                    }
                };

                // Update the max while there are additional values.
                while let Some(value) = visitor.visit()? {
                    max = cmp::max(max, value);
                }

                Ok(max)
            }
        }

        // Create the visitor and ask the deserializer to drive it. The
        // deserializer will call visitor.visit_seq if a seq is present in
        // the input data.
        let visitor = MaxVisitor(PhantomData);
        deserializer.deserialize_seq(visitor)
    }

    fn main() {
        let j = r#"
            {
              "id": "demo-deserialize-max",
              "values": [
                256,
                100,
                384,
                314,
                271
              ]
            }
        "#;

        let out: Outer = serde_json::from_str(j).unwrap();

        // Prints "max value: 384"
        println!("max value: {}", out.max_value);
    }

## Skip serializing field
    extern crate serde;
    extern crate serde_json;
    #[macro_use] extern crate serde_derive;

    use std::collections::BTreeMap as Map;

    #[derive(Serialize)]
    struct Resource {
        // Always serialized.
        name: String,

        // Never serialized.
        #[serde(skip_serializing)]
        hash: String,

        // Use a method to decide whether the field should be skipped.
        #[serde(skip_serializing_if="Map::is_empty")]
        metadata: Map<String, String>,
    }

    fn main() {
        let resources = vec![
            Resource {
                name: "Stack Overflow".to_string(),
                hash: "b6469c3f31653d281bbbfa6f94d60fea130abe38".to_string(),
                metadata: Map::new(),
            },
            Resource {
                name: "GitHub".to_string(),
                hash: "5cb7a0c47e53854cd00e1a968de5abce1c124601".to_string(),
                metadata: {
                    let mut metadata = Map::new();
                    metadata.insert("headquarters".to_string(),
                                    "San Francisco".to_string());
                    metadata
                },
            },
        ];

        let json = serde_json::to_string_pretty(&resources).unwrap();

        // Prints:
        //
        //    [
        //      {
        //        "name": "Stack Overflow"
        //      },
        //      {
        //        "name": "GitHub",
        //        "metadata": {
        //          "headquarters": "San Francisco"
        //        }
        //      }
        //    ]
        println!("{}", json);
    }

## Implement Deserialize for a custom map type
    // A Visitor is a type that holds methods that a Deserializer can drive
    // depending on what is contained in the input data.
    //
    // In the case of a map we need generic type parameters K and V to be
    // able to set the output type correctly, but don't require any state.
    // This is an example of a "zero sized type" in Rust. The PhantomData
    // keeps the compiler from complaining about unused generic type
    // parameters.
    struct MyMapVisitor<K, V> {
        marker: PhantomData<MyMap<K, V>>
    }
    
    impl<K, V> MyMapVisitor<K, V> {
        fn new() -> Self {
            MyMapVisitor {
                marker: PhantomData
            }
        }
    }
    
    // This is the trait that Deserializers are going to be driving. There
    // is one method for each type of data that our type knows how to
    // deserialize from. There are many other methods that are not
    // implemented here, for example deserializing from integers or strings.
    // By default those methods will return an error, which makes sense
    // because we cannot deserialize a MyMap from an integer or string.
    impl<K, V> de::Visitor for MyMapVisitor<K, V>
        where K: Deserialize,
              V: Deserialize
    {
        // The type that our Visitor is going to produce.
        type Value = MyMap<K, V>;
        
        // Deserialize MyMap from an abstract "map" provided by the
        // Deserializer. The MapVisitor input is a callback provided by
        // the Deserializer to let us see each entry in the map.
        fn visit_map<M>(self, mut visitor: M) -> Result<Self::Value, M::Error>
            where M: de::MapVisitor
        {
            let mut values = MyMap::with_capacity(visitor.size_hint().0);
    
            // While there are entries remaining in the input, add them
            // into our map.
            while let Some((key, value)) = visitor.visit()? {
                values.insert(key, value);
            }
    
            Ok(values)
        }
    
        // As a convenience, provide a way to deserialize MyMap from
        // the abstract "unit" type. This corresponds to `null` in JSON.
        // If your JSON contains `null` for a field that is supposed to
        // be a MyMap, we interpret that as an empty map.
        fn visit_unit<E>(self) -> Result<Self::Value, E>
            where E: de::Error
        {
            Ok(MyMap::new())
        }
    
        // When an unexpected data type is encountered, this method will 
        // be invoked to inform the user what is actually expected.
        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            write!(formatter, "a map or `null`")
        }
    }
    
    // This is the trait that informs Serde how to deserialize MyMap.
    impl<K, V> Deserialize for MyMap<K, V>
        where K: Deserialize,
              V: Deserialize
    {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where D: Deserializer
        {
            // Instantiate our Visitor and ask the Deserializer to drive
            // it over the input data, resulting in an instance of MyMap.
            deserializer.deserialize_map(MyMapVisitor::new())
        }
    }



## Serialize enum as string
    extern crate serde;
    extern crate serde_json;
    
    macro_rules! enum_str {
        ($name:ident { $($variant:ident($str:expr), )* }) => {
            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            pub enum $name {
                $($variant,)*
            }
    
            impl ::serde::Serialize for $name {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                    where S: ::serde::Serializer,
                {
                    // Serialize the enum as a string.
                    serializer.serialize_str(match *self {
                        $( $name::$variant => $str, )*
                    })
                }
            }
    
            impl ::serde::Deserialize for $name {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                    where D: ::serde::Deserializer,
                {
                    struct Visitor;
    
                    impl ::serde::de::Visitor for Visitor {
                        type Value = $name;
                        
                        fn expecting(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                            write!(formatter, "a string for {}", stringify!($name))
                        }
    
                        fn visit_str<E>(self, value: &str) -> Result<$name, E>
                            where E: ::serde::de::Error,
                        {
                            match value {
                                $( $str => Ok($name::$variant), )*
                                _ => Err(E::invalid_value(::serde::de::Unexpected::Other(
                                    &format!("unknown {} variant: {}", stringify!($name), value)
                                ), &self)),
                            }
                        }
                    }
    
                    // Deserialize the enum from a string.
                    deserializer.deserialize_str(Visitor)
                }
            }
        }
    }
    
    enum_str!(LanguageCode {
        English("en"),
        Spanish("es"),
        Italian("it"),
        Japanese("ja"),
        Chinese("zh"),
    });
    
    fn main() {
        use LanguageCode::*;
        let languages = vec![English, Spanish, Italian, Japanese, Chinese];
    
        // Prints ["en","es","it","ja","zh"]
        println!("{}", serde_json::to_string(&languages).unwrap());
    
        let input = r#" "ja" "#;
        assert_eq!(Japanese, serde_json::from_str(input).unwrap());
    }



## Serialize fields as camelCase
    extern crate serde;
    extern crate serde_json;
    #[macro_use] extern crate serde_derive;

    #[derive(Serialize)]
    struct Person {
        #[serde(rename="firstName")]
        first_name: String,
        #[serde(rename="lastName")]
        last_name: String,
    }

    fn main() {
        let person = Person {
            first_name: "Joel".to_string(),
            last_name: "Spolsky".to_string(),
        };

        let json = serde_json::to_string_pretty(&person).unwrap();

        // Prints:
        //
        //    {
        //      "firstName": "Joel",
        //      "lastName": "Spolsky"
        //    }
        println!("{}", json);
    }

## Implement Serialize for a custom map type
    impl<K, V> Serialize for MyMap<K, V>
        where K: Serialize,
              V: Serialize
    {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer
        {
            let mut state = serializer.serialize_map(Some(self.len()))?;
            for (k, v) in self {
                state.serialize_entry(k, v)?;
            }
            state.end()
        }
    }



## Handwritten generic type bounds
When deriving `Serialize` and `Deserialize` implementations for structs with generic type parameters, most of the time Serde is able to infer the correct [trait bounds](https://doc.rust-lang.org/book/traits.html#trait-bounds-on-generic-functions) without help from the programmer. It uses several heuristics to guess the right bound, but most importantly it puts a bound of `T: Serialize` on every type parameter `T` that is part of a serialized field and a bound of `T: Deserialize` on every type parameter `T` that is part of a deserialized field. As with most heuristics, this is not always right and Serde provides an escape hatch to replace the automatically generated bound by one written by the programmer.

    extern crate serde;
    extern crate serde_json;
    #[macro_use] extern crate serde_derive;

    use serde::de::{self, Deserialize, Deserializer};

    use std::fmt::Display;
    use std::str::FromStr;

    #[derive(Deserialize, Debug)]
    struct Outer<'a, S, T: 'a + ?Sized> {
        // When deriving the Deserialize impl, Serde would want to generate a bound
        // `S: Deserialize` on the type of this field. But we are going to use the
        // type's `FromStr` impl instead of its `Deserialize` impl by going through
        // `deserialize_from_str`, so we override the automatically generated bound
        // by the one required for `deserialize_from_str`.
        #[serde(deserialize_with = "deserialize_from_str")]
        #[serde(bound(deserialize = "S: FromStr, S::Err: Display"))]
        s: S,

        // Here Serde would want to generate a bound `T: Deserialize`. That is a
        // stricter condition than is necessary. In fact, the `main` function below
        // uses T=str which does not implement Deserialize. We override the
        // automatically generated bound by a looser one.
        #[serde(bound(deserialize = "Ptr<'a, T>: Deserialize"))]
        ptr: Ptr<'a, T>,
    }

    /// Deserialize a type `S` by deserializing a string, then using the `FromStr`
    /// impl of `S` to create the result. The generic type `S` is not required to
    /// implement `Deserialize`.
    fn deserialize_from_str<S, D>(deserializer: D) -> Result<S, D::Error>
        where S: FromStr,
              S::Err: Display,
              D: Deserializer
    {
        let s: String = try!(Deserialize::deserialize(deserializer));
        S::from_str(&s).map_err(|e| de::Error::custom(e.to_string()))
    }

    /// A pointer to `T` which may or may not own the data. When deserializing we
    /// always want to produce owned data.
    #[derive(Debug)]
    enum Ptr<'a, T: 'a + ?Sized> {
        Ref(&'a T),
        Owned(Box<T>),
    }

    impl<'a, T: 'a + ?Sized> Deserialize for Ptr<'a, T>
        where Box<T>: Deserialize
    {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where D: Deserializer
        {
            let box_t = try!(Deserialize::deserialize(deserializer));
            Ok(Ptr::Owned(box_t))
        }
    }

    fn main() {
        let j = r#"
            {
              "s": "1234567890",
              "ptr": "owned"
            }
        "#;

        let result: Outer<u64, str> = serde_json::from_str(j).unwrap();

        // result = Outer { s: 1234567890, ptr: Owned("owned") }
        println!("result = {:?}", result);
    }

## Implement Serialize and Deserialize for a type in a different crate
Rust's [coherence rule](https://doc.rust-lang.org/book/traits.html#rules-for-implementing-traits) requires that either the trait or the type for which you are implementing the trait must be defined in the same crate as the impl, so it is not possible to implement Serialize and Deserialize for a type in a different crate directly. The [newtype pattern](https://doc.rust-lang.org/book/structs.html#tuple-structs) and [`Deref` coercion](https://doc.rust-lang.org/book/deref-coercions.html) provide a way to implement Serialize and Deserialize for a type that behaves the same way as the one you wanted.

    use serde::{Serialize, Serializer, Deserialize, Deserializer};
    use std::ops::Deref;

    // Pretend this module is from some other crate.
    mod not_my_crate {
        pub struct Data { /* ... */ }
    }

    // This single-element tuple struct is called a newtype struct.
    struct Data(not_my_crate::Data);

    impl Serialize for Data {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer
        {
            // Any implementation of Serialize.
        }
    }

    impl Deserialize for Data {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where D: Deserializer
        {
            // Any implementation of Deserialize.
        }
    }

    // Enable `Deref` coercion.
    impl Deref for Data {
        type Target = not_my_crate::Data;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    // Now `Data` can be used in ways that require it to implement
    // Serialize and Deserialize.
    #[derive(Serialize, Deserialize)]
    struct Outer {
        id: u64,
        name: String,
        data: Data,
    }

