---
title: "Primitive Data Types"
slug: "primitive-data-types"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Scalar Types
# Integers

**Signed**: `i8`, `i16`, `i32`, `i64`, `isize`

**Unsigned**: `u8`, `u16`, `u32`, `u64`, `usize`

The type of an integer literal, say `45`, will be automatically inferred from context. But to force it, we add a suffix: `45u8` (without space) will be typed `u8`.

Note: Size of `isize` and `usize` depend on the architecture. On 32-bit arch, it's 32-bits, and on 64-bit, you guessed it!

# Floating Points

`f32` and `f64`.

If you just write `2.0`, it is `f64` by default, unless the type inference determine otherwise!

To force `f32`, either define the variable with `f32` type, or suffix the literal: `2.0f32`.

# Booleans

`bool`, having values `true` and `false`.

# Characters

`char`, with values written as `'x'`. In single quotes, contain a single Unicode Scalar Value, which means that it is valid to have an emoji in it! Here are 3 more examples: `'😻'`, `'\u{3f}'`, `'\u{1d160}'`.

