---
title: "Built-in types"
slug: "built-in-types"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Numbers
Elixir comes with **integers** and **floating point numbers**. 
An **integer literal** can be written in decimal, binary, octal and hexadecimal formats. 

    iex> x = 291
    291

    iex> x = 0b100100011
    291
    
    iex> x = 0o443
    291
    
    iex> x = 0x123
    291
As Elixir uses bignum arithmetic, **the range of integer is only limited by the available memory on the system**. 

**Floating point numbers** are double precision and follows IEEE-754 specification.
   

    iex> x = 6.8
    6.8
    
    iex> x = 1.23e-11
    1.23e-11
Note that Elixir also supports exponent form for floats.
      
    iex> 1 + 1
    2
    
    iex> 1.0 + 1.0
    2.0

First we added two integers numbers, and the result is an integer. Later we added two floating point numbers, and the result is a floating point number.

Dividing in Elixir always returns a floating point number:

    iex> 10 / 2
    5.0
In the same way, if you add, subtract or multiply an integer by a floating point number the result will be floating point:
    
    iex> 40.0 + 2
    42.0

    iex> 10 - 5.0
    5.0

    iex> 3 * 3.0
    9.0

For integer division, one can use the `div/2` function:
```
iex> div(10, 2)
5
```

## Atoms
Atoms are constants that represent a name of some thing. The value of an atom is it's name. An atom name starts with a colon.

    :atom   # that's how we define an atom

An atom's name is unique. Two atoms with the same names always are equal.

    iex(1)> a = :atom
    :atom

    iex(2)> b = :atom
    :atom

    iex(3)> a == b
    true

    iex(4)> a === b
    true

Booleans `true` and `false`, actually are atoms.

    iex(1)> true == :true
    true

    iex(2)> true === :true
    true

Atoms are stored in special atoms table. It's very important to know that this table is not garbage-collected. So, if you want (or accidentally it is a fact) constantly create atoms - it is a bad idea.

## Binaries and Bitstrings
Binaries in elixir are created using the Kernel.SpecialForms construct <a href="http://elixir-lang.org/docs/stable/elixir/Kernel.SpecialForms.html#%3C%3C%3E%3E/1">&lt;&lt;&gt;&gt;</a>.

They are a powerful tool which makes Elixir very useful for working with binary protocols and encodings.

Binaries and bitstrings are specified using a comma delimited list of integers or variable values, bookended by "<<" and ">>".  They are composed of 'units', either a grouping of bits or a grouping of bytes.  The default grouping is a single byte (8 bits), specified using an integer:

    <<222,173,190, 239>> # 0xDEADBEEF

Elixir strings also convert directly to binaries:

    iex> <<0, "foo">>
    <<0, 102, 111, 111>>

You can add "specifiers" to each "segment" of a binary, allowing you to encode:

 - Data Type
 - Size
 - Endianness


These specifiers are encoded by following each value or variable with the "::" operator:

    <<102::integer-native>>
    <<102::native-integer>> # Same as above
    <<102::unsigned-big-integer>>
    <<102::unsigned-big-integer-size(8)>>
    <<102::unsigned-big-integer-8>> # Same as above
    <<102::8-integer-big-unsigned>>
    <<-102::signed-little-float-64>> # -102 as a little-endian Float64
    <<-102::native-little-float-64>> # -102 as a Float64 for the current machine

The available data types you can use are:

 - integer
 - float
 - bits (alias for bitstring)
 - bitstring
 - binary
 - bytes (alias for binary)
 - utf8
 - utf16
 - utf32

Be aware that when specifying the 'size' of the binary segment, it varies according to the 'type' chosen in the segment specifier:

 - integer (default)   1 bit
 - float    1 bit
 - binary    8 bits



