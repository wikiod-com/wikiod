---
title: "Redis String datatype"
slug: "redis-string-datatype"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Redis provides a string datatype that is used to associate data with a particular key. Redis string are the most basic datatype available in Redis and one of the first datatypes that users learn to work with.

Strings are often associated with text data, but Redis strings are more like buffers that can be used to store a wide range of different data.  Redis strings can be used to represent integers, floating point numbers, bitmaps, text, and binary data.

## Syntax
- SET key value [EX seconds] [PX milliseconds] [NX|XX]
- INCR key
- INCRBY key increment
- INCRBYFLOAT key increment
- DECR key
- DECRBY key decrement


## Working with String as Integers
Several commands allow you to work with Strings representing integer values.

A user can set the integer value of a key using the command:

```
SET intkey 2
```
The set command will create the key if necessary or update it if it already exists.


The value of an integer key can be updated on the server using either the INCR or INCRBY commands.  INCR will increase the value of a key by 1 and INCRBY will increase the value of the key by the provided step value.

```
INCR intkey
INCRBY intkey 2
```
If the value of the key specified to INCR or INCRBY can't be expressed as an integer, Redis will return an error.  If the key doesn't exist, the key will be created and the operation will be applied to the default value of 0.

The DECR and DECRBY ccommands work in reverse to decrement the value.

## Working with Strings as Floating Point Numbers
Redis allow you to use the String data type to store floating point numbers.


A user can set the float value of a key using the command:

```
SET floatkey 2.0
```
The set command will create the key if necessary or update it if it already exists.

The value of the key can be updated on the server using either the INCRBYFLOAT command.  INCRBYFLOAT will increase the value of a key by the provided increment value.

```
INCRBYFLOAT floatkey 2.1
```
If the value of the key specified to INCRBYFLOAT can't be expressed as a floating point, Redis will return an error.  If the key doesn't exist, the key will be created and the operation will be applied to the default value of 0.0.

Keys can be decremented by passing a negative increment to the INCRBYFLOAT command.

