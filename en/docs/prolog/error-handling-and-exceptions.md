---
title: "Error handling and exceptions"
slug: "error-handling-and-exceptions"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Instantiation errors
An **instantiation** error is thrown if an argument is not sufficiently *instantiated*.

Critically, an instantiation error *cannot* be replaced by *silent failure*: Failing in such cases would mean that there is **no solution**, whereas an instantiation error means that an *instance* of the argument may participate in a&nbsp;solution.

This is in contrast to&mdash;for example&mdash;**domain error**, which can be replaced by silent failure without changing the declarative meaning of a&nbsp;program.

## General points about error handling
Prolog features **exceptions**, which are part of the Prolog ISO standard.

An exception can be thrown with `throw/1`, and caught with `catch/3`.

The ISO standard defines many cases in which errors must or may be thrown. The standardized exceptions are all of the form `error(E,_)`, where `E` indicates the error. Examples are `instantiation_error`, `domain_error` and `type_error`, which see.

An important predicate in connection with exceptions is `setup_call_cleanup/3`, which see.

## Cleaning up after exceptions
The predicate `setup_call_cleanup/3`, which is currently being considered for inclusion in the Prolog ISO&nbsp;standard and provided by an increasing number of implementations, lets us ensure that resources are correctly freed after an exception is thrown.

A typical invocation is:

    setup_call_cleanup(open(File, Mode, Stream), process_file(File), close(Stream))

Note that an exception or interrupt may even occur immediately after `open/3` is called in this case. For this reason, the `Setup` phase is performed *atomically*. In Prolog systems that only provide `call_cleanup/2`, this is much harder to express.

## Type and domain errors
A **type error** occurs if an argument is not of the expected *type*. Examples of types are:

- `integer`
- `atom`
- `list`.

If the predicate is of the expected type, but outside the expected *domain*, then a **domain&nbsp;error** is raised.

For example, a domain error is admissible if an integer between 0 and 15 is expected, but the argument is the integer&nbsp;20.

Declaratively, a type or domain error is equivalent to *silent failure*, since no instantiation can make a predicate whose argument is of the wrong type or domain succeed.

