---
title: "Contracts"
slug: "contracts"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

The assertions will be optimized away in an release build.

## Function contracts
Function contracts allow the programer to check for inconsistencies. Inconsistencies include invalid parameters, checks for the correct return value or an invalid state of the object.

The checks can happen before and after the body of the function or method is executed.

    void printNotGreaterThan42(uint number)
    in {
        assert(number < 42);
    }
    body {
        import std.stdio : writeln;
        writeln(number);
    }

The assertions will be optimized away in an release build.

## Function contracts
For example if an method is invoked the state of the object may not allow that a method is called with specific parameters or not at all.

    class OlderThanEighteen {
        uint age;

        final void driveCar()
        in {
             assert(age >= 18); // variable must be in range
        }
        body {
             // step on the gas
        }
    }

