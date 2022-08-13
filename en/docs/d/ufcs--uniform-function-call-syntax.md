---
title: "UFCS - Uniform Function Call Syntax"
slug: "ufcs---uniform-function-call-syntax"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
- aThirdFun(anotherFun(myFun(), 42); // common notation (also valid)
- myFun().anotherFun(42).aThirdFun(); // UFCS
- myFun.anotherFun(42).aThirdFun; // empty braces can be removed

In a call `a.b(args...)`, if the type `a` does not have a method named `b`, then the compiler will try to rewrite the call as `b(a, args...)`.

## Checking if a Number is Prime
<!-- language: lang-d -->

    import std.stdio;

    bool isPrime(int number) {
        foreach(i; 2..number) {
            if (number % i == 0) {
                return false;
            }
        }

        return true;
    }

    void main() {
        writeln(2.isPrime);
        writeln(3.isPrime);
        writeln(4.isPrime);
        5.isPrime.writeln;
    }

## UFCS with ranges
<!-- language: lang-d -->

    void main() {
        import std.algorithm : group;
        import std.range;
        [1, 2].chain([3, 4]).retro; // [4, 3, 2, 1]
        [1, 1, 2, 2, 2].group.dropOne.front; //  tuple(2, 3u)        
    }

## UFCS with Durations from std.datetime
<!-- language: lang-d -->

    import core.thread, std.stdio, std.datetime;
    
    void some_operation() {
        // Sleep for two sixtieths (2/60) of a second.
        Thread.sleep(2.seconds / 60);
        // Sleep for 100 microseconds.
        Thread.sleep(100.usecs);
    }
    
    void main() {
        MonoTime t0 = MonoTime.currTime();
        some_operation();
        MonoTime t1 = MonoTime.currTime();
        Duration time_taken = t1 - t0;
    
        writeln("You can do some_operation() this many times per second: ",
                1.seconds / time_taken);
    }



