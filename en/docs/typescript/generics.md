---
title: "Generics"
slug: "generics"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Syntax
 - The generic types declared within the triangle brackets: `<T>`
 - Constrainting the generic types is done with the extends keyword: `<T extends Car>`

The generic parameters are not available at runtime, they are just for the compile time.
This means you can't do something like this:
    
    class Executor<T, U> {
        public execute(executable: T): void {
            if (T instanceof Executable1) {    // Compilation error
                ...
            } else if (U instanceof Executable2){    // Compilation error
                ...
            }
        }
    }


However, class information is still preserved, so you can still test for the type of a variable as you have always been able to:

    class Executor<T, U> {
        public execute(executable: T): void {
            if (executable instanceof Executable1) {
                ...
            } else if (executable instanceof Executable2){
                ...
            } // But in this method, since there is no parameter of type `U` it is non-sensical to ask about U's "type"
        }
    }



## Generic Interfaces
# Declaring a generic interface
    interface IResult<T> {
        wasSuccessfull: boolean;
        error: T;
    }

    var result: IResult<string> = ....
    var error: string = result.error;

# Generic interface with multiple type parameters
    interface IRunnable<T, U> {
        run(input: T): U;
    }

    var runnable: IRunnable<string, number> = ...
    var input: string;
    var result: number = runnable.run(input);

# Implementing a generic interface
    interface IResult<T>{
        wasSuccessfull: boolean;
        error: T;

        clone(): IResult<T>;
    }

Implement it with generic class:

    class Result<T> implements IResult<T> {
        constructor(public result: boolean, public error: T) {
        }

        public clone(): IResult<T> {
            return new Result<T>(this.result, this.error);
        }
    }

Implement it with non generic class:

    class StringResult implements IResult<string> {
        constructor(public result: boolean, public error: string) {
        }

        public clone(): IResult<string> {
            return new StringResult(this.result, this.error);
        }
    }

## Generic Class
    class Result<T> {
        constructor(public wasSuccessful: boolean, public error: T) {
        }

        public clone(): Result<T> {
           ...
        }
    }

    let r1 = new Result(false, 'error: 42');  // Compiler infers T to string
    let r2 = new Result(false, 42);           // Compiler infers T to number
    let r3 = new Result<string>(true, null);  // Explicitly set T to string
    let r4 = new Result<string>(true, 4);     // Compilation error because 4 is not a string

## Type parameters as constraints


## Generics Constraints
Simple constraint:

    interface IRunnable {
        run(): void;
    }

    interface IRunner<T extends IRunnable> {
        runSafe(runnable: T): void;
    }

More complex constraint:

    interface IRunnble<U> {
        run(): U;
    }

    interface IRunner<T extends IRunnable<U>, U> {
        runSafe(runnable: T): U;
    }
    
Even more complex:

    interface IRunnble<V> {
        run(parameter: U): V;
    }

    interface IRunner<T extends IRunnable<U, V>, U, V> {
        runSafe(runnable: T, parameter: U): V;
    }

Inline type constraints:

    interface IRunnable<T extends { run(): void }> {
        runSafe(runnable: T): void;
    }

## Generic Functions
In interfaces:

    interface IRunner {
        runSafe<T extends IRunnable>(runnable: T): void;
    }

In classes:

    class Runner implements IRunner {

        public runSafe<T extends IRunnable>(runnable: T): void {
            try {
                runnable.run();
            } catch(e) {
            }
        }

    }

Simple functions:

    function runSafe<T extends IRunnable>(runnable: T): void {
        try {
            runnable.run();
        } catch(e) {
        }
    }

## Using generic Classes and Functions:
Create generic class instance:

    var stringRunnable = new Runnable<string>();


Run generic function:

    function runSafe<T extends Runnable<U>, U>(runnable: T);

    // Specify the generic types:
    runSafe<Runnable<string>, string>(stringRunnable);

    // Let typescript figure the generic types by himself:
    runSafe(stringRunnable);

