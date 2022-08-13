---
title: "Using Modern Prolog"
slug: "using-modern-prolog"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## CLP(FD) for integer arithmetic
Traditionally Prolog performed arithmetic using the `is` and `=:=` operators. However, several current Prologs offer CLP(FD) (Constraint Logic Programming over Finite Domains) as a cleaner alternative for integer arithmetic. CLP(FD) is based on storing the constraints that apply to an integer value and combining those together in memory.

CLP(FD) is an extension in most Prologs that support it, so must be loaded explicitly. Once it is loaded, the `#=` syntax can take the place of both `is` and `=:=`. For example, in SWI-Prolog:

    ?- X is 2+2.
    X = 4.

    ?- use_module(library(clpfd)).
    ?- X #= 2+2.
    X = 4.

Unlike `is`, `#=` is able to solve simple equations and unify in both directions:

    ?- 4 is 2+X.
    ERROR: is/2: Arguments are not sufficiently instantiated
    
    ?- 4 #= 2+X.
    X = 2.

CLP(FD) provides its own generator syntax.

    ?- between(1,100,X).
    X = 1;
    X = 2;
    X = 3...

    ?- X in 1..100.
    X in 1..100.

Note that the generator does not actually run: only the range constraint is stored, ready for later constraints to be combined with it. The generator can be forced to run (and brute force constraints) using the `label` predicate:

    ?- X in 1..100, label([X]).
    X = 1;
    X = 2;
    X = 3..

Using CLP can allow some intelligent reduction of brute force cases. For example, using old-style integer arithmetic:
    
    ?- trace.
    ?- between(1,10,X), Y is X+5, Y>10.
    ...
    Exit: (8) 6 is 1+5 ? creep
    Call: (8) 6 > 10 ? creep
    ...
    X = 6, Y = 11; ...

Prolog still loops through the values 1-5 even though it is mathematically provable from the given conditions that these values cannot be useful. Using CLP(FD):

    ?- X in 1..10, Y #= X+5, Y #> 10.
    X is 6..10,
    X+5 #= Y,
    Y is 11..15.

CLP(FD) immediately does the maths and works out the available ranges. Adding `label([Y])` will cause X to loop only through the useful values 6..10. In this toy example, this does not increase performance because with such a small range as 1-10, the algebra processing takes as long as the loop would have done; but when a larger range of numbers are being processed this may valuably reduce computation time.

Support for CLP(FD) is variable between Prologs. The acknowledged best development of CLP(FD) is in SICStus Prolog, which is commercial and expensive. SWI-Prolog and other open Prologs often have some implementation. Visual Prolog does not include CLP(FD) in its standard library, although extension libraries for it are available.


## Introduction
Many modern Prolog systems are in continuous development and have added new features to address classic shortcomings of the language. Unfortunately, many Prolog textbooks and even teaching courses still introduce only the outdated prolog. This topic is intended to illustrate how modern Prolog has overcome some of the problems and rather crufty syntax that appears in older Prolog and may still be being introduced.

## Forall instead of failure-driven loops
Some "classic" Prolog textbooks still use the confusing and error-prone failure-driven loop syntax where a `fail` construct is used to force backtracking to apply a goal to every value of a generator. For example, to print all numbers up to a given limit:

    fdl(X) :- between(1,X,Y), print(Y), fail.
    fdl(_).

The vast majority of Modern Prologs no longer require this syntax, instead providing a higher order predicate to address this.

    nicer(X) :- forall(between(1,X,Y), print(Y)).

Not only is this much easier to read, but if a goal that could fail was used in place of *print*, its failure would be correctly detected and passed on - whereas failures of the goals in a failure-driven loop are confused with the forced failure that drives the loop.

Visual Prolog has a custom syntactic sugar for these loops, combined with function predicates (see below):

    vploop(X) :- foreach Y = std::fromTo(1,X) do
                     console::write(X)
                 end foreach.

Although this looks like an imperative *for* loop, it still follows Prolog rules: in particular, each iteration of the *foreach* is its own scope.


## Function-style Predicates
Traditionally in Prolog, "functions" (with one output and bound inputs) were written as regular predicates:

    mangle(X,Y) :- Y is (X*5)+2.

This can create the difficulty that if a function-style predicate is called multiple times, it is necessary to "daisy chain" temporary variables.

    multimangle(X,Y) :- mangle(X,A), mangle(A,B), mangle(B,Y).

In most Prologs, it is possible to avoid this by writing an alternate infix operator to use in place of `is` which expands expressions including the alternative function.

    % Define the new infix operator
    :- op(900, xfy, <-).

    % Define our function in terms of the infix operator - note the cut to avoid
    % the choice falling through
    R <- mangle(X) :- R is (X*5)+2, !.

    % To make the new operator compatible with is..
    R <- X :-
        compound(X),            % If the input is a compound/function
        X =.. [OP, X2, X3],     % Deconstruct it
        R2 <- X2,               % Recurse to evaluate the arguments
        R3 <- X3,
        Expr =.. [OP, R2, R3],  % Rebuild a compound with the evaluated arguments
        R is Expr,              % And send it to is
        !.
    R <- X :- R is X, !.        % If it's not a compound, just use is directly

We can now write:

    multimangle(X,Y) :- X <- mangle(mangle(mangle(Y))).

However, some modern Prologs go further and offer a custom syntax for this type of predicate. For example, in Visual Prolog:

    mangle(X) = Y :- Y = ((X*5)+2).
    multimangle(X,Y) :- Y = mangle(mangle(mangle(X))).

Note that the `<-` operator and the functional-style predicate above still behave as *relations* - it is legal for them to have choice points and perform multiple unification. In the first example, we prevent this using cuts. In Visual Prolog, it is normal to use the functional syntax for relations and choice points are created in the normal way - for example, the goal `X = (std::fromTo(1,10))*10` succeeds with bindings X=10, X=20, X=30, X=40, etc.
 

## Flow/mode declarations
When programming in Prolog it is not always possible, or desirable, to create predicates which unify for every possible combination of parameters. For example, the predicate `between(X,Y,Z)` which expresses that Z is numerically between X and Y. It is easily implemented in the cases where X, Y, and Z are all bound (either Z is between X and Y or it is not), or where X and Y are bound and Z is free (Z unifies with all numbers between X and Y, or the predicate fails if Y<X); but in other cases, such as where X and Z are bound and Y is free, there are potentially an infinite number of unifications. Although this can be implemented, it usually would not be.

_Flow declaration_ or _mode declarations_ allow an explicit description of how predicates behave when called with different combinations of bound parameters. In the case of `between`, the declaration would be:

    %! between(+X,+Y,+Z) is semidet.
    %! between(+X,+Y,-Z) is nondet. 

Each line specifies one potential calling pattern for the predicate. Each argument is decorated with `+` to indicate cases where it is bound, or `-` to indicate cases where it is not (there are also other decorations available for more complex types such as tuples or lists that may be partially bound). The keyword after _is_ indicates the behavior of the predicate in that case, and may be one of these:

* `det` if the predicate always succeeds with no choice point. For example `add(+X,+Y,-Z)` is `det` because adding two given numbers X and Y will always have exactly one answer.
* `semidet` if the predicate either succeeds or fails, with no choice point. As above, `between(+X,+Y,+Z)` is `semidet` because Z is either between X and Y or it is not.
* `multi` if the predicate always succeeds, but may have choice points (but also may not). For example, `factor(+X,-Y)` would be `multi` because a number always has at least one factor - itself - but may have more.
* `nondet` if the predicate may succeed with choice points, or fail. For example, `between(+X,+Y,-Z)` is `nondet` because there may be several possible unifications of Z to numbers between X and Y, or if Y<X then there are no numbers between them and the predicate fails.

Flow/mode declarations can also be combined with argument labeling to clarify what terms mean, or with typing. For example, `between(+From:Int, +To:Int, +Mid:Int) is semidet`.

In pure Prologs, flow and mode declarations are optional and only used for documentation generation, but they can be extremely useful to help programmers identify the cause of instantiation errors.

In Mercury, flow and mode declarations (and types) are mandatory and are validated by the compiler. The syntax used is as above.

In Visual Prolog, flow and mode declarations and types are also mandatory and the syntax is different. The above declaration would be written as:

    between : (int From, int To, int Mid) determ (i,i,i) nondeterm (i,i,o).

The meaning is the same as above, but with the differences that:

* The flow/mode declarations are separated from the type declarations (since it is assumed that flow/mode for a single predicate will not vary with type overloading);
* `i` and `o` are used for `+` and `-` and are matched with the parameters based on ordering;
* The terms used are different. `det` becomes `procedure`, `semidet` becomes `determ`, and `nondet` becomes `nondeterm` (`multi` is still `multi`).

