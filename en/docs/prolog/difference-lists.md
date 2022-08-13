---
title: "Difference Lists"
slug: "difference-lists"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Difference Lists in Prolog denotes the concept to know the structure of a list *up to a point*. The remaining of the list can be left unbound until the complete evaluation of a predicate. A list where its end is unknown is referred as an *open list*, ended by a *hole*. This technique is especially useful to validate complex syntaxes or grammars. 

The well-known Definite Clause Grammars (DCG) is using Difference Lists to operate under the hood.

## Basic usage
Let's consider the predicate `sumDif/2`, verified if the structure of a list matches several constraints. The first term represents the list to analyze and the second term another list that holds the part of the first list that is unknown to our constraints.

For the demonstration, `sumDif/2` recognizes an arithmetic expression to sum *n* integers.
 
    sumDif([X, +|OpenList], Hole) :-
        integer(X),
        sumDif(OpenList, Hole).
    
We know the first element of the list to validate is an integer, here illustrated by `X`, followed by the symbol of the addition (`+`). The remaining of the list that still needs to be processed later on (`OpenList`) is left unvalidated at that level. `Hole` represents the part of the list we *don't need* to validate.

Let's give another definition of the predicate `sumDif/2` to complete the validation of the arithmetic expression:

    sumDif([X|Hole], Hole) :-
        integer(X).

We expect an integer called `X` directly at the start the open list. Interestingly, the remaining of the list `Hole` is left unknown and that's the whole purpose of the Difference Lists: the structure of the list is known up to a point.

Finally, the missing piece comes when a list is evaluated:

    ?- sumDif([1,+,2,+,3], []).
    true

This is when the predicate is used that the end of the list is mentioned, here `[]`, indicates the list does not contain additional elements.

## Evaluate an arithmetic expression
Let's define a grammar enabling us to perform additions, multiplications with the usage of parenthesis. To add more value to this example, we are going to compute the result of the arithmetic expression. Summary of the grammar:

> expression → times   
> expression → times '+' expression  
> times → element  
> times → element '*' times  
> element → "integer"   
> element → '(' expression ')'

All the predicates have an arity of 3, because they need to open list, the hole and the value of the arithmetic expression.

    expression(Value, OpenList, FinalHole) :-
        times(Value, OpenList, FinalHole).
    
    expression(SumValue, OpenList, FinalHole) :-
        times(Value1, OpenList, ['+'|Hole1]),
        expression(Value2, Hole1, FinalHole),
        plus(Value1, Value2, SumValue).
    
    times(Value, OpenList, FinalHole) :-
        element(Value, OpenList, FinalHole).
    
    times(TimesValue, OpenList, FinalHole) :-
        element(Value1, OpenList, ['*'|Hole1]),
        times(Value2, Hole1, FinalHole),
        TimesValue is Value1 * Value2.
    
    element(Value, [Value|FinalHole], FinalHole) :-
        integer(Value).
    
    element(Value, ['('|OpenList], FinalHole) :-
        expression(Value, OpenList, [')'|FinalHole]).

To properly explain the principle of *holes* and how the value is computed, let's take the second clause `expression`:

    expression(SumValue, OpenList, FinalHole) :-
        times(Value1, OpenList, ['+'|Hole1]),
        expression(Value2, Hole1, FinalHole),
        plus(Value1, Value2, SumValue).

The open list is denoted by the predicate `OpenList`. The first element to validate is *what comes before the addition symbol (`+`)*. When the first element is validated, it's directly followed by the addition symbol and by the continuation of the list, called `Hole1`. We know that `Hole1` is the next element to validate and can be another `expression`, hence `Hole1` is then the term given to the predicate `expression`.

The value is always represented in the first term. In this clause, it's defined by the sum of the `Value1` (everything before the addition symbol) and `Value2` (everything after the addition symbol).

Finally, the an expression can be evaluated.

    ?- expression(V, [1,+,3,*,'(',5,+,5,')'], []).
    V = 31

