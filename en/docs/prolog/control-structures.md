---
title: "Control structures"
slug: "control-structures"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Disjunction (logical OR), implicit vs. explict
Prolog tries alternative clauses for a predicate in the order of appearance:

    likes(alice, music).
    likes(bob, hiking).

    // Either alice likes music, or bob likes hiking will succeed.

The disjunction (OR) operator `;` can be used to express this in one rule:

    likes(P,Q) :-
        ( P = alice , Q = music ) ; ( P = bob , Q = hiking ).

Parentheses are important here for clarity. See [this Question on relative precedence](http://stackoverflow.com/questions/29060684/conjunction-vs-disjunction-precedence-in-prolog) for conjunction `,` and disjunction `;`.

## Conjunction (logical AND)
Conjunction (logical AND) is represented by the comma `,` operator (among other roles).

Conjunction between clauses can appear in a query:

    ?- X = 1, Y = 2.

Conjunction can also appear between the subgoal clauses in the body of a rule:

    triangleSides(X,Y,Z) :-
        X + Y > Z, X + Z > Y, Y + Z > X.




## Cut (remove choice points)
Sometimes it is desirable to prevent Prolog from backtracking into alternative solutions. The basic tool available to the programmer to stop prolog from continuing futher in its backtrack is the cut operator. consider the following.

    % (percent signs mean comments)
    % a is the parent of b, c, and d.
    parent(a,b).
    parent(a,c).
    parent(a,d).

Here the predicate `parent/2` succeeds more than once when

    ?- parent(a,X).

is called. To stop prolog from searching for more solutions after the first is found you would use the cut operator, like so.

    ?- parent(a,X), !.

This will have X equal to b (as it is the first possible solution) and look for no more solutions.

