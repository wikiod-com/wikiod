---
title: "Reasoning about data"
slug: "reasoning-about-data"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

A new section called **Data Structures** was brought to life where explanations of certain structures + some simple example(s) of creation are provided. To keep its content concise and uncluttered, it should not contain any documentation about data manipulation.

Therefore, this section was renamed to "Reasoning about data" with as purpose the generalisation of reasoning about data in Prolog. This could include topics ranging from 'top-down inference' to 'traversal of lists', as well as many others. Because of its broad generalisation, clear subsections should be made!

## Recursion
Prolog doesn't have iteration, but all iteration can be rewritten using recursion. Recursion appears when a predicate contains a goal that refers to itself. When writing such predicates in Prolog, a standard recursive pattern always has at least two parts:

 -  **Base (non-recursive) clause**: Typically the base-case rule(s) will represent the smallest possible example(s) of the problem that you are trying to solve - a list with no members, or just one member, or if you're working with a tree structure, it might deal with an empty tree, or a tree with just one node in it, etc. It non-recursively describes the base of the recursive process.

 -  **Recursive (continuing) clause**: Contains any required logic including a call to itself, continuing recursion.

As an example we shall define the well-known predicate `append/3`. Viewed declaratively, `append(L1,L2,L3)` holds when the list `L3` is the result of appending lists `L1` and `L2`. When we try to figure out the declarative meaning of a predicate, we try to describe solutions for which the predicate holds. The difficulty here lies in trying to avoid any step-by-step recurring details while still keeping in mind the procedural behaviour the predicate should exhibit.

    % Base case
    append([],L,L).

    % Recursive clause
    append([X|L1],L2,[X|L3]) :- append(L1,L2,L3).

The base case declaratively states "any L appended to the empty list is L", note that this says nothing about L being empty – or even being a list (remember, in Prolog everything boils down to terms):

    ?- append(X,some_term(a,b),Z).
    X = [],
    Z = some_term(a, b).


For describing the recursive rule, although Prolog executes rules left-to-right, we omit the head for a second and look at the body first – reading the rule right-to-left:

<pre>
    <del>append([X|L1],L2,[X|L3])</del> :- append(L1,L2,L3).
</pre>

Now we say that if the body holds: “assuming that `append(L1,L2,L3)` holds”

<pre>
    append([X|L1],L2,[X|L3]) :- <del> append(L1,L2,L3). </del>
</pre>

Then so does the head: “then so does `append([X|L1],L2,[X|L3])`”

In plain English this simply translates to:

> Assuming L3 is the concatenation of L1 and L2,
> then [X followed by L3] is also the concatenation of [X followed by L1] and L2.

In a practical example:

> “Assuming [1,2,3] is the concatenation of [1] and [2,3], then [a,1,2,3] is also the concatenation of
    [a,1] and [2,3].”

Now let's look at some queries:

It's always a good idea to initially test your predicate with the **most general query** rather than providing it with a specific scenario test case. Think of it: because of Prolog's unification, we're not required to provide test data, we just hand it free variables!

    ?- append(L1,L2,L3).
    L1 = [],
    L2 = L3 ;                                   % Answer #1
    L1 = [_G1162],
    L3 = [_G1162|L2] ;                          % Answer #2
    L1 = [_G1162, _G1168],
    L3 = [_G1162, _G1168|L2] ;                  % Answer #3
    L1 = [_G1162, _G1168, _G1174],
    L3 = [_G1162, _G1168, _G1174|L2] ;          % Answer #4
    ...

Let's replace the free variable `_G1162`-like notation with alphabetical letters to get a better overview:

    ?- append(L1,L2,L3).
    L1 = [],
    L2 = L3 ;                                   % Answer #1
    L1 = [_A],
    L3 = [_A|L2] ;                              % Answer #2
    L1 = [_A, _B],
    L3 = [_A, _B|L2] ;                          % Answer #3
    L1 = [_A, _B, _C],
    L3 = [_A, _B, _C|L2] ;                      % Answer #4
    ...

In the first answer, the base case was pattern matched and Prolog instantiated `L1` to the empty list and unified `L2` and `L3` proving that `L3` is the concatenation of the empty list and L2.

At answer #2, through chronological backtracking, the recursive clause comes into play and Prolog tries to proof that some element in the head of `L1` concatenated with `L2` is `L3` with that same element in its list head. To do so, a new free variable `_A` is unified with the head of L1 and L3 is proven to now be `[_A|L2]`.

A new recursive call is made, now with `L1 = [_A]`. Once more, Prolog tries to proof that some element placed in the head of `L1`, concatenated with `L2` is `L3` with that same element in its head. Notice that `_A` is already the head of `L1`, which perfectly matches the rule, so now, through recursion, Prolog puts `_A` in front of a new free variable and we get `L1 = [_A,_B]` and `L3 = [_A,_B|L2]`

We clearly see the recursive pattern repeating itself and can easily see that, for example, the result of the 100th step in recursion would look like:

    L1 = [X1,X2,..,X99],
    L3 = [X1,X2,..,X99|L2]


Note: as is typical for good Prolog code, the recursive definition of `append/3` provides us not only with the possibility of *verifying* whether a list is the concatenation of two other lists, it also *generates* all possible answers satisfying the logical relations with either fully or partially instantiated lists.



## Accessing lists
**Member**

`member/2` has signature `member(?Elem, ?List)` and denotes `true` if `Elem` is a member of `List`. This predicate can be used to access variables in a list, where different solutions are retrieved through backtracking.

Example queries:

    ?- member(X, [1,2,3]).
    X = 1 ;
    X = 2 ;
    X = 3.

    ?- member(X,[Y]).
    X = Y.

    ?- member(X,Y).
    Y = [X|_G969] ;
    Y = [_G968, X|_G972] ;
    Y = [_G968, _G971, X|_G975] ;
    Y = [_G968, _G971, _G974, X|_G978]
    ...

**Pattern matching**

When the indices you need to access are small, pattern matching can be a good solution, e.g.:

    third([_,_,X|_], X).
    fourth([_,_,_,X|_], X).

