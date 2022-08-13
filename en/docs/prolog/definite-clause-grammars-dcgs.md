---
title: "Definite Clause Grammars (DCGs)"
slug: "definite-clause-grammars-dcgs"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Anything at all: `... //0`
One of the most elementary DCG nonterminals is `... //0`, which can be read as "anything at all":

    ... --> [] | [_], ... .

It can be used to describe a list `Ls` that contains the element `E` via:

    phrase(( ..., [E], ... ), Ls)

## Parsing with DCGs
DCGs can be used for parsing. Best of all, *the same* DCG can often be used to both parse and *generate* lists that are being described. For example:

    sentence --> article, subject, verb, object.
    
    article --> [the].
    
    subject --> [woman] | [man].
    
    verb --> [likes] | [enjoys].
    
    object --> [apples] | [oranges].

Example queries:

    ?- phrase(sentence, Ls).
    Ls = [the, woman, likes, apples] ;
    Ls = [the, woman, likes, oranges] ;
    Ls = [the, woman, enjoys, apples] .

    ?- phrase(sentence, [the,man,likes,apples]).
    true .

## Extra goals
Extra goals enable to add processing to DCG clauses, for example, conditions that the elements of the list must satisfy.

The extra goals are observed between curly braces at the end of a DCG clause.

    % DCG clause requiring an integer
    int --> [X], {integer(X)}.

Usage:

    ?- phrase(int, [3]).
    true.
    
    ?- phrase(int, [a]).
    false.

## Extra arguments
The extra arguments add results to predicates of a DCG clause, by decorating the derivation tree. For example, it's possible to create a algebraic grammar that computes the value at the end.

Given a grammar that supports the operation addition:

    % Extra arguments are passed between parenthesis after the name of the DCG clauses.
    exp(C) --> int(A), [+], exp(B), {plus(A, B, C)}.
    exp(X) --> int(X).
    int(X) --> [X], {integer(X)}.

The result of this grammar can be validated and queried:

    ?- phrase(exp(X), [1,+,2,+,3]).
    X = 6 ;

