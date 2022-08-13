---
title: "Getting started with Prolog Language"
slug: "getting-started-with-prolog-language"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Database Programming
Prolog categorizes everything into:

 - **Atoms** - Any sequence of characters that do not start with an uppercase alphabet. Eg - `a`, `b`, `okay`
 - **Numbers** - There is no special syntax for numbers, no declaration is required. Eg `1`, `22`, `35.8`
 - **Variables** - A string which starts with an uppercase character or underscore (`_`). Eg `X`, `Y`, `Abc`, `AA`
 - **Complex Terms** - They are made from a *functor* and a sequence of *arguments*. Name of a complex term is always an atom, while arguments can either be atoms or variables. Eg `father(john,doe)`, `relative(a)`, `mother(X,Y)`.

A logic database contains a set of *facts* and *rules*.

A complex term with only atoms as arguments is called a fact, while a complex term with variables as arguments is called a rule. 

Example of facts in Prolog:


    father_child(fred, susan).
    mother_child(hillary, joe).

Example of a rule in Prolog:

    child_of(X,Y):-
        father_child(Y,X)
        ;
        mother_child(Y,X).

Note that the `;` here is like the `or` operator in other languages.

Prolog is a declarative language and you can read this database as follows:

> *fred is the father of susan*

> *hillary is the mother of joe.*

> *For all `X` and `Y`, `X` is a child of `Y` if `Y` is a father of `X` or `Y` is a mother of `X`.*

In fact, a finite set of facts and or rules constitutes as a logic *program*.

The use of such a program is demonstrated by doing *queries*. Queries lets you retrieve information from a logic program.

To load the database into the interpreter (assuming that you've saved the database into the directory you are running the interpreter in) you simply enter:

    ?- [nameofdatabase].

replacing the `nameofdatabase` with the actual file name (note that here we exclude the `.pl` extension to the filename).

Example of queries in the interpreter for the program above and the results:

    ?- child_of(susan,fred).
    true

    ?- child_of(joe,hillary).
    true

    ?- child_of(fred,susan).
    false

    ?- child_of(susan,hillary).
    false

    ?- child_of(susan,X).
    X = fred

    ?- child_of(X,Y).
    X = susan,
    Y = fred ;
    X = joe,
    Y = hillary.

The queries above and their answers can be read as follows:

> *is susan a child of fred? - true*

> *is joe a child of hillary? - true*

> *is fred a child of susan? - false*

> *is susan a child of hillary? - false*

> *who is susan a child of? - fred*

This is how we program logic in Prolog. A logic program is more formally: a set of axioms, or rules, defining relations (aka predicates) between objects. An alternative way of interpreting the database above in a more formal logic way is:

> *The relation `father_child` holds between fred and susan*

> *The relation `mother_child` holds between hillary and joe*

> *For all `X` and `Y` the relation `child_of` holds between `X` and `Y` if the relation `father_child` holds between `Y` and `X`, or the relation `mother_child` holds between `Y` and `X`.*

## Hello, World

## *Hello, World* in the interactive interpreter

To print "Hello, World!" in the Prolog interpreter (here we are using `swipl`, the shell for SWI Prolog): 

    $ swipl
    <...banner...>
    ?- write('Hello, World!'), nl.

`?-` is the system prompt: it indicates that the system is ready for the user to enter a sequence of *goals* (i.e. a *query*) that must be terminated with a `.` (full stop).

Here the query `write('Hello World!'), nl` has two goals:

- `write('Hello World!')`: `'Hello World!'` has to be displayed **and** (`,`)
- a new line (`nl`) must follow.

[`write/1`](http://www.swi-prolog.org/pldoc/doc_for?object=write/1) (the `/1` is used to indicate that the predicate takes one argument) and [`nl/0`](http://www.swi-prolog.org/pldoc/doc_for?object=nl/0) are *built-in predicates* (the definition is provided in advance by the Prolog system). Built-in predicates provide facilities that cannot be obtained by pure Prolog definition or to save the programmer from having to define them.

The output:

> Hello, World!
>
> yes 

ends with `yes` meaning that the query has succeeded. In some systems `true` is printed instead of `yes`.

## *Hello, World* from a file

Open a new file called `hello_world.pl` and insert the following text: 

    :- initialization hello_world, halt.
    
    hello_world :-
        write('Hello, World!'), nl.

The `initialization` directive specifies that the goal `hello_world, halt` should be called when the file is loaded. `halt` exits the program. 

This file can then be executed by your Prolog executable. The exact flags depend on the Prolog system. If you are using SWI Prolog: 

    $ swipl -q -l hello_world.pl 

This will produce output `Hello, World!`. The `-q` flag suppresses the banner that usually displays when you call run `swipl`. The `-l` specifies a file to load.

## Installation or Setup
**SWI-Prolog**

*Windows and Mac:*

 - Download SWI-Prolog at the [official](http://www.swi-prolog.org/download/stable) website
 - Simply install by following the installer instructions.

*Linux (PPA):*

 - Add the PPA `ppa:swi-prolog/stable` to your system’s software sources (developers may choose for `ppa:swi-prolog/devel`) :

     - Open a terminal (Ctrl+Alt+T) and type:
    `sudo add-apt-repository ppa:swi-prolog/stable`

     - Afterwards, update the package information:
     `sudo apt-get update`

 - Now install SWI-Prolog through the package manager:
        `sudo apt-get install swi-prolog`

 - You can now start SWI-Prolog through the command-line with command `swipl`

## append/3
    append([], Bs, Bs).
    append([A|As], Bs, [A|Cs]) :-
        append(As, Bs, Cs).

[`append/3`](http://www.swi-prolog.org/pldoc/doc_for?object=append/3) is one of the most well-known Prolog relations. It defines a relation between three arguments and is true *if* the third argument is a list that denotes the concatenation of the lists that are specified in the first and second arguments.

Notably, and as is typical for good Prolog code, `append/3` can be used in *several directions*: It can be used to:

- *append* two fully or partially instantiated lists:

        ?- A = [1, 2, 3], B=[4, 5, 6], append(A, B, Y)
        Output:
        A = [1, 2, 3],
        B = [4, 5, 6],
        Y = [1, 2, 3, 4, 5, 6].
    
- *check* whether the relation is true for three fully instantiated lists:

        ?- A = [1, 2, 3], B = [4, 5], C = [1, 2, 3, 4, 5, 6], append(A, B, C)
        Output:
        false

- *generate* all possible ways to append two lists to a given list:

        ?- append(A, B, [1, 2, 3, 4]).
        Output:
        A = [],
        B = [1, 2, 3, 4] ;
        A = [1],
        B = [2, 3, 4] ;
        A = [1, 2],
        B = [3, 4] ;
        A = [1, 2, 3],
        B = [4] ;
        A = [1, 2, 3, 4],
        B = [] ;
        false.

## CLP(FD) Constraints
**CLP(FD) constraints** are provided by all serious Prolog implementations. They allow us to reason about **integers** in a pure way.

    ?- X #= 1 + 2.
    X = 3.

    ?- 5 #= Y + 2.
    Y = 3.


