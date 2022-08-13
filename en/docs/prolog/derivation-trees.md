---
title: "Derivation trees"
slug: "derivation-trees"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Proof tree
The proof tree (also search tree or derivation tree) is a tree that shows the execution of a Prolog program. This tree helps visualise the chronological backtracking process present in Prolog. The root of the tree represents the initial query and branches are created when choice points occur. Every node in the tree thus represents a goal. Branches only become leafs when either true/false was proven for the required (set of) goal(s) and search in Prolog is performed in a **left-to-right depth-first** fashion.

Consider following example:

    % Facts
    father_child(paul,chris).        % Paul is the father of Chris and Ellen
    father_child(paul,ellen).
    mother_child(ellen,angie).       % Ellen is the mother of Angie and Peter
    mother_child(ellen,peter).


    % Rules
    grandfather_grandchild(X,Y) :-
        father_child(X,Z),
        father_child(Z,Y).

    grandfather_grandchild(X,Y) :-
        father_child(X,Z),
        mother_child(Z,Y).

When we now query:

    ?- grandfather_grandchild(paul,peter).

following proof tree visualises the depth-first search process:

                                       ?- grandfather_grandchild(paul,peter).
                                           /                             \
                                          /                               \
      ?- father_child(paul,Z1),father_child(Z1,peter).            ?- father_child(paul,Z2),mother_child(Z2,peter).
                 /                   \                                    /                              \
          {Z1=chris}             {Z1=ellen}                         {Z2=chris}                        {Z2=ellen}
               /                       \                                /                                  \      
    ?- father_child(chris,peter).  ?- father_child(ellen,peter).  ?- mother_child(chris,peter). ?- mother_child(ellen,peter).
             |                         |                               |                               /              \ 
           fail                      fail                            fail                          fail(*)          success  


(*) fails for `mother_child(ellen,angie)` where 'angie' fails to match 'peter'


