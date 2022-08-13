---
title: "Regular Expression Engine Types"
slug: "regular-expression-engine-types"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## NFA
<!-- language-all: none -->

A NFA (Nondeterministic Finite Automaton) engine is *driven by the pattern*.

# Principle

The regex pattern is parsed into a tree.

The *current position* pointer is set to the start of the input string, and a match is attempted at this position. If the match fais, the position is incremented to the next character in the string and another match is attempted from this position. This process is repeated until a match is found or the end of the input string is reached.

# For each match attempt

The algorithm works by performing a traversal of the pattern tree for a given starting position. As it progresses through the tree, it updates the *current input position* by consuming matching characters.

If the algorithm encounters a tree node which does not match the input string at the current position, it will have to *backtrack*. This is performed by going back to the parent node in the tree, resetting the current input position to the value it had upon entering the parent node, and trying the next alternative branch.

If the algorithm manages to exit the tree, it reports a successful match. Otherwise, when all possibilities have been tried, the match fails.

# Optimizations

Regex engines usually apply some optimizations for better performance. For instance, if they determine that a match must start with a given character, they will attempt a match only at those positions in the input string where that character appears.

# Example

Match `a(b|c)a` against the input string `abeacab`:

The pattern tree could looks something like:

    CONCATENATION
        EXACT: a
        ALTERNATION
            EXACT: b
            EXACT: c
        EXACT: a
    
The match processes as follows:

    a(b|c)a      abeacab
    ^            ^
    
`a` is found in the input string, consume it and proceed to the next item in the pattern tree: the alternation. Try the first possibility: an exact `b`.
    
    a(b|c)a      abeacab
      ^           ^
    
`b` is found, so the alternation succeeds, consume it and proceed to the next item in the concatenation: an exact `a`:

    a(b|c)a      abeacab
          ^        ^
          
`a` is *not* found at the expected position. Backtrack to the alternation, reset the input position to the value it had upon entering the alternation for the first time, and try the *second* alternative:

    a(b|c)a      abeacab
        ^         ^
        
`c` is *not* found at this position. Backtrack to the concatenation. There are no other possibilities to try at this point, so there is no match at the start of the string.

Attempt a second match at the next input position:

    a(b|c)a      abeacab
    ^             ^
    
`a` does *not* match there. Attempt another match at the next position:

    a(b|c)a      abeacab
    ^              ^

No luck either. Advance to the next position.

    a(b|c)a      abeacab
    ^               ^

`a` matches, so consume it and enter the alternation:

    a(b|c)a      abeacab
      ^              ^

`b` does not match. Attempt the second alternative:

    a(b|c)a      abeacab
        ^            ^

`c` matches, so consume it and advance to the next item in the concatenation:

    a(b|c)a      abeacab
          ^           ^

`a` matches, and the end of the tree has been reached. Report a successful match:

    a(b|c)a      abeacab
                    \_/



## DFA
<!-- language-all: none -->

A DFA (Deterministic Finite Automaton) engine is *driven by the input*.

# Principle

The algorithm scans through the input string *once*, and remembers all possible paths in the regex which could match. For instance, when an alternation is encountered in the pattern, two new paths are created and attempted independently. When a given path does not match, it is dropped from the possibilities set.

# Implications

The matching time is bounded by the input string size. There is no backtracking, and the engine can find multiple matches simultaneously, even overlapping matches.

The main drawback of this method is the reduced feature set which can be supported by the engine, compared to the NFA engine type.

# Example

Match `a(b|c)a` against `abadaca`:

    abadaca      a(b|c)a
    ^            ^        Attempt 1      ==> CONTINUE
    
    abadaca      a(b|c)a
     ^           ^        Attempt 2      ==> FAIL
                   ^      Attempt 1.1    ==> CONTINUE
                     ^    Attempt 1.2    ==> FAIL
    
    abadaca      a(b|c)a
      ^          ^        Attempt 3      ==> CONTINUE
                       ^  Attempt 1.1    ==> MATCH
    
    abadaca      a(b|c)a
       ^         ^        Attempt 4      ==> FAIL
                   ^      Attempt 3.1    ==> FAIL
                     ^    Attempt 3.2    ==> FAIL
    
    abadaca      a(b|c)a
        ^        ^        Attempt 5      ==> CONTINUE
    
    abadaca      a(b|c)a
         ^       ^        Attempt 6      ==> FAIL
                   ^      Attempt 5.1    ==> FAIL
                     ^    Attempt 5.2    ==> CONTINUE
    
    abadaca      a(b|c)a
          ^      ^        Attempt 7      ==> CONTINUE
                       ^  Attempt 5.2    ==> MATCH

    abadaca      a(b|c)a
           ^       ^      Attempt 7.1    ==> FAIL
                     ^    Attempt 7.2    ==> FAIL



