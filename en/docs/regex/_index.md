---
title : Regular Expressions Tutorial
slug : regular-expressions-tutorial
weight : 9674
draft : false
images : []
type : docs
---

For many programmers the *regex* is some sort of magical sword that they throw to solve any kind of text parsing situation. But this tool is nothing magical, and even though it's great at what it does, it's not a full featured programming language (*i.e.* it is **not** Turing-complete).

# What does 'regular expression' mean?

*Regular expressions* express a language defined by a *regular grammar* that can be solved by a *nondeterministic finite automaton* (NFA), where matching is represented by the states. 

A *regular grammar* is the most simple grammar as expressed by the [*Chomsky Hierarchy*](http://en.wikipedia.org/wiki/Chomsky_hierarchy).

[![Chomsky's hierarchy](https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Chomsky-hierarchy.svg/300px-Chomsky-hierarchy.svg.png)](http://en.wikipedia.org/wiki/Chomsky_hierarchy)

Simply said, a regular language is visually expressed by what an NFA can express, and here's a very simple example of NFA:

[![NFA example](https://upload.wikimedia.org/wikipedia/commons/thumb/f/f9/NFASimpleExample.svg/175px-NFASimpleExample.svg.png)](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton)

And the *Regular Expression* language is a textual representation of such an automaton. That last example is expressed by the following regex:

    ^[01]*1$

Which is matching any string beginning with `0` or `1`, repeating 0 or more times, that ends with a `1`. In other words, it's a regex to match odd numbers from their binary representation.

# Are all regex actually a *regular* grammar?

Actually they are not. Many regex engines have improved and are using [*push-down automata*](https://en.wikipedia.org/wiki/Pushdown_automaton), that can stack up, and pop down information as it is running. Those automata define what's called [*context-free* grammars](https://en.wikipedia.org/wiki/Context-free_grammar) in Chomsky's Hierarchy. The most typical use of those in non-regular *regex*, is the use of a recursive pattern for parenthesis matching.

A recursive regex like the following (that matches parenthesis) is an example of such an implementation:

    {((?>[^\(\)]+|(?R))*)}

(this example does not work with python's `re` engine, but with the [`regex` engine](https://pypi.python.org/pypi/regex), or with the [PCRE engine](http://www.pcre.org)).

# Resources

For more information on the theory behind Regular Expressions, you can refer to the following courses made available by MIT:

 * [Automata, Computability, and Complexity][1] 
 * [Regular Expressions & Grammars][2]
 * [Specifying Languages with Regular Expressions and Context-Free Grammars][3]

When you're writing or debugging a complex regex, there are online tools that can help visualize regexes as automatons, like the [debuggex site](http://debuggex.com).

[1]:http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-045j-automata-computability-and-complexity-spring-2011/lecture-notes/MIT6_045JS11_lec04.pdf
[2]:http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-005-elements-of-software-construction-fall-2011/lecture-notes/MIT6_005F11_lec05.pdf
[3]:http://www.saylor.org/site/wp-content/uploads/2012/01/CS304-2.1-MIT.pdf



