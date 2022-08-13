---
title : algorithm Tutorial
slug : algorithm-tutorial
weight : 9079
draft : false
images : []
type : docs
---

# Introduction to Algorithms

Algorithms are ubiquitous in Computer Science and Software Engineering. Selection of appropriate algorithms and data structures improves our program efficiency in cost and time.

What is an algorithm? Informally, an algorithm is a procedure to accomplish a specific task. [ Skiena:2008:ADM:1410219] Specifically, an algorithm is a _well-defined_ computational procedure, which takes some value (or set of values) as **input** and produces some value, or a set of values, as **output**. An algorithm is thus a sequence of computational steps that transform the input into the output. Cormen et. al. does not explicitly remark that an algorithm does not necessarily require an input. [Cormen:2001:IA:580470]

Formally, an algorithm must satisfy five features: [Knuth:1997:ACP:260999]

1) _Finiteness_. An algorithm must always terminate after a finite number of steps.
2) _Definiteness_. Each step of an algorithm must be precisely defined; the actions to be carried out must be rigorously specified. It is this quality that [Cormen:2001:IA:580470] refers to with the term "well-defined".
3) _Input_. An algorithm has zero or more _inputs_. These are quantities that are given to the algorithm initially before it begins or dynamically as it runs.
4) _Output_. An algorithm has one or more _outputs_. These are quantities that have a specified relation to the inputs. We expect that an algorithm produces the same output when given the same input over and over again.
5) _Effectiveness_. An algorithm is also generally expected to be _effective_. Its operations must be sufficiently basic that they can be done exactly in principle and in a finite length of time by someone using pencil and paper.

A procedure that lacks finiteness but satisfies all other characteristics of an algorithm may be called a _computational method_. [Knuth:1997:ACP:260999]

