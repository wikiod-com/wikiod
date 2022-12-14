---
title: "Algorithm Complexity"
slug: "algorithm-complexity"
draft: false
images: []
weight: 9520
type: docs
toc: true
---

All algorithms are a list of steps to solve a problem.  Each step has dependencies on some set of previous steps, or the start of the algorithm.  A small problem might look like the following:

[![enter image description here][1]][1]

This structure is called a directed acyclic graph, or DAG for short.  The links between each node in the graph represent dependencies in the order of operations, and there are no cycles in the graph.

How do dependencies happen?  Take for example the following code:

    total = 0
    for(i = 1; i < 10; i++)
        total = total + i

In this psuedocode, each iteration of the for loop is dependent on the result from the previous iteration because we are using the value calculated in the previous iteration in this next iteration.  The DAG for this code might look like this:

[![enter image description here][2]][2]

If you understand this representation of algorithms, you can use it to understand algorithm complexity in terms of work and span.

# Work
Work is the actual number of operations that need to be executed in order to achieve the goal of the algorithm for a given input size `n`.

# Span
Span is sometimes referred to as the critical path, and is the fewest number of steps an algorithm must make in order to achieve the goal of the algorithm.

The following image highlights the graph to show the differences between work and span on our sample DAG.

[![enter image description here][3]][3]

The work is the number of nodes in the graph as a whole.  This is represented by the graph on the left above.  The span is the critical path, or longest path from the start to the end.  When work can be done in parallel, the yellow highlighted nodes on the right represent span, the fewest number of steps required.  When work must be done serially, the span is the same as the work.

Both work and span can be evaluated independently in terms of analysis.  The speed of an algorithm is determined by the span.  The amount of computational power required is determined by the work.


  [1]: http://i.stack.imgur.com/qP043.png
  [2]: http://i.stack.imgur.com/7zrQI.png
  [3]: http://i.stack.imgur.com/LD7PU.png

## Big-Theta notation
Unlike Big-O notation, which represents only upper bound of the running time for some algorithm, Big-Theta is a tight bound; both upper and lower bound. Tight bound is more precise, but also more difficult to compute.

The Big-Theta notation is symmetric: `f(x) = ??(g(x)) <=> g(x) = ??(f(x))`

An intuitive way to grasp it is that `f(x) = ??(g(x))` means that the graphs of f(x) and g(x) grow in the same rate, or that the graphs 'behave' similarly for big enough values of x.

The full mathematical expression of the Big-Theta notation is as follows:<br />
??(f(x)) = {g: N<sub>0</sub> -> R and c<sub>1</sub>, c<sub>2</sub>, n<sub>0</sub> > 0, where c<sub>1</sub> < abs(g(n) / f(n)), for every n > n<sub>0</sub> and abs is the absolute value }

**An example**

If the algorithm for the input `n` takes `42n^2 + 25n + 4` operations to finish, we say that is `O(n^2)`, but is also `O(n^3)` and `O(n^100)`. However, it is `??(n^2)` and it is not `??(n^3)`, `??(n^4)` etc. Algorithm that is `??(f(n))` is also `O(f(n))`, but not vice versa!

**Formal mathematical definition**

`??(g(x))` is a set of functions.

`??(g(x)) = {f(x) such that there exist positive constants c1, c2, N such that 0 <= c1*g(x) <= f(x) <= c2*g(x) for all x > N}`

Because `??(g(x))` is a set, we could write `f(x) ??? ??(g(x))`
to indicate that `f(x)` is a member of `??(g(x))`. Instead, we will usually write
`f(x) = ??(g(x))` to express the same notion - that's the common way.

Whenever `??(g(x))` appears in a formula, we interpret it as standing for some anonymous function that we do not care to name. For example the equation `T(n) = T(n/2) + ??(n)`, means `T(n) = T(n/2) + f(n)` where `f(n)` is a function in the set `??(n)`.

Let `f` and `g` be two functions defined on some subset of the real numbers. We write `f(x) = ??(g(x))` as `x->infinity` if and only if there are positive constants `K` and `L` and a real number `x0` such that holds:

`K|g(x)| <= f(x) <= L|g(x)|` for all `x >= x0`.

The definition is equal to:

`f(x) = O(g(x)) and f(x) = ??(g(x))`

**A method that uses limits**

if `limit(x->infinity) f(x)/g(x) = c ??? (0,???)` i.e. the limit exists and it's positive, then `f(x) = ??(g(x))`

**Common Complexity Classes**

| Name         | Notation    | n = 10    | n = 100
| ------------ | ----------- | ---------:| -------------:
| Constant     | ??(1)        |         1 |             1
| Logarithmic  | ??(log(n))   |         3 |             7
| Linear       | ??(n)        |        10 |           100
| Linearithmic | ??(n*log(n)) |        30 |           700
| Quadratic    | ??(n^2)      |       100 |        10 000
| Exponential  | ??(2^n)      |     1 024 | 1.267650e+ 30
| Factorial    | ??(n!)       | 3 628 800 | 9.332622e+157

## Comparison of the asymptotic notations
Let `f(n)` and `g(n)` be two functions defined on the set of the positive real numbers, `c, c1, c2, n0` are positive real constants.

| Notation | f(n) = O(g(n)) | f(n) = ??(g(n)) | f(n) = ??(g(n)) | f(n) = o(g(n)) | f(n) = ??(g(n)) |
| ------ | ------ | ------ | ------ | ------ | ------ |
| Formal definition   | `??? c > 0, ??? n0 > 0 : ??? n ??? n0, 0 ??? f(n) ??? c g(n)` |  `??? c > 0, ??? n0 > 0 : ??? n ??? n0, 0 ??? c g(n) ??? f(n)`  |  `??? c1, c2 > 0, ??? n0 > 0 : ??? n ??? n0, 0 ??? c1 g(n) ??? f(n) ??? c2 g(n)`  |  `??? c > 0, ??? n0 > 0 : ??? n ??? n0, 0 ??? f(n) < c g(n)`  |  `??? c > 0, ??? n0 > 0 : ??? n ??? n0, 0 ??? c g(n) < f(n)`  |
| Analogy between the asymptotic comparison of `f, g` and real numbers `a, b` | `a ??? b` | `a ??? b` | `a = b` | `a < b` | `a > b` |
| Example | `7n + 10 = O(n^2 + n - 9)` | `n^3 - 34 = ??(10n^2 - 7n + 1)` | `1/2 n^2 - 7n = ??(n^2)` | `5n^2 = o(n^3)` | `7n^2 = ??(n)`  |
| Graphic interpretation |  [![O-notation][1]][1] | [![??-notation][2]][2] | [![??-notation][3]][3] |  |  |

The asymptotic notations can be represented on a Venn diagram as follows:
[![Asymptotic notations][4]][4]


# Links #

Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, Clifford Stein. Introduction to Algorithms.


  [1]: http://i.stack.imgur.com/AkEKr.png
  [2]: http://i.stack.imgur.com/5qDtj.png
  [3]: http://i.stack.imgur.com/RPdzC.png
  [4]: http://i.stack.imgur.com/v2eH3.png

## Big-Omega Notation
??-notation is used for asymptotic lower bound.


# Formal definition #

Let `f(n)` and `g(n)` be two functions defined on the set of the positive real numbers. We write `f(n) = ??(g(n))` if there are positive constants `c` and `n0` such that:

`0 ??? c g(n) ??? f(n) for all n ??? n0`.


# Notes #

`f(n) = ??(g(n))` means that `f(n)` grows asymptotically no slower than `g(n)`. 
Also we can say about `??(g(n))` when algorithm analysis is not enough for statement about `??(g(n))` or / and `O(g(n))`.


From the definitions of notations follows the theorem:

For two any functions `f(n)` and `g(n)` we have `f(n) = ??(g(n))` if and only if
`f(n) = O(g(n)) and f(n) = ??(g(n))`.

Graphically ??-notation may be represented as follows:

[![??-notation][1]][1]

For example lets we have `f(n) = 3n^2 + 5n - 4`. Then `f(n) = ??(n^2)`. It is also correct `f(n) = ??(n)`, or even `f(n) = ??(1)`.

Another example to solve perfect matching algorithm : If the number of vertices is odd then output "No Perfect Matching" otherwise try all possible matchings.

We would like to say the algorithm requires exponential time but in fact you cannot prove a `??(n^2)` lower bound using the usual definition of `??` since the algorithm runs in linear time for n odd. We should instead define `f(n)=??(g(n))` by saying for some constant `c>0`, `f(n)??? c g(n)` for infinitely many `n`. This gives a nice correspondence between upper and lower bounds: `f(n)=??(g(n))` iff `f(n) != o(g(n))`.

# References #

Formal definition and theorem are taken from the book "Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, Clifford Stein. Introduction to Algorithms".




  [1]: http://i.stack.imgur.com/5qDtj.png


