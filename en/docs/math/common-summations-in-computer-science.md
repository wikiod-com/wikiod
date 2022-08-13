---
title: "Common summations in computer science"
slug: "common-summations-in-computer-science"
draft: false
images: []
weight: 9718
type: docs
toc: true
---

## Gauss's sum: 1 + 2 + 3 + ... + n
The sum

> 1 + 2 + 3 + ... + n

Simplifies to 

> n(n+1) / 2.

Notice that this quantity is &Theta;(n<sup>2</sup>).

This shortcut arises frequently in the [analysis of algorithms][1] like insertion sort or selection sort. 

Numbers of the form n(n+1)/2 are called [the triangular numbers][2].


  
  [1]: https://www.wikiod.com/algorithm/getting-started-with-algorithm
  [2]: https://en.wikipedia.org/wiki/Triangular_number

## Sum of a geometric series: r^0 + r^1 + r^2 + ...
The sum of the geometric series

> r<sup>0</sup> + r<sup>1</sup> + r<sup>2</sup> + ... + r<sup>n-1</sup>

In the case where r &ne; 1, simplifies to (r<sup>n</sup> - 1) / (r - 1). If r &lt; 1, this sum is bounded from above by 1 / (1 - r).

If r = 1, this sum is rn.

## Sums of Fibonacci Numbers
The Fibonacci numbers are defined inductively as

* F<sub>0</sub> = 0
* F<sub>1</sub> = 1
* F<sub>n+2</sub> = F<sub>n</sub> + F<sub>n+1</sub>

The sum of the first n+1 Fibonacci numbers is given by

> F<sub>0</sub> + F<sub>1</sub> + F<sub>2</sub> + ... + F<sub>n</sub> = F<sub>n+2</sub> - 1.

This summation arises, among other places, in the analysis of Fibonacci heaps, where it's used to provide a lower bound on the number of nodes in each tree in the heap.

## Sums of Powers of Two: 1 + 2 + 4 + 8 + 16 + ...
The sum

> 2<sup>0</sup> + 2<sup>1</sup> + 2<sup>2</sup> + ... + 2<sup>n-1</sup>

simplifies to 2<sup>n</sup> - 1. This explains why the maximum value that can be stored in an unsigned 32-bit integer is 2<sup>32</sup> - 1.

## Fencepost Sums
Here we consider sums of the form

a + b + a + b + ... a

vs.

a + b + a + b + ... b


To visualize these sums, imagine a section of fence alternating between posts and rails.  Three scenarios are possible.

---

1) Imagine a section of fence with posts at each end, connected by rails.  n rails require n+1 posts.  Conversely p posts are joined by p-1 rails.

    |&mdash;|&mdash;|&mdash;|

---

2) Imagine a section of fence with a post at one end, but but an open rail at the other.  n rails require n posts.

    |&mdash;|&mdash;|&mdash;

    or

    &mdash;|&mdash;|&mdash;|

---

3) Imagine a section of fence with open rails at both ends.  n rails require n-1 posts.  Conversely, p posts are joined by p+1 rails.

    &mdash;|&mdash;|&mdash;

---

Calculations like this arise in situations such as the layout of graphical objects where the sizes of the objects must be summed and the spaces between the objects must also be summed.  In such situations it is important to be aware of whether or not the intent is to have spaces at each end.

The total width of such a fence will always be:

(width of post) x (number of posts) + (width of rail) x (number of rails)

But caution must be used in computing the number of posts and number of rails so as to avoid a so-called *off-by-one* error.  Mistakes of this kind are common.

## Sums of reciprocals: 1/1 + 1/2 + 1/3 + 1/4 + ...
The summation

> 1/1 + 1/2 + 1/3 + 1/4 + ... + 1/n

is equal to the nth [harmonic number][1], denoted H<sub>n</sub>. The nth harmonic number obeys the inequalities

> ln (n + 1) &le; H<sub>n</sub> &le; (ln n) + 1

and therefore H<sub>n</sub> = &Theta;(log n). The harmonic numbers often arise in the analysis of algorithms, with randomized quicksort being a particularly nice example.


  [1]: https://en.wikipedia.org/wiki/Harmonic_number

## Sums of reciprocal squares: 1/1 + 1/4 + 1/9 + 1/16 + 1/25 + ...
The summation

> 1/1 + 1/4 + 1/9 + 1/16 + ...

out to infinity converges to &pi;<sup>2</sup> / 6, and therefore any summation of the form

> 1/1 + 1/4 + 1/9 + 1/16 + ... + 1/n<sup>2</sup>

is &Theta;(1).

