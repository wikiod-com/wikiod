---
title: "Landau Notation"
slug: "landau-notation"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

All five classes in the Landau system describe *asymptotic* behaviour, i.e. the behaviour when the size of the problem tends to infinity. While this might look irrelevant to our – very finite – real world problems, experience has shown that behaviour of real world algorithms mirrors this infinite behaviour well enough on real data to be of practical use.

## Big O
Big O notation provides upper bounds for the growth of functions. Intuitively for `f ∊ O(g)`, `f` grows *at most* as fast as `g`.

Formally `f ∊ O(g)` if and only if there is a positive number `C` and a positive number ``n such that for all positive numbers `m > n` we have
> C⋅g(m) > f(m).

# Intuition of this definition
## Intuition regarding C
`C` is responsible for swallowing constant factors in the functions. If `h` is two times `f`, we still have `h ∊ O(g)` since `C` can be twice as big. For this there are two rationales:
- Easier notation: `f ∊ O(n)` is preferable to `f ∊ O(7.39 n)`.
- Abstraction: Any units of time are swallowed in these considerations because there is nothing to gain from them; they differ between machines and the algorithms can be evaluated free of that. Since `C` swallows constant factors, the complexity classes stay the same even on a machine ten times as fast.

## Intuition regarding n
`n` is responsible for swallowing initial turbulences. One algorithm might have an initialization overhead that is enormous for small inputs, but pays off in the long run. The choice of `n` allows sufficiently big inputs to get the focus while the initial stretch is ignored.

## Intuition regarding m
`m ` ranges over all values greater than `n` - to formalize the idea “from `n` onwards, this holds”.

