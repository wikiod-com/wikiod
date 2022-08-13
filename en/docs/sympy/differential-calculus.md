---
title: "Differential Calculus"
slug: "differential-calculus"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Constrained Non-Linear Optimization
*Problem statement*:

Find the minimum (over `x`, `y`) of the function `f(x,y)`, subject to 
`g(x,y)=0`, where `f(x,y) = 2 * x**2 + 3 * y**2` and `g(x,y) = x**2 + y**2 - 4`.

*Solution*: We will solve this problem by performing the following steps:

 1. Specify the Lagrangian function for the problem
 2. Determine the Karush-Kuhn-Tucker (KKT) conditions 
 3. Find the `(x,y)` tuples that satisfy the KKT conditions
 4. Determine which of these `(x,y)` tuples correspond to the minimum of `f(x,y)`

 

First, define the optimization variables as well as objective and constraint functions:

    import sympy as sp
    x, y = sp.var('x,y',real=True);
    f = 2 * x**2 + 3 * y**2
    g = x**2 + y**2 - 4

Next, define the Lagrangian function which includes a Lagrange multiplier `lam` corresponding to the constraint

    lam = sp.symbols('lambda', real = True)
    L = f - lam* g

Now, we can compute the set of equations corresponding to the KKT conditions.

    gradL = [sp.diff(L,c) for c in [x,y]] # gradient of Lagrangian w.r.t. (x,y)
    KKT_eqs = gradL + [g]
    KKT_eqs

> `[-2*lambda*x + 4*x, -2*lambda*y + 6*y, x**2 + y**2 - 4]`

The potential minimizers of `f` (given `g=0`) are obtained by solving the `KKT_eqs` equations overs `x`, `y`, `lam`:

    stationary_points = sp.solve(KKT_eqs, [x, y, lam], dict=True) # solve the KKT equations
    stationary_points 

>     [{x: -2, y: 0, lambda: 2},
>      {x: 2, y: 0, lambda: 2},
>      {x: 0, y: -2, lambda: 3},
>      {x: 0, y: 2, lambda: 3}]

Finally, check the objective function for each of the above points to determine the minimum

    [f.subs(p) for p in stat_points]

    

> `[8, 8, 12, 12]`

It follows that the constrained minimum of `f` equals `8` and is achieved at `(x,y)=(-2,0)` and `(x,y)=(2,0)`.



