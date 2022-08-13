---
title: "Solvers"
slug: "solvers"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

As of version 1.0 of Sympy perhaps the main thing to understand about using its solvers is that '**solveset** will take over **solve** either internally or externally'. At this point solveset should already be used for solving univariate equations and systems of linear equations.

## Solving a univariate inequality
    >>> from sympy.solvers.inequalities import solve_univariate_inequality
    >>> from sympy import var
    >>> x=var('x')
    >>> solve_univariate_inequality(2*x**2-6>1,x,relational=False)
    (-oo, -sqrt(14)/2) U (sqrt(14)/2, oo)

The **relational=False** parameter simply indicates how the results are to be rendered. The default (**relational=True**) produces a result like this.

    >>> solve_univariate_inequality(2*x**2-6>1,x)
    Or(And(-oo < x, x < -sqrt(14)/2), And(sqrt(14)/2 < x, x < oo))

## Solving a linear Diophantine equation
[![Sample equation][1]][1]

sympy provides its solution as a Python set of expressions in terms of parametric variables, as shown here in the final line.

    >>> from sympy.solvers.diophantine import diophantine
    >>> from sympy import var
    >>> x,y,z=var('x y z')
    >>> diophantine(2*x+3*y-5*z-77)
    {(t_0, -9*t_0 - 5*t_1 + 154, -5*t_0 - 3*t_1 + 77)}



