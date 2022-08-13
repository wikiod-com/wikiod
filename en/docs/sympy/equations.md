---
title: "Equations"
slug: "equations"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Solve system of linear equations
    import sympy as sy
    
    x1, x2 = sy.symbols("x1 x2")
    
    equations = [
        sy.Eq( 2*x1 + 1*x2 ,  10 ),
        sy.Eq( 1*x1 - 2*x2 ,  11 )
    ]
    
    print sy.solve(equations)
    # Result: {x1: 31/5, x2: -12/5}

## Solve a single equation
    import sympy as sy
    
    # Symbols have to be defined before one can use them
    x = sy.S('x')
    
    # Definition of the equation to be solved
    eq=sy.Eq(x**2 + 2, 6)
    
    #Print the solution of the equation
    print sy.solve(eq)

The result printed will be:

    [-2, 2]




## Solve nonlinear set of equations numerically
    import sympy as sy
    
    x, y = sy.symbols("x y")
    
    # nsolve needs the (in this case: two) equations, the names of the variables 
    # (x,y) we try to evaluate solutions for, and an initial guess (1,1) for the 
    # solution
    print sy.nsolve((x**3+sy.exp(y)-4,x+3*y),(x,y),(1,1)) 

The result shown will be the solution for x and y:
    
    [  1.50281519319939]
    [-0.500938397733129]

