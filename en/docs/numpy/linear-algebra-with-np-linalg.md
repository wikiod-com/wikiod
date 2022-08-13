---
title: "Linear algebra with np.linalg"
slug: "linear-algebra-with-nplinalg"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

As of version 1.8, several of the routines in `np.linalg` can operate on a 'stack' of matrices. That is, the routine can calculate results for multiple matrices if they're stacked together. For example, `A` here is interpreted as two stacked 3-by-3 matrices:

    np.random.seed(123)
    A = np.random.rand(2,3,3)
    b = np.random.rand(2,3)
    x = np.linalg.solve(A, b)
    
    print np.dot(A[0,:,:], x[0,:])
    # array([ 0.53155137,  0.53182759,  0.63440096])
    
    print b[0,:]
    # array([ 0.53155137,  0.53182759,  0.63440096])

The official `np` docs specify this via parameter specifications like `a : (..., M, M) array_like`.

## Solve linear systems with np.solve
Consider the following three equations:

    x0 + 2 * x1 + x2 = 4
             x1 + x2 = 3
    x0 +          x2 = 5

We can express this system as a matrix equation `A * x = b` with:

    A = np.array([[1, 2, 1],
                  [0, 1, 1],
                  [1, 0, 1]])
    b = np.array([4, 3, 5])

Then, use `np.linalg.solve` to solve for `x`:

    x = np.linalg.solve(A, b)
    # Out: x = array([ 1.5, -0.5,  3.5])

`A` must be a square and full-rank matrix: All of its rows must be be linearly independent. `A` should be invertible/non-singular (its determinant is not zero). For example, If one row of `A` is a multiple of another, calling `linalg.solve` will raise `LinAlgError: Singular matrix`:          

    A = np.array([[1, 2, 1], 
                  [2, 4, 2],   # Note that this row 2 * the first row
                  [1, 0, 1]])
    b = np.array([4,8,5])

Such systems can be solved with `np.linalg.lstsq`.

## Find the least squares solution to a linear system with np.linalg.lstsq
[Least squares][1] is a standard approach to problems with more equations than unknowns, also known as overdetermined systems.

Consider the four equations:

    x0 + 2 * x1 + x2 = 4
    x0 + x1 + 2 * x2 = 3
    2 * x0 + x1 + x2 = 5
    x0 + x1 + x2 = 4

We can express this as a matrix multiplication `A * x = b`:

    A = np.array([[1, 2, 1],
                  [1,1,2],
                  [2,1,1],
                  [1,1,1]])
    b = np.array([4,3,5,4])

Then solve with `np.linalg.lstsq`:

    x, residuals, rank, s = np.linalg.lstsq(A,b)

`x` is the solution, `residuals` the sum, `rank` the [matrix rank][2] of input `A`, and `s` the [singular values][3] of `A`. If `b` has more than one dimension, `lstsq` will solve the system corresponding to each column of `b`:

    A = np.array([[1, 2, 1],
                  [1,1,2],
                  [2,1,1],
                  [1,1,1]])
    b = np.array([[4,3,5,4],[1,2,3,4]]).T # transpose to align dimensions
    x, residuals, rank, s = np.linalg.lstsq(A,b)
    print x # columns of x are solutions corresponding to columns of b
    #[[ 2.05263158  1.63157895]
    # [ 1.05263158 -0.36842105]
    # [ 0.05263158  0.63157895]]
    print residuals # also one for each column in b
    #[ 0.84210526  5.26315789]

`rank` and `s` depend only on `A`, and are thus the same as above.
    
[1]: https://en.wikipedia.org/wiki/Least_squares
[2]: https://en.wikipedia.org/wiki/Rank_(linear_algebra)
[3]: https://en.wikipedia.org/wiki/Singular_value

