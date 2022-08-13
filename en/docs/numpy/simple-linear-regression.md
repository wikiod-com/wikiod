---
title: "Simple Linear Regression"
slug: "simple-linear-regression"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

Fitting a line (or other function) to a set of data points.

## Using np.polyfit
We create a dataset that we then fit with a straight line $f(x) = m x + c$.

    npoints = 20
    slope = 2
    offset = 3
    x = np.arange(npoints)
    y = slope * x + offset + np.random.normal(size=npoints)
    p = np.polyfit(x,y,1)           # Last argument is degree of polynomial

To see what we've done:

    import matplotlib.pyplot as plt
    f = np.poly1d(p)                # So we can call f(x)
    fig = plt.figure()
    ax  = fig.add_subplot(111)
    plt.plot(x, y, 'bo', label="Data")
    plt.plot(x,f(x), 'b-',label="Polyfit")
    plt.show()

Note: This example follows the numpy documentation at https://docs.scipy.org/doc/numpy/reference/generated/numpy.polyfit.html quite closely.

## Using np.linalg.lstsq
We use the same dataset as with polyfit:

    npoints = 20
    slope = 2
    offset = 3
    x = np.arange(npoints)
    y = slope * x + offset + np.random.normal(size=npoints)

Now, we try to find a solution by minimizing the system of linear equations  A b = c by minimizing |c-A b|**2

    import matplotlib.pyplot as plt # So we can plot the resulting fit
    A = np.vstack([x,np.ones(npoints)]).T
    m, c = np.linalg.lstsq(A, y)[0] # Don't care about residuals right now
    fig = plt.figure()
    ax  = fig.add_subplot(111)
    plt.plot(x, y, 'bo', label="Data")
    plt.plot(x, m*x+c, 'r--',label="Least Squares")
    plt.show()

Note: This example follows the numpy documentation at https://docs.scipy.org/doc/numpy/reference/generated/numpy.linalg.lstsq.html quite closely.


