---
title: "Contour Maps"
slug: "contour-maps"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Simple filled contour plotting
    import matplotlib.pyplot as plt
    import numpy as np
    
    # generate 101 x and y values between -10 and 10 
    x = np.linspace(-10, 10, 101)
    y = np.linspace(-10, 10, 101)
    
    # make X and Y matrices representing x and y values of 2d plane
    X, Y = np.meshgrid(x, y)
    
    # compute z value of a point as a function of x and y (z = l2 distance form 0,0)
    Z = np.sqrt(X ** 2 + Y ** 2)
    
    # plot filled contour map with 100 levels
    cs = plt.contourf(X, Y, Z, 100)
    
    # add default colorbar for the map
    plt.colorbar(cs)

Result:
[![contourf][1]][1]


  [1]: https://i.stack.imgur.com/sDATU.png

## Simple contour plotting
    import matplotlib.pyplot as plt
    import numpy as np

    # generate 101 x and y values between -10 and 10 
    x = np.linspace(-10, 10, 101)
    y = np.linspace(-10, 10, 101)
    
    # make X and Y matrices representing x and y values of 2d plane
    X, Y = np.meshgrid(x, y)
    
    # compute z value of a point as a function of x and y (z = l2 distance form 0,0)
    Z = np.sqrt(X ** 2 + Y ** 2)
    
    # plot contour map with 3 levels 
    # colors: up to 1 - blue,  from 1 to 4 - green, from 4 to 8 - red
    plt.contour(X, Y, Z, [1, 4, 8], colors=['b', 'g', 'r'])

Result:
[![contour][1]][1]


  [1]: https://i.stack.imgur.com/921UT.png

