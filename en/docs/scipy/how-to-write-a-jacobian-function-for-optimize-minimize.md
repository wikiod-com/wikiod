---
title: "How to write a Jacobian function for optimize.minimize"
slug: "how-to-write-a-jacobian-function-for-optimizeminimize"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
 1. import numpy as np     
 2. from scipy.optimize import _minimize  
 3. from scipy import special
 4. import matplotlib.pyplot as plt

Note the underscore before 'minimize' when importing from scipy.optimize; '_minimize'
Also, i tested the functions from [this link][1] before doing this section, and found I had less trouble/it worked faster, if I imported 'special' separately. 
The Rosenbrock function on the linked page was incorrect - you have to configure the colorbar first; I've posted alternate code but think it could be better.

Further examples to come.

See here for an explanation of [Hessian Matrix][2]


  [1]: http://blog.mmast.net/optimization-scipy
  [2]: http://mathworld.wolfram.com/Hessian.html

## Optimization Example (golden)
The 'Golden' method minimizes a unimodal function by narrowing the range in the extreme values

    import numpy as np
    from scipy.optimize import _minimize
    from scipy import special
    import matplotlib.pyplot as plt
    
    x = np.linspace(0, 10, 500)
    y = special.j0(x)
    optimize.minimize_scalar(special.j0, method='golden')
    plt.plot(x, y)
    plt.show()
    
Resulting image

[![golgen_small][1]][1]
    


  [1]: http://i.stack.imgur.com/nRwaf.jpg

## Optimization Example (Brent)
Brent's method is a more complex algorithm combination of other root-finding algorithms; however, the resulting graph isn't much different from the graph generated from the golden method.

    import numpy as np
    import scipy.optimize as opt
    from scipy import special
    import matplotlib.pyplot as plt
        
    x = np.linspace(0, 10, 500)
    y = special.j0(x)
    # j0 is the Bessel function of 1st kind, 0th order
    minimize_result = opt.minimize_scalar(special.j0, method='brent')
    the_answer = minimize_result['x']
    minimized_value = minimize_result['fun']
    # Note: minimize_result is a dictionary with several fields describing the optimizer,
    # whether it was successful, etc. The value of x that gives us our minimum is accessed
    # with the key 'x'. The value of j0 at that x value is accessed with the key 'fun'.
    plt.plot(x, y)
    plt.axvline(the_answer, linestyle='--', color='k')
    plt.show()
    print("The function's minimum occurs at x = {0} and y = {1}".format(the_answer, minimized_value))


Resulting graph

[![brent_sml][1]][1]

Outputs:

`The function's minimum occurs at x = 3.8317059554863437 and y = -0.4027593957025531`

    


  [1]: http://i.stack.imgur.com/ZZgZa.jpg

## Rosenbrock function
Think this could example could be better but you get the gist

    import numpy as np
    from scipy.optimize import _minimize
    from scipy import special
    import matplotlib.pyplot as plt
    from matplotlib import cm
    from numpy.random import randn

    x, y = np.mgrid[-2:2:100j, -2:2:100j]
    plt.pcolor(x, y, optimize.rosen([x, y]))
    plt.plot(1, 1, 'xw')

    # Make plot with vertical (default) colorbar
    data = np.clip(randn(100, 100), -1, 1)
    cax = plt.imshow(data, cmap=cm.coolwarm)

    # Add colorbar, make sure to specify tick locations to match desired  ticklabels 
    cbar = plt.colorbar(cax, ticks=[-2, 0, 2])  # vertically oriented colorbar
    plt.axis([-2, 2, -2, 2])
    plt.title('Rosenbrock function') #add title if desired
    plt.xlabel('x')
    plt.ylabel('y')

    plt.show()  #generate


[![rosenbrock][1]][1]


  [1]: http://i.stack.imgur.com/MoXbX.png

