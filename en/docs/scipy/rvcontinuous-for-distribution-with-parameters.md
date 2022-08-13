---
title: "rv_continuous for Distribution with Parameters"
slug: "rv_continuous-for-distribution-with-parameters"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Negative binomial on positive reals
    from scipy.stats import rv_continuous
    import numpy
    
    class Neg_exp(rv_continuous): 
        def _cdf(self, x, lamda):
            return 1-numpy.exp(-lamda*x)
       
    neg_exp = Neg_exp(name="Negative exponential", a=0)
    
    print (neg_exp.pdf(0,.5))
    print (neg_exp.pdf(5,.5))
    print (neg_exp.cdf(5,.5))
    print (neg_exp.stats(0.5))
    print (neg_exp.rvs(0.5))

It's essential to define either _pdf or _cdf because scipy infers the parameters of the other function (that you do not define), and the order of these parameters in any functions calls that you make, from the your definition. In this case there is only one distribution parameter, lambda. The variable representing the random variable value appears first in the definition of _pdf or _cdf. 

When you define just one of these functions scipy will calculate the other numerically. For possible greater efficiency, define both. Similarly, define _stats in terms of known parameters for best efficiency; otherwise scipy uses numerical methods.

Notice that the distribution's support is defined when the class is instantiated (variable **a** is set to zero and **b** is set to infinity by default), rather then when it is subclassed. Notice too that the distribution's parameters are set only when one of the class instances are called, as in the final five lines of code.

