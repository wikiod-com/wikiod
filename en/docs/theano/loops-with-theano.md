---
title: "Loops with theano"
slug: "loops-with-theano"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Basic scan usage
`scan` is used for calling function multiple times over a list of values, the function may contain state.

`scan` syntax (as of theano 0.9):
    
    scan(
        fn,
        sequences=None,
        outputs_info=None,
        non_sequences=None,
        n_steps=None,
        truncate_gradient=-1,
        go_backwards=False,
        mode=None,
        name=None,
        profile=False,
        allow_gc=None,
        strict=False)

This can be very confusing at a first glance. We will explain several basic but important `scan` usage in multiple code examples.

The following code examples assume you have executed imports:

    import numpy as np
    import theano
    import theano.tensor as T

<b>`sequences` - Map a function over a list</b>
 
  In the simplest case, scan just maps a pure function (a function without state) to a list. The lists is specified in the `sequences` argument

      s_x = T.ivector()
      s_y, _ = theano.scan(
          fn = lambda x:x*x,
          sequences = [s_x])
      fn = theano.function([s_x], s_y)
      fn([1,2,3,4,5]) #[1,4,9,16,25]

Note `scan` have two return values, the former is the resulting list, and the latter is the updates to state value, which will be explained later.

<b>`sequences` - Zip a function over a list</b>

   Almost same as above, just give `sequences` argument a list of two elements. The order of the two elements should match to the order of arguments in `fn`

      s_x1 = T.ivector()
      s_x2 = T.ivector()
      s_y, _ = theano.scan(
          fn = lambda x1,x2:x1**x2,
          sequences = [s_x1, s_x2])
      fn = theano.function([s_x], s_y)
      fn([1,2,3,4,5],[0,1,2,3,4]) #[1,2,9,64,625]

<b>`outputs_info` - Accumulate a list</b>

Accumulation involves a state variable. State variables need initial values, which shall be specified in the `outputs_info` parameter.

      s_x = T.ivector()
      v_sum = th.shared(np.int32(0))
      s_y, update_sum = theano.scan(
          lambda x,y:x+y,
          sequences = [s_x],
          outputs_info = [s_sum])
      fn = theano.function([s_x], s_y, updates=update_sum)
      
      v_sum.get_value() # 0
      fn([1,2,3,4,5]) # [1,3,6,10,15]
      v_sum.get_value() # 15
      fn([-1,-2,-3,-4,-5]) # [14,12,9,5,0]
      v_sum.get_value() # 0

We put a shared variable into `outputs_info`, this will cause `scan` return updates to our shared variable, which can then be put into `theano.function`.

<b>`non_sequences` and `n_steps` - Orbit of logistic map `x -> lambda*x*(1-x)`</b>

You can give inputs that does not change during `scan` in `non_sequences` argument. In this case `s_lambda` is a non-changing variable (but NOT a constant since it must be supplied during runtime).
      
      s_x = T.fscalar()
      s_lambda = T.fscalar()
      s_t = T.iscalar()
      s_y, _ = theano.scan(
          fn = lambda x,l: l*x*(1-x),
          outputs_info = [s_x],
          non_sequences = [s_lambda],
          n_steps = s_t
      )
      fn = theano.function([s_x, s_lambda, s_t], s_y)

      fn(.75, 4., 10) #a stable orbit

      #[ 0.75,  0.75,  0.75,  0.75,  0.75,  0.75,  0.75,  0.75,  0.75,  0.75]

      fn(.65, 4., 10) #a chaotic orbit

      #[ 0.91000003,  0.32759991,  0.88111287,  0.41901192,  0.97376364,
      # 0.10219204,  0.3669953 ,  0.92923898,  0.2630156 ,  0.77535355]

<b>Taps - Fibonacci</b>

states/inputs may come in multiple timesteps. This is done by:
* putting `dict(input=<init_value>, taps=<list of int>)` inside `sequences` argument.

* putting `dict(initial=<init_value>, taps=<list of int>)` inside `outputs_info` argument.

In this example, we use two taps in `outputs_info` to compute recurrence relation `x_n = x_{n-1} + x_{n-2}`.

    s_x0 = T.iscalar()
    s_x1 = T.iscalar()
    s_n = T.iscalar()
    s_y, _ = theano.scan(
        fn = lambda x1,x2: x1+x2,
        outputs_info = [dict(initial=T.join(0,[s_x0, s_x1]), taps=[-2,-1])],
        n_steps = s_n
    )
    fn_fib = theano.function([s_x0, s_x1, s_n], s_y)
    fn_fib(1,1,10)
    # [2, 3, 5, 8, 13, 21, 34, 55, 89, 144]

## theano map and reduce
`theano.map` and `theano.scan_module.reduce` are wrappers of `theano_scan`. They can be seen as handicapped version of `scan`. You can view <b>Basic scan usage</b> section for reference.

    import theano
    import theano.tensor as T
    s_x = T.ivector()
    s_sqr, _ = theano.map(
        fn = lambda x:x*x,
        sequences = [s_x])
    s_sum, _ = theano.reduce(
        fn = lambda: x,y:x+y,
        sequences = [s_x],
        outputs_info = [0])
    fn = theano.function([s_x], [s_sqr, s_sum])
    fn([1,2,3,4,5]) #[1,4,9,16,25], 15

## making while loop
As of theano 0.9, while loops can be done via `theano.scan_module.scan_utils.until`.
To use, you should return `until` object in `fn` of `scan`.

In the following example, we build a function that checks whether a complex number is inside Mandelbrot set. A complex number `z_0` is inside mandelbrot set if series `z_{n+1} = z_{n}^2 + z_0` does not converge.

    MAX_ITER = 256
    BAILOUT = 2.
    s_z0 = th.cscalar()
    def iterate(s_i_, s_z_, s_z0_):
        return [s_z_*s_z_+s_z0_,s_i_+1], {}, until(T.abs_(s_z_)>BAILOUT)
    (_1, s_niter), _2 = theano.scan(
        fn = iterate,
        outputs_info = [0, s_z0],
        non_sequences = [s_z0],
        n_steps = MAX_ITER
    )
    fn_mandelbrot_iters = theano.function([s_z0], s_niter)
    def is_in_mandelbrot(z_):
        return fn_mandelbrot_iters(z_)>=MAX_ITER

    is_in_mandelbrot(0.24+0.j) # True
    is_in_mandelbrot(1.j) # True
    is_in_mandelbrot(0.26+0.j) # False

