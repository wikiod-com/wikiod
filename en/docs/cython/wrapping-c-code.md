---
title: "Wrapping C Code"
slug: "wrapping-c-code"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Using functions from a custom C library
We have a C library named `my_random` that produces random numbers from a custom distribution. It provides two functions that we want to use: `set_seed(long seed)` and `rand()` (and many more we do not need). In order to use them in Cython we need to

1. define an interface in the .pxd file and
2. call the function in the .pyx file.

# Code #

## test_extern.pxd ##

    # extern blocks define interfaces for Cython to C code
    cdef extern from "my_random.h":
        double rand()
        void c_set_seed "set_seed" (long seed) # rename C version of set_seed to c_set_seed to avoid naming conflict

## test_extern.pyx ##

    def set_seed (long seed):
        """Pass the seed on to the c version of set_seed in my_random."""
        c_set_seed(seed)

    cpdef get_successes (int x, double threshold):
        """Create a list with x results of rand <= threshold
        
        Use the custom rand function from my_random.
        """
        cdef:
            list successes = []
            int i
        for i in range(x):
            if rand() <= threshold:
                successes.append(True)
            else:
                successes.append(False)
        return successes

