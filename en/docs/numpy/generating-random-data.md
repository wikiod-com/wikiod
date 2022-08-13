---
title: "Generating random data"
slug: "generating-random-data"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

The `random` module of NumPy provides convenient methods for generating random data having the desired shape and distribution. 

Here's the [official documentation](https://docs.scipy.org/doc/numpy-1.12.0/reference/routines.random.html).

## Generating random numbers drawn from specific distributions
Draw samples from a normal (gaussian) distribution

    # Generate 5 random numbers from a standard normal distribution
    # (mean = 0, standard deviation = 1)
    np.random.randn(5) 
    # Out: array([-0.84423086,  0.70564081, -0.39878617, -0.82719653, -0.4157447 ])

    # This result can also be achieved with the more general np.random.normal
    np.random.normal(0, 1, 5)
    # Out: array([-0.84423086,  0.70564081, -0.39878617, -0.82719653, -0.4157447 ])
    
    # Specify the distribution's parameters
    # Generate 5 random numbers drawn from a normal distribution with mean=70, std=10
    np.random.normal(70, 10, 5)
    # Out: array([ 72.06498837,  65.43118674,  59.40024236,  76.14957316,  84.29660766])


There are several additional distributions available in `numpy.random`, for example `poisson`, `binomial` and `logistic`

    np.random.poisson(2.5, 5)  # 5 numbers, lambda=5
    # Out: array([0, 2, 4, 3, 5])

    np.random.binomial(4, 0.3, 5)  # 5 numbers, n=4, p=0.3
    # Out: array([1, 0, 2, 1, 0])

    np.random.logistic(2.3, 1.2, 5)  # 5 numbers, location=2.3, scale=1.2
    # Out: array([ 1.23471936,  2.28598718, -0.81045893,  2.2474899 ,  4.15836878])

## Creating random integers
    # Creates a 5x5 random integer array ranging from 10 (inclusive) to 20 (inclusive)
    np.random.randint(10, 20, (5, 5))
    
    ''' 
    Out: array([[12, 14, 17, 16, 18],
                [18, 11, 16, 17, 17],
                [18, 11, 15, 19, 18],
                [19, 14, 13, 10, 13],
                [15, 10, 12, 13, 18]])
    '''

## Selecting a random sample from an array
    letters = list('abcde')

Select three letters randomly (*with replacement* - same item can be chosen multiple times):

    np.random.choice(letters, 3)
    ''' 
    Out: array(['e', 'e', 'd'], 
          dtype='<U1')
    '''

Sampling without replacement:

    np.random.choice(letters, 3, replace=False)
    ''' 
    Out: array(['a', 'c', 'd'], 
          dtype='<U1')
    '''

Assign probability to each letter:

    # Choses 'a' with 40% chance, 'b' with 30% and the remaining ones with 10% each
    np.random.choice(letters, size=10, p=[0.4, 0.3, 0.1, 0.1, 0.1])
    
    '''
    Out: array(['a', 'b', 'e', 'b', 'a', 'b', 'b', 'c', 'a', 'b'],
      dtype='<U1')
    '''




## Setting the seed
Using `random.seed`:
    
    np.random.seed(0)
    np.random.rand(5)
    # Out: array([ 0.5488135 ,  0.71518937,  0.60276338,  0.54488318,  0.4236548 ])

By creating a random number generator object:

    prng = np.random.RandomState(0)
    prng.rand(5)
    # Out: array([ 0.5488135 ,  0.71518937,  0.60276338,  0.54488318,  0.4236548 ])
    


## Creating a simple random array
    # Generates 5 random numbers from a uniform distribution [0, 1)
    np.random.rand(5)
    # Out: array([ 0.4071833 ,  0.069167  ,  0.69742877,  0.45354268,  0.7220556 ])

