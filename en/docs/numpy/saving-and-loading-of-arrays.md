---
title: "Saving and loading of Arrays"
slug: "saving-and-loading-of-arrays"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Numpy arrays can be saved and loaded in various ways. 

## Using numpy.save and numpy.load
[np.save](https://docs.scipy.org/doc/numpy/reference/generated/numpy.save.html) and [np.load](https://docs.scipy.org/doc/numpy/reference/generated/numpy.load.html) provide a easy to use framework for saving and loading of arbitrary sized numpy arrays:


    import numpy as np

    a = np.random.randint(10,size=(3,3))
    np.save('arr', a)

    a2 = np.load('arr.npy')
    print a2

