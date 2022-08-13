---
title: "Figures and Axes Objects"
slug: "figures-and-axes-objects"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Creating an axes
There are two main ways to create an axes in matplotlib: using pyplot, or using the object-oriented API.

Using pyplot:

<!-- language-all: python -->
    import matplotlib.pyplot as plt
    
    ax = plt.subplot(3, 2, 1)  # 3 rows, 2 columns, the first subplot

Using the object-oriented API:

    import matplotlib.pyplot as plt

    fig = plt.figure()
    ax = fig.add_subplot(3, 2, 1)

The convenience function `plt.subplots()` can be used to produce a figure and collection of subplots in one command:

    import matplotlib.pyplot as plt

    fig, (ax1, ax2) = plt.subplots(ncols=2, nrows=1)  # 1 row, 2 columns

## Creating a figure
The figure contains all the plot elements. The main way to create a figure in `matplotlib` is to use `pyplot`.

    import matplotlib.pyplot as plt
    fig = plt.figure()

You can optionally supply a number, which you can use to access a previously-created figure. If a number is not supplied, the last-created figure's ID will be incremented and used instead; figures are indexed starting from 1, not 0.

    import matplotlib.pyplot as plt
    fig = plt.figure()
    fig == plt.figure(1)  # True

Instead of a number, figures can also identified by a string. If using an interactive backend, this will also set the window title.
    
    import matplotlib.pyplot as plt
    fig = plt.figure('image')

To choose figure use

    plt.figure(fig.number) # or
    plt.figure(1)



