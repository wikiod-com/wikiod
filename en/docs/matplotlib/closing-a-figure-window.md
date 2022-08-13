---
title: "Closing a figure window"
slug: "closing-a-figure-window"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
 - plt.close()  # closes the current active figure
 - plt.close(fig) # closes the figure with handle 'fig'
 - plt.close(num) # closes the figure number 'num'
 - plt.close(name) # closes the figure with the label 'name'
 - plt.close('all') # closes all figures

## Closing the current active figure using pyplot
The pyplot interface to `matplotlib` might be the simplest way to close a figure.

    import matplotlib.pyplot as plt
    plt.plot([0, 1], [0, 1])
    plt.close()

## Closing a specific figure using plt.close()
A specific figure can be closed by keeping its handle

    import matplotlib.pyplot as plt

    fig1 = plt.figure() # create first figure
    plt.plot([0, 1], [0, 1])

    fig2 = plt.figure() # create second figure
    plt.plot([0, 1], [0, 1])

    plt.close(fig1) # close first figure although second one is active

