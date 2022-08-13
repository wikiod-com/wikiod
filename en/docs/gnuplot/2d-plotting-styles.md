---
title: "2D Plotting Styles"
slug: "2d-plotting-styles"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Selecting a plotting style
Explicite selection
-------------------

A plotting style is usually selected using the `with` keyword, like

    plot x with points

This allows to use different plotting styles for every `plot`:

    plot x with points, 2*x with lines

Typing `help with` in the gnuplot command window gives a list of all available plotting styles.


Global plotting style selection
-------------------------------

Plotting styles can also be set globally for all plot commands. Here, gnuplot distinguishes between function and data plots, for which different default styles can be set.

For functions use `set style function`:

    set style function linespoints
    plot x, 2*x

For data files use `set style data`:

    set style data lines
    plot 'file.dat', 'other-file.dat'

Note, that for functions the default style is `lines`, and for data files it is `points`. With `show style data` and `show style function` you can inspect the currently selected plotting styles.


