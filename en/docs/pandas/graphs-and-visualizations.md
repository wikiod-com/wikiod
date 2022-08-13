---
title: "Graphs and Visualizations"
slug: "graphs-and-visualizations"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Basic Data Graphs
Pandas uses provides multiple ways to make graphs of the data inside the data frame. It uses [matplotlib][1] for that purpose.

The basic graphs have their wrappers for both DataFrame and Series objects:

**Line Plot**

    df = pd.DataFrame({'x': [10, 8, 10, 7, 7, 10, 9, 9],
                       'y': [6, 4, 5, 5, 7, 10, 9, 9]})
    df.plot()
[![Plot of all columns][2]][2]

You can call the same method for a Series object to plot a subset of the Data Frame:

    df['x'].plot()

[![Plot of a data frame subset][3]][3]

**Bar Chart**

If you want to explore the distribution of your data, you can use the `hist()` method.


  
    df['x'].hist()

 [![Data Histogram][4]][4]

**General method for plotting [plot][5]()**

All the possible graphs are available through the plot method. The kind of chart is selected by the **kind** argument.

    df['x'].plot(kind='pie')

[![A Pie Chart][6]][6]

**Note** In many environments, the pie chart will come out an oval. To make it a circle, use the following:

    from matplotlib import pyplot

    pyplot.axis('equal')
    df['x'].plot(kind='pie')



  [1]: http://matplotlib.org
  [2]: http://i.stack.imgur.com/Uv0DM.png
  [3]: http://i.stack.imgur.com/ihF5F.png
  [4]: http://i.stack.imgur.com/ZPHMa.png
  [5]: http://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.plot.html
  [6]: http://i.stack.imgur.com/wYn6a.png

## Styling the plot
`plot()` can take arguments that get passed on to matplotlib to style the plot in different ways.

    df.plot(style='o')  # plot as dots, not lines
    df.plot(style='g--')  # plot as green dashed line
    df.plot(style='o', markeredgecolor='white')  # plot as dots with white edge

## Plot on an existing matplotlib axis
By default, `plot()` creates a new figure each time it is called. It is possible to plot on an existing axis by passing the `ax` parameter.

    plt.figure()  # create a new figure
    ax = plt.subplot(121)  # create the left-side subplot
    df1.plot(ax=ax)  # plot df1 on that subplot
    ax = plt.subplot(122)  # create the right-side subplot
    df2.plot(ax=ax)  # and plot df2 there
    plt.show()  # show the plot

