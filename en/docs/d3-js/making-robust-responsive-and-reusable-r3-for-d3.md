---
title: "Making Robust, Responsive and Reusable (r3) for d3"
slug: "making-robust-responsive-and-reusable-r3-for-d3"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

d3 is a powerful library for creating interactive charts; however, that power stems from users having to work at a lower level than other interactive libraries. Consequently many of the examples for d3 charts are designed to demonstrate how to produce a particular thing - e.g. whiskers for a box and whisker plot - while often hard coding in parameters thereby making the code inflexible. 
The purpose of this documentation is demonstrate how to make more reusable code to save time in the future.

## Scatter Plot
This example contains over 1000 lines of code in total (too much to be embedded here). For that reason all code is accessible on http://blockbuilder.org/SumNeuron/956772481d4e625eec9a59fdb9fbe5b2 (alternatively hosted at https://bl.ocks.org/SumNeuron/956772481d4e625eec9a59fdb9fbe5b2). Note the bl.ocks.org uses iframe so to see the resizing you will need to click the button open (lower right corner of the iframe). Since there is a lot of code, it has been broken up into multiple files and the relevant code segment will be reference both by file name and line number. Please open this example as we go through it. 

--------
# What makes a chart?
There are several core components that go into any complete chart; namely these include:
- title 
- axes
- axes labels
- the data

There are other aspects that might be included depending on the chart - e.g. a chart legend. However, many of these elements can be circumvented with tooltip. For that reason there are interactive chart specific elements - e.g. buttons to switch between data. 

Since our chart's content will be interactive, it would be appropriate for the chart itself to be dynamic - e.g. resize when the window's size changes. SVG is scalable, so you could just allow your chart to be scaled maintaining the current perspective. However, depending on the set perspective, the chart may become too small to be readable even if there is still sufficient space for the chart (e.g. if the width is greater than the height). Therefore it may be preferable to just redraw the chart in the remaining size. 

This example will cover how to dynamically calculate the placement of the buttons, title, axes, axes labels, as well as handle datasets of variant amounts of data

--------
# Set up

## Configuration
Since we are aiming for code reuse, we should make a configuration file to contain global options for aspects of our chart. An example of such a configuration file is `charts_configuration.json`.

If we look at this file we can see I have included several elements which should have already have clear use for when we make our chart:

- files (stores the string for where our chart data is held)
- document_state (which button is currently selected for our chart)
- chart_ids (html ids for the charts we will make)
- svg (options for the svg, e.g. size)
- plot_attributes
  + title (set various font attributes)
  + tooltip (set various tooltip style properties)
  + axes (set various font attributes)
  + buttons (set various font and style attributes)
- plots
  + scatter (set various aspects of our scatter plot, e.g. point radius)
- colors (a specific color palette to use)

## Helper functions
In addition to setting up these global aspects we need to define some helper functions. These can be found under `helpers.js`

- `ajax_json` (load json files either synchronously or asynchronously)
- `keys` (returns keys of the given object - equivalent to d3.keys())
- `parseNumber` (a general number parse in case we do not know what type or number is)
- `typeofNumber` (return the number type)

## index.html
Lastly we should set up our html file. For the purpose of this example we will put our chart in a `section` tag where the `id` matches the id provided in the configuration file (line 37). Since percentages only work if they can be calculated from their parent member, we also include some basic styling (lines 19-35)

------
# Making our scatter plot
Let's open `make_scatter_chart.js`. Now let's pay close attention to line 2, where many of the most important variables are predefined:
- svg - d3 selection of the chart's svg
- chart_group - d3 selection of the group inside the svg in which the data will be placed
- canvas - core aspects of the svg extract for convenience
- margins - the margins we need to take into consideration
- maxi_draw_space the largest x and y values in which we can draw our data
- doc_state - the current state of the document if we are using buttons (in this example we are)

You may have noticed that we did not include the svg in the html. Therefore before we can do anything with our chart, we need to add the svg to`index.html` if it doesn't yet exist. This is achieved in the file `make_svg.js` by the function `make_chart_svg`. Looking at `make_svg.js` we see that we use the helper function `parseNumber` on the chart configuration for the svg width and height. If the number is a float, it makes the svg's width and height proportional its section's width and height. If the number is an integer, it will just set it to those integers.


Lines 6 - 11 tests to see - in effect - if this is the first call or not and sets the `chart_group` (and document state if it is the first call). 

Line 14 - 15 extracts the currently selected data by the clicked button; line 16 sets `data_extent`. While d3 has a function for extracting the data extent, it is *my* preference to store the data extent in this variable. 

Lines 27 - 38 contains the magic which sets up our chart by making the margins, the buttons, the title, and the axes. These are all dynamically determined and might seem a bit complex, so we will look at each in turn.

### make_margins (in make_margins.js)
We can see that the margins object takes into account some space on the left, right, top and bottom of the chart (x.left, x.right, y.top, y.bottom respectively), the title, the buttons, and the axes.

We also see that the axes margins are updated in line 21. 

Why do we do this? Well unless we specify the number of ticks, the tick labels the tick size, and the tick label font size, we could not calculate the size the axes need.  Even then we still would have to guesstimate the space between the tick labels and the ticks. Therefore it is easier to make some dummy axes using our data, see how large the corresponding svg elements are, and then return the size. 

We actually only need the width of the y axis and the height of the x axis, which is what is stored in axes.y and axes.x. 

With our default margins set, we then calculate the `max_drawing_space` (lines 29-34 in make_margins.js)

### make_buttons (in make_buttons.js)

The function makes a group for all of the buttons, and then a group for each button, which in turn stores a circle and text element. Line 37 - 85 calculates the position of the buttons. It does this by seeing if the text right of each button's length is longer than space allowed for us to draw in (line 75). If so, it drops the button down a line and updates the margins.

### make_title (in make_title.js)
`make_title` is similar to make_buttons in that it will automatically break up the title of your chart into multiple lines, and hyphenate if need be. It is a bit hacky since it doesn't have the sophistication of TeX's hyphenation scheme, but it works sufficiently well. If we need more lines than one the margins are updated.

With the buttons, title, and margins set, we can make our axes.

### make_axes (in make_axes.js)
The logic of make_axes mirrors that for calculating the space needed by the dummy axes. Here, however, it adds transitions to change between axes.


## Finally our scatter plot

With all of that set up done, we can finally make our scatter plot. Since our datasets may have a different number of points we need to take this into account and leverage d3's enter and exit events accordingly.  Getting the number of already existing points is done in line 40. The if statement in line 45 - 59 adds more circle elements if we have more data, or transitions the extra elements to a corner and then removes them if there is too many.

Once we know we have the right number of elements, we can transition all of the remaining elements to their correct position (line 64)

Lastly we add tooltip in line 67 and 68. The tooltip function is in `make_tooltip.js`








## Box and Whisker Chart
To show the value of making generalized functions like those in the previous example (make_title, make_axes, make_buttons, etc), consider this box and whisker chart: https://bl.ocks.org/SumNeuron/262e37e2f932cf4b693f241c52a410ff

While the code for making the boxes and whiskers is more intensive than just placing the points, we see that the same functions work perfectly. 


## Bar Chart
https://bl.ocks.org/SumNeuron/7989abb1749fc70b39f7b1e8dd192248

