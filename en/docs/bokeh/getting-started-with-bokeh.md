---
title: "Getting started with bokeh"
slug: "getting-started-with-bokeh"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing Bokeh
# [Bokeh's Docs on Installation](http://bokeh.pydata.org/en/latest/docs/installation.html)

Bokeh runs on Python it has the following dependencies;   

`NumPy, Jinja2, Six, Requests, Tornado >= 4.0, PyYaml, DateUtil`    

If you plan on installing with Python 2.7 you will also need `future`.


All of those come with the [Anaconda Python Distribution](https://www.continuum.io/downloads). Which you can download and install for free.

Once you have anaconda installed onto your machine then you can simply run the following in cmd.exe on Windows or terminal on Mac:


```
conda install bokeh
```

If you already have a version of Python then you can run the following in cmd.exe on Windows or terminal on Mac:

```
pip install bokeh
```

Be sure to check out the Bokeh [quick start guide](http://bokeh.pydata.org/en/latest/docs/user_guide/quickstart.html#jupyter-notebooks) for several examples.

## Using Bokeh in Jupyter Notebook
Here is a simple example of how to use Bokeh in Jupyter Notebook:
<!-- language: lang-python -->
    import numpy as np
    from bokeh.plotting import figure
    # Make Bokeh Push push output to Jupyter Notebook.
    from bokeh.io import push_notebook, show, output_notebook
    from bokeh.resources import INLINE
    output_notebook(resources=INLINE)
    
    # Create some data.
    x = np.linspace(0,2*np.pi,20)
    y = np.sin(x)
    
    # Create a new plot with a title and axis labels
    p = figure(title="Simple Line Plot in Bokeh", x_axis_label='x', y_axis_label='y')
    
    # Add a line renderer with legend and line thickness
    p.line(x, y, legend="Value", line_width=3)
    
    # Show the results
    show(p)

## Hello World
To use bokeh you need to launch a bokeh server and connect to it using a browser. We will use this example script (`hello_world.py`):

    from bokeh.models import ColumnDataSource
    from bokeh.plotting import figure
    from bokeh.io import curdoc
    
    def modify_doc(doc):
        """Add a plotted function to the document.
    
        Arguments:
            doc: A bokeh document to which elements can be added.
        """
        x_values = range(10)
        y_values = [x ** 2 for x in x_values]
        data_source = ColumnDataSource(data=dict(x=x_values, y=y_values))
        plot = figure(title="f(x) = x^2",
                      tools="crosshair,pan,reset,save,wheel_zoom",)
        plot.line('x', 'y', source=data_source, line_width=3, line_alpha=0.6)
        doc.add_root(plot)
        doc.title = "Hello World"
    
    def main():
        modify_doc(curdoc())
        
    main()


To launch it you need to execute bokeh on the command line and use the `serve` command to launch the server:

    $ bokeh serve --show hello_world.py

The `--show` parameter tells bokeh to open a browser window and show document defined in `hello_world.py`.

