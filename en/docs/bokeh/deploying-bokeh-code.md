---
title: "Deploying Bokeh Code"
slug: "deploying-bokeh-code"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

This topic should cover all ways to make a bokeh application available to users.

## Local bokeh server with console entry point
To allow a bokeh application to be executed like a *normal* .py file, you need to handle the tornado IOloop in your application, as described [here](http://bokeh.pydata.org/en/latest/docs/user_guide/server.html#embedding-bokeh-server-as-a-library). A standalone bokeh application like this can be used to implement a console script entry point in `setup.py`.
However, this requires bokeh version **>= 0.12.4**.

# The bokeh application #
Consider the file `local_server.py`:


    from tornado.ioloop import IOLoop
    
    from bokeh.application.handlers import FunctionHandler
    from bokeh.application import Application
    from bokeh.models import ColumnDataSource
    from bokeh.plotting import figure
    from bokeh.server.server import Server
    
    
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
        doc.title = "Test Plot"
    
    
    def main():
        """Launch the server and connect to it.
        """
        print("Preparing a bokeh application.")
        io_loop = IOLoop.current()
        bokeh_app = Application(FunctionHandler(modify_doc))
    
        server = Server({"/": bokeh_app}, io_loop=io_loop)
        server.start()
        print("Opening Bokeh application on http://localhost:5006/")
    
        io_loop.add_callback(server.show, "/")
        io_loop.start()
    
    
    main()

This file can be executed 

    $ python local_server.py

which run the server and automatically launch a browser to show the document.


# Entry points and the setup.py #

In order to provide a script that can be easily installed and called using the setup.py. Consider the following folder structure:

    project
    ├── setup.py
    └── my_package
        ├── __init__.py
        └── local_server.py

Content of `setup.py`:

    from setuptools import setup

    setup(
        name = "my_package",
        entry_points={
            "console_scripts": ["my_script = my_package.local_server:main"],
        },
    )

When installing the package using 

    $ python setup.py install

you can then use the call

    $ my_script

to launch the bokeh application and automatically start a browser displaying the document.

