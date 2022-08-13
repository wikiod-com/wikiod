---
title: "graph-tool"
slug: "graph-tool"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

The python tools can be used to generate graph

## PyDotPlus
PyDotPlus is an improved version of the old pydot project that provides a Python Interface to Graphviz’s Dot language.

### Installation

For the latest stable version:

    pip install pydotplus

For the development version:

    pip install https://github.com/carlos-jenkins/pydotplus/archive/master.zip

#### Load graph as defined by a DOT file 

- The file is assumed to be in DOT format. It will be loaded, parsed and a Dot class will be returned, representing the graph. For example,a simple demo.dot:

   

>  digraph demo1{
>         a -> b -> c;
>         c ->a;
>         }

    import pydotplus
    graph_a = pydotplus.graph_from_dot_file('demo.dot')
    graph_a.write_svg('test.svg') # generate graph in svg.

You will get a svg(Scalable Vector Graphics) like this:

[![enter image description here][1]][1]

  [1]: https://i.stack.imgur.com/Wz3LU.png

## PyGraphviz
Get PyGraphviz from the Python Package Index at http://pypi.python.org/pypi/pygraphviz

or install it with:

`pip install pygraphviz`

and an attempt will be made to find and install an appropriate version that matches your operating system and Python version.

You can install the development version (at github.com) with:

`pip install git://github.com/pygraphviz/pygraphviz.git#egg=pygraphviz`

Get PyGraphviz from the Python Package Index at http://pypi.python.org/pypi/pygraphviz

or install it with:

`easy_install pygraphviz`

and an attempt will be made to find and install an appropriate version that matches your operating system and Python version.

#### Load graph as defined by a DOT file 

- The file is assumed to be in DOT format. It will be loaded, parsed and a Dot class will be returned, representing the graph. For example,a simple demo.dot:

   

>  digraph demo1{
>         a -> b -> c;
>         c ->a;
>         }

- Load it and draw it.
```
import pygraphviz as pgv
G = pgv.AGraph("demo.dot")
G.draw('test', format='svg', prog='dot')
```

You will get a svg(Scalable Vector Graphics) like this:

[![enter image description here][1]][1]

  [1]: https://i.stack.imgur.com/Wz3LU.png

