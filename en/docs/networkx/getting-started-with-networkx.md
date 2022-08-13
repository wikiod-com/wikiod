---
title: "Getting started with networkx"
slug: "getting-started-with-networkx"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Welcome to the world of Graphs.  If you have connected data then you might need one of the types of graphs to model those patterns.  There are several things that can be done with Graphs like mapping traffic patterns, managing water distribution networks, social media analysis, etc... At it's heart we need to be able to create Nodes and Edges with data associated about each.

NetworkX is a library dealing with Graph Database creation/import/export/manipulation/algorithms/plotting.  You can start using several types of network graphs.  For example: Facebook might put their users in a plain Graph()

    import networkx as nx
    facebook = nx.Graph()  
    facebook.add_node('you')
    facebook.add_node('me')
    if both_agree_to_be_friends('you','me'):
        facebook.add_edge('you','me') #order isn't important here.

Facebook would use a regular Graph() because there isn't anything special about the edge between nodes.  This Facebook example can only have one edge (friendship) between nodes.

Another type of Graph would be a Directed Graph.  Twitter would use a Directed Graph because the nodes have a direction.  In Twitter I can follow you but you don't have to follow me.  So we could represent that with this code:

    import networkx as nx
    twitter = nx.DiGraph()
    twitter.add_node('you')
    twitter.add_node('me')
    twitter.add_edge('me','you') #order is important here.

This is how Twitter might set up 'me' to follow 'you', but not the other way around.

There are MultiGraphs() and MultiDiGraphs() as well just in case you want more than one edge between two nodes.  These four types cover a wide variety of problems that can be represented with Graphs.  If you want you can add a dictionary of data to both nodes and edges.  The [documentation on this module][1] is incredible.  Every algorithm is well researched and well implemented.  I hope you enjoy working with it.


**Installation instructions:**
The detailed instructions on installing NetworkX is available [here][2].

As with any other python package, NetworkX can be installed using pip, Miniconda/Anaconda and from source code. 

**Installing with pip**

    pip install networkx
An attempt will be made to find and install an appropriate version of NetworkX that matches your operating system and Python version.

> To use pip, you need to have setuptools installed.

If you want to install the development version from GitHub, use the command 

    pip install git://github.com/networkx/networkx.git#egg=networkx

**Miniconda and Anaconda use `conda` for software installation/updates.**

NetworkX is [currently installed][2] with [Anaconda][3].  [Miniconda][3] doesn't come with NetworkX by default.

You can update/install NetworkX to the latest version with:

    conda install networkx

or if you want to update NetworkX installation then

    conda update networkx

**Installing from source**

*Source file archive*

 1. Download the source from
    https://pypi.python.org/pypi/networkx/ or get the [latest version][3]. 
2. Unpack and change
    directory to the source directory (it should have the files
    README.txt and setup.py). 
3. Run `python setup.py install` to build and
    install 
4. (Optional) Run nosetests to execute the tests if you have
    nose installed.

*Installing from GitHub*

1. Clone the NetworkX repository (see https://github.com/networkx/networkx/ for options)

       git clone https://github.com/networkx/networkx.git

2. Change directory to NetworkX

3. Run `python setup.py install` to build and install

4. (Optional) Run nosetests to execute the tests if you have nose installed.

If you don’t have permission to install software on your system, you can install into another directory using the `--user`, `--prefix`, or `--home` flags to setup.py.

**Requirements**
To use NetworkX you need Python 2.7, 3.3 or later

**Optional Packages**

1. NumPy: Provides matrix representation of graphs and is used in some graph algorithms for high-performance matrix computations. (http://scipy.org/Download)
2. SciPy: Provides sparse matrix representation of graphs and many numerical scientific tools. (http://scipy.org/Download)
3. Matplotlib: Provides flexible drawing of graphs. (http://matplotlib.sourceforge.net/)
4. GraphViz in conjunction with either PyGraphviz (http://pygraphviz.github.io) or pydotplus (https://github.com/carlos-jenkins/pydotplus): provides graph drawing and graph layout algorithms. (http://graphviz.org)
5. PyYAML: Required for YAML format reading and writing. (http://pyyaml.org)


  [3]: http://conda.pydata.org/miniconda.html  [2]: https://networkx.github.io/documentation/development/install.html


  [1]: https://networkx.readthedocs.io/en/stable/
  [2]: https://docs.continuum.io/anaconda/pkg-docs
  [3]: https://github.com/networkx/networkx/

## Basic program for displaying nodes in matplotlib using networkx
    import networkx as nx  # importing networkx package
    import matplotlib.pyplot as plt # importing matplotlib package and pyplot is for displaying the graph on canvas 
    b=nx.Graph()
    b.add_node('helloworld')
    b.add_node(1)
    b.add_node(2)
    '''Node can be called by any python-hashable obj like string,number etc'''
    nx.draw(b)  #draws the networkx graph containing nodes which are declared till before
    plt.show()  # displays the networkx graph on matplotlib canvas
**Additional clarification:**
  
    nx.draw(b,nodelist=[1,'helloworld']) #displays the particular nodes which are given by nodelist only 
    nx.draw_networkx(b,nodelist=[1,'helloworld']) #displays the node along with its name given by us i.e 1, hello  respectively


