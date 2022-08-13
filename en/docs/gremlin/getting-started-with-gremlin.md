---
title: "Getting started with gremlin"
slug: "getting-started-with-gremlin"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation of Gremlin Console
The easiest way to get started with Gremlin is to install the [Gremlin Console][1]. The Gremlin Console is a [REPL][2] that allows immediate feedback on the results of Gremlin traversals.

As a prerequisite, [Java 8][3] is required for the Gremlin Console to run. Ensure that it is installed prior to moving forward with the following steps.

[Download][4] the console, unpackage it and start it:

    $ unzip apache-gremlin-console-x.y.z-bin.zip
    $ cd apache-gremlin-console-x.y.z
    $ bin/gremlin.sh
    
             \,,,/
             (o o)
    -----oOOo-(3)-oOOo-----
    plugin activated: tinkerpop.server
    plugin activated: tinkerpop.utilities
    plugin activated: tinkerpop.tinkergraph
    gremlin>

If on Windows, there is an included `gremlin.bat` file that can be used to start the console.

To learn more about Gremlin Console, please read TinkerPop's [tutorial][5] which discusses it's usage in greater detail.


  [1]: http://tinkerpop.apache.org/docs/current/reference/#gremlin-console
  [2]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
  [3]: http://www.oracle.com/technetwork/java/index.html
  [4]: https://www.apache.org/dyn/closer.lua/tinkerpop/3.2.0/apache-gremlin-console-3.2.0-bin.zip
  [5]: http://tinkerpop.apache.org/docs/current/tutorials/the-gremlin-console/

## Using the Toy Graphs
The TinkerPop "toy graphs" make it possible to quickly try out some basic features of Gremlin. These graphs are pre-built and packaged with the Gremlin Console. The most commonly used "toy graphs" are "Modern" and "The Crew". When asking questions on StackOverflow or the [Gremlin Users][1] mailing list it is often useful to frame questions in the context of these graphs as they can help questions get answered quickly and easily by the community.

Both the Modern and Crew graph can be constructed with the `TinekrFactory`, which will construct an in-memory TinkerGraph with the data pre-loaded:

    gremlin> graph = TinkerFactory.createModern()
    ==>tinkergraph[vertices:6 edges:6]
    gremlin> g = graph.traversal()
    ==>graphtraversalsource[tinkergraph[vertices:6 edges:6], standard]
    gremlin> g.V()
    ==>v[1]
    ==>v[2]
    ==>v[3]
    ==>v[4]
    ==>v[5]
    ==>v[6]

Note the conventions in the above code. The `Graph` instance is typically called "graph" and then to execute traversals (i.e. queries) one creates a `TraversalSource` from that `Graph` called "g". Then, the query `g.V()` executes a traversal that gets a list of all the vertices in "g".

To create "The Crew" graph, which features meta/multi-properties, use `TinkerFactory.createTheCrew()`.

More information about using the toy graphs can be found in TinkerPop's tutorial called [The Gremlin Console][2].


  [1]: https://groups.google.com/forum/#!forum/gremlin-users
  [2]: http://tinkerpop.apache.org/docs/current/tutorials/the-gremlin-console/#toy-graphs

