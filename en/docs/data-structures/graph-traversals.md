---
title: "Graph traversals"
slug: "graph-traversals"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

All the algorithms related to Graph traversals. Their complexities, both runtime and space

## Depth First Search
Depth First Traversal (or Search) for a graph is similar to Depth First Traversal of a tree. The only catch here is, unlike trees, graphs may contain cycles, so we may come to the same node again. To avoid processing a node more than once, we use a boolean visited array.

Below algorithm presents the steps for graph traversal using DFS:

**Algorithm DFS(v);**

**Input**: A vertex v in a graph

**Output**: A labeling of the edges as “discovery” edges
and “backedges”

    for each edge e incident on v do

        if edge e is unexplored then

        let w be the other endpoint of e
        if vertex w is unexplored then
            label e as a discovery edge
            recursively call DFS(w)
        else
        label e as a backedge

[![DFS Illustration][1]][1]

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/Nxxfb.jpg
  [2]: https://i.stack.imgur.com/TtAu7.jpg

## Breadth First Search
**Algorithm BFS(G)**

**Input** graph **G**

**Output** labeling of the edges
        and partition of the
        vertices of **G**

        for all u ∈ G.vertices()
            setLabel(u, UNEXPLORED)
        for all e ∈ G.edges()
        setLabel
        (e, UNEXPLORED)
        for all v ∈ G.vertices()
            if getLabel(v) = UNEXPLORED
                BFS(G, v)

[![Demonstration][1]][1]

[![Demo continued][2]][2]

[![Demo continued][3]][3]




  [1]: https://i.stack.imgur.com/enrF5.jpg
  [2]: https://i.stack.imgur.com/er6az.jpg
  [3]: https://i.stack.imgur.com/VBk5F.jpg

