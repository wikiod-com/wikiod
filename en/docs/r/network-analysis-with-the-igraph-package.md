---
title: "Network analysis with the igraph package"
slug: "network-analysis-with-the-igraph-package"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Simple Directed and Non-directed Network Graphing
The igraph package for R is a wonderful tool that can be used to model networks, both real and virtual, with simplicity. This example is meant to demonstrate how to create two simple network graphs using the igraph package within R v.3.2.3. 

**Non-Directed Network**

The network is created with this piece of code:

    g<-graph.formula(Node1-Node2, Node1-Node3, Node4-Node1)
    plot(g)

![Valid XHTML](http://i.imgur.com/wCJh3xI.png)

**Directed Network**

    dg<-graph.formula(Tom-+Mary, Tom-+Bill, Tom-+Sam, Sue+-Mary, Bill-+Sue)
    plot(dg)



This code will then generate a network with arrows: 

![Valid XHTML](http://i.imgur.com/TdI8Slh.png)

Code example of how to make a double sided arrow:

    dg<-graph.formula(Tom-+Mary, Tom-+Bill, Tom-+Sam, Sue+-Mary, Bill++Sue)
    plot(dg)



![Valid XHTML](http://i.imgur.com/PEJTMV6.png)




