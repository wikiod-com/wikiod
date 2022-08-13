---
title: "Solving Graph Problems Using Dynamic Programming"
slug: "solving-graph-problems-using-dynamic-programming"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Floyd-Warshall Algorithm


## Minimum Vertex Cover
[Minimum Vertex Cover](https://en.wikipedia.org/wiki/Vertex_cover) is a classic graph problem. Let's say, in a city we have a few roads connecting a few points. Let's represent the roads using edges and the points using nodes. Let's take two example graphs:

[![Example Graph, A-B, B-C, A-C, A-E, A-D, A-F, G-I, G-H, H-I, I-J, J-L, J-K, K-H][1]][1]

We want to set watchmen on some points. A watchman can guard all the roads connected to the point. The problem is, what is the minimum number of watchmen needed to cover all the roads? If we set watchmen at node **A**, **B**, **H**, **I** and **J**, we can cover all the roads.

[![Graph with watchmen][2]][2]

This is our optimal solution. We need at least **5** watchmen to guard the whole city. How to determine this?

At first, we need to understand this is an *NP-hard* problem, i.e. the problem has no polynomial time solution. But if the graph was a **Tree**, that means if it had **(n-1)** nodes where **n** is the number of edges and there are no cycle in the graph, we can solve it using dynamic programming.

[![Minimum Vertex Cover of a Tree, A-B, A-C, B-D, B-E, C-F][3]][3]

To construct a DP solution, we need to follow two strategies:

 1. If there is no watchman in a node, all the nodes connected to it must have a watchman, or all the roads won't be covered. If **u** and **v** are connected and **u** doesn't have any watchman, then **v** must have a watchman.
 2. If there is a watchman in a node, a different node connected to it may or may not have a watchman. That means it is not necessary to have a watchman, but it can be beneficial. If **u** and **v** are connected and **u** has a watchman, we'll check and find which on is beneficial for us by:
    - Setting watchman in **v**.
    - Not setting watchman in **v**.

Let's define a recursive function with state being the current node we're in and whether it has a watchman or not. Here:

    F(u,1) = Currently we're in 'u' node and there is a watchman in this node.
    F(u,0) = Currently we're in 'u' node and there is no watchman in this node.
The function will return the number of watchman in remaining nodes.

Let's take an example tree:

[![Example Tree A-B, A-C, A-D][4]][4]

We can easily say that if we don't put watchman on node-**A**, we'll have to put watchmen on node-**B**, **C** and **D**. We can deduce:

    F(A,0) = F(B,1) + F(C,1) + F(D,1) + 0
It returns us the number of watchmen needed if we don't put watchman in node-**A**. We've added **0** at the end because we didn't set any watchman in our current node.

Now `F(A,1)` means, we set watchman in node-**A**. For that, we can either set watchmen in all the connected nodes or we don't. We'll take the one that provides us with minimum number of watchmen.

    F(A,1) = min(F(B,0), F(B,1) + min(F(C,0), F(C,1)) + min(F(D,0), F(D,1)) + 1
We check by setting and not setting watchman on each node and taking the optimal value.

One thing we must be careful that is, once we go to the child node, we'll never look back to the parent node. From the example above, we went to **B** from **A**, so **parent[B]** = **A**. So we'll not go back to **A** from **B**.

To determine base case, we can notice that, if from a node, we can't go to any other new node, we'll return **1** if there is a watchman in our current node, **0** if there is no watchman in our current node.

It is better to have a adjacency list for our tree. Let the list be denoted by **edge**. We'll have an array **dp[n]\[2]**, where **n** denotes the number of nodes to store the calculated values and initialize it with **-1**. We'll also have a **parent[n]** array to denote the parent and child relation between nodes. Our pseudo-code will look like:

    Procedure f(u, isGuarded):
    if edge[u].size is equal to 0                    //node doesn't have any new edge
        Return isGuarded
    else if dp[u][isGuarded] is not equal to -1      //already calculated
        Return dp[u][isGuarded]
    end if
    sum := 0
    for i from i to edge[u].size
        v := edge[u][i]
        if v is not equal to parent[u]               //not a parent
            parent[v] := u
            if isGuarded equals to 0                 //not guarded, must set a watchman
                sum := sum + f(v,1)
            else                                     //guarded, check both
                sum := sum + min(f(v,1), f(v,0)
            end if
        end if
    end for
    dp[u][isGuarded] := sum + isGuarded
    Return dp[u][isGuarded]

If we denote node-**A** as root, we'll call the function by: `min(f(A,1), f(A,0))`. That means we'll also check if it is optimal to set watchman in the root node or not. This is our DP solution. This problem can also be solved using maximum matching algorithm or max-flow.


  [1]: https://i.stack.imgur.com/7n7bv.jpg
  [2]: https://i.stack.imgur.com/ADM8O.jpg
  [3]: https://i.stack.imgur.com/YvGHM.jpg
  [4]: https://i.stack.imgur.com/rka2p.jpg

