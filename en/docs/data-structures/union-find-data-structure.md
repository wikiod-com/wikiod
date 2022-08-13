---
title: "Union-find data structure"
slug: "union-find-data-structure"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

A union-find (or disjoint-set) data structure is a simple data structure a partition of a number of elements into disjoint sets. Every set has a representative which can be used to distinguish it from the other sets.

It is used in many algorithms, e.g. to compute minimum spanning trees via Kruskal's algorithm, to compute connected components in undirected graphs and many more.

## Theory
Union find data structures provide the following operations:

* `make_sets(n)` initializes a union-find data structure with `n` singletons
* `find(i)` returns a representative for the set of element `i`
* `union(i,j)` merges the sets containing `i` and `j`

We represent our partition of the elements `0` to `n - 1` by storing a *parent* element `parent[i]` for every element `i` which eventually leads to a *representative* of the set containing `i`.  
If an element itself is a representative, it is its own parent, i.e. `parent[i] == i`.

Thus, if we start with singleton sets, every element is its own representative:

[![Singleton sets: Every element is its own parent.][1]][1]

We can find the representative for a given set by simply following these parent pointers.

Let us now see how we can merge sets:  
If we want to merge the elements 0 and 1 and the elements 2 and 3, we can do this by setting the parent of 0 to 1 and setting the parent of 2 to 3:

[![after merge(0,1), merge(2,3)][2]][2]

In this simple case, only the elements parent pointer itself must be changed.  
If we however want to merge larger sets, we must always change the parent pointer of the *representative* of the set that is to be merged into another set: After **merge(0,3)**, we have set the parent of the representative of the set containing 0 to the representative of the set containing 3

[![after merge(0,3)][3]][3]

To make the example a bit more interesting, let's now also **merge (4,5), (5,6) and (3,4)**:

[![after merge(4,5), merge(5,6), merge(3,4)][4]][4]

The last notion I want to introduce is **path compression**:  
If we want to find the representative of a set, we might have to follow several *parent* pointers before reaching the representative. We might make this easier by storing the representative for each set directly in their parent node. We lose the order in which we merged the elements, but can potentially have a large runtime gain. In our case, the only paths that aren't compressed are the paths from 0, 4 and 5 to 3:[![enter image description here][5]][5]


  [1]: https://i.stack.imgur.com/jLcl9.png
  [2]: https://i.stack.imgur.com/TtVPO.png
  [3]: https://i.stack.imgur.com/BgQJ4.png
  [4]: https://i.stack.imgur.com/YdHQ6.png
  [5]: https://i.stack.imgur.com/GLe3E.png

## Basic implementation
The most basic implementation of a union-find data structure consists of an array `parent` storing the a parent element for every element of the structure. Following these parent 'pointers' for an element `i` leads us to the representative  `j = find(i)` of the set containing `i`, where `parent[j] = j` holds.

<!-- language: c++ -->


    using std::size_t;

    class union_find {
    private:
        std::vector<size_t> parent;  // Parent for every node

    public:
        union_find(size_t n) : parent(n) {
            for (size_t i = 0; i < n; ++i)
                parent[i] = i;      // Every element is its own representative
        }

        size_t find(size_t i) const {
            if (parent[i] == i)     // If we already have a representative
                return i;           // return it
            return find(parent[i]); // otherwise return the parent's representative
        }

        void merge(size_t i, size_t j) {
            size_t pi = find(i);
            size_t pj = find(j);
            if (pi != pj) {        // If the elements are not in the same set: 
                parent[pi] = pj;   // Join the sets by marking pj as pi's parent
            }
        }
    };

## Improvements: Path compression
If we do many `merge` operations on a union-find data structure, the paths represented by the `parent` pointers might be quite long. *Path compression*, as already described in the theory part, is a simple way of mitigating this issue.

We might try to do path compression on the whole data structure after every *k*-th merge operation or something similar, but such an operation could have a quite large runtime.

Thus, path compression is mostly only used on a small part of the structure, especially the path we walk along to find the representative of a set. This can be done by storing the result of the `find` operation after every recursive subcall:

<!-- language: c++ -->

    size_t find(size_t i) const {
        if (parent[i] == i)          // If we already have a representative
            return i;                // return it
        parent[i] = find(parent[i]); // path-compress on the way to the representative
        return parent[i];            // and return it
    }

## Improvements: Union by size
In our current implementation of `merge`, we always choose the left set to be the child of the right set, without taking the size of the sets into consideration. Without this restriction, the paths (without *path compression*) from an element to its representative might be quite long, thus leading to large runtimes on `find` calls.

Another common improvement thus is the *union by size* heuristic, which does exactly what it says: When merging two sets, we always set the larger set to be the parent of the smaller set, thus leading to a path length of at most [![log n][1]][1] steps:

We store an additional member `std::vector<size_t> size` in our class which gets initialized to 1 for every element. When merging two sets, the larger set becomes the parent of the smaller set and we sum up the two sizes:

<!-- language: c++ -->

    private:
        ...
        std::vector<size_t> size;

    public:
        union_find(size_t n) : parent(n), size(n, 1) { ... }

        ...

        void merge(size_t i, size_t j) {
            size_t pi = find(i);
            size_t pj = find(j);
            if (pi == pj) {            // If the elements are in the same set: 
                return;                // do nothing
            }
            if (size[pi] > size[pj]) { // Swap representatives such that pj
                std::swap(pi, pj);     // represents the larger set
            }
            parent[pi] = pj;           // attach the smaller set to the larger one
            size[pj] += size[pi];      // update the size of the larger set
        }

  [1]: https://i.stack.imgur.com/At4lc.png

## Improvements: Union by rank
Another heuristic commonly used instead of union by size is the *union by rank* heuristic

Its basic idea is that we don't actually need to store the exact size of the sets, an approximation of the size (in this case: roughly the logarithm of the set's size) suffices to achieve the same speed as union by size.

For this, we introduce the notion of the *rank* of a set, which is given as follows:
 * Singletons have rank 0
 * If two sets with unequal rank are merged, the set with larger rank becomes the parent while the rank is left unchanged.
 * If two sets of equal rank are merged, one of them becomes the parent of the other (the choice can be arbitrary), its rank is incremented.

One advantage of *union by rank* is the space usage: As the maximum rank increases roughly like [![log n][1]][1], for all realistic input sizes, the rank can be stored in a single byte (since `n < 2^255`).

A simple implementation of union by rank might look like this:

<!-- language: c++ -->

    private:
        ...
        std::vector<unsigned char> rank;

    public:
        union_find(size_t n) : parent(n), rank(n, 0) { ... }

        ...

        void merge(size_t i, size_t j) {
            size_t pi = find(i);
            size_t pj = find(j);
            if (pi == pj) {
                return;
            }
            if (rank[pi] < rank[pj]) {
                // link the smaller group to the larger one
                parent[pi] = pj;
            } else if (rank[pi] > rank[pj]) {
                // link the smaller group to the larger one
                parent[pj] = pi;
            } else {
                // equal rank: link arbitrarily and increase rank
                parent[pj] = pi;
                ++rank[pi];
            }
        }


  [1]: https://i.stack.imgur.com/At4lc.png

## Final Improvement: Union by rank with out-of-bounds storage
While in combination with path compression, union by rank nearly achieves constant time operations on union-find data structures, there is a final trick that allows us to get rid of the `rank` storage altogether by storing the rank in out-of-bounds entries of the `parent`array. It is based on the following observations:

* We actually only need to store the rank for *representatives*, not for other elements. For these representatives, we don't need to store a `parent`.
* So far, `parent[i]` is at most `size - 1`, i.e. larger values are unused.
* All ranks are at most [![log n][1]][1].

This brings us to the following approach:

 * Instead of the condition `parent[i] == i`, we now identify representatives by  
 `parent[i] >= size`
 * We use these out-of-bounds values to store the ranks of the set, i.e. the set with representative `i`has rank `parent[i] - size`
 * Thus we initialize the parent array with `parent[i] = size` instead of `parent[i] = i`, i.e. each set is its own representative with rank 0.

Since we only offset the rank values by `size`, we can simply replace the `rank` vector by the `parent` vector in the implementation of `merge` and only need to exchange the condition identifying representatives in `find`:

Finished implementation using union by rank and path compression:

<!-- language: c++ -->


    using std::size_t;

    class union_find {
    private:
        std::vector<size_t> parent;

    public:
        union_find(size_t n) : parent(n, n) {} // initialize with parent[i] = n

        size_t find(size_t i) const {
            if (parent[i] >= parent.size()) // If we already have a representative
                return i;                   // return it
            return find(parent[i]);         // otherwise return the parent's repr.
        }

        void merge(size_t i, size_t j) {
            size_t pi = find(i);
            size_t pj = find(j);
            if (pi == pj) {
                return;
            }
            if (parent[pi] < parent[pj]) {
                // link the smaller group to the larger one
                parent[pi] = pj;
            } else if (parent[pi] > parent[pj]) {
                // link the smaller group to the larger one
                parent[pj] = pi;
            } else {
                // equal rank: link arbitrarily and increase rank
                parent[pj] = pi;
                ++parent[pi];
            }
        }
    };

  [1]: https://i.stack.imgur.com/At4lc.png

