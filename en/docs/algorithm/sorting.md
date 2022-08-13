---
title: "Sorting"
slug: "sorting"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

## Parameters
| Parameter | Description | 
| ----------| ----------- |
| Stability | A sorting algorithm is **stable** if it preserves the relative order of equal elements after sorting. |
| In place | A sorting algorithm is **in-place** if it sorts using only `O(1)` auxiliary memory (not counting the array that needs to be sorted). |
| Best case complexity | A sorting algorithm has a best case time complexity of `O(T(n))` if its running time is **at least** `T(n)` for all possible inputs. |
| Average case complexity | A sorting algorithm has an average case time complexity of `O(T(n))` if its running time, **averaged over all possible inputs**, is `T(n)`. |
| Worst case complexity | A sorting algorithm has a worst case time complexity of `O(T(n))` if its running time is **at most** `T(n)`.

## Stability in Sorting
Stability in sorting means whether a sort algorithm maintains the relative order of the equals keys of the original input in the result output. 

So a sorting algorithm is said to be stable if two objects with equal keys appear in the same order in sorted output as they appear in the input unsorted array.

Consider a list of pairs:

    (1, 2) (9, 7) (3, 4) (8, 6) (9, 3)

Now we will sort the list using the first element of each pair.

A **stable sorting** of this list will output the below list:

    (1, 2) (3, 4) (8, 6) (9, 7) (9, 3)

Because `(9, 3)` appears after `(9, 7)` in the original list as well.

An **unstable sorting** will output the below list:

    (1, 2) (3, 4) (8, 6) (9, 3) (9, 7)

Unstable sort may generate the same output as the stable sort but not always.

Well-known stable sorts: 

 - [Merge sort][1]
 - [Insertion sort][2]
 - [Radix sort][6]
 - Tim sort
 - [Bubble Sort][4]


Well-known unstable sorts:

 - [Heap sort][5]
 - [Quick sort][3]


  [1]: https://www.wikiod.com/algorithm/merge-sort
  [2]: https://www.wikiod.com/algorithm/insertion-sort
  [3]: https://www.wikiod.com/algorithm
  [4]: https://www.wikiod.com/algorithm/bubble-sort
  [5]: https://www.wikiod.com/algorithm/heap-sort
  [6]: https://www.wikiod.com/algorithm/radix-sort

