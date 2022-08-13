---
title: "Quick Sort"
slug: "quick-sort"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Python
The below image shows the working of a quick sort. 

[![enter image description here][1]][1]

Below example shows the working program for quick sort in python:


    def quickSort(alist):
       quickSortHelper(alist,0,len(alist)-1)
    
    def quickSortHelper(alist,first,last):
       if first<last:
    
           splitpoint = partition(alist,first,last)
    
           quickSortHelper(alist,first,splitpoint-1)
           quickSortHelper(alist,splitpoint+1,last)
    
    
    def partition(alist,first,last):
       pivotvalue = alist[first]
    
       leftmark = first+1
       rightmark = last
    
       done = False
       while not done:
    
           while leftmark <= rightmark and alist[leftmark] <= pivotvalue:
               leftmark = leftmark + 1
    
           while alist[rightmark] >= pivotvalue and rightmark >= leftmark:
               rightmark = rightmark -1
    
           if rightmark < leftmark:
               done = True
           else:
               temp = alist[leftmark]
               alist[leftmark] = alist[rightmark]
               alist[rightmark] = temp
    
       temp = alist[first]
       alist[first] = alist[rightmark]
       alist[rightmark] = temp
    
    
       return rightmark
    
    alist = [54,26,93,17,77,31,44,55,20]
    print("Input:")
    print(alist)
    quickSort(alist)
    print("Output:")
    print(alist)

Below is the output of the code:

[![enter image description here][2]][2]

Complexity of the above logic is : **O(n log n)**

  [1]: https://i.stack.imgur.com/SVplm.gif
  [2]: https://i.stack.imgur.com/2TiLz.png

