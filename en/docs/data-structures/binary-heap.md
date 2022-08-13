---
title: "Binary Heap"
slug: "binary-heap"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

A binary heap is a complete binary tree which satisfies the heap ordering property. The ordering can be one of two types: the min-heap property: the value of each node is greater than or equal to the value of its parent, with the minimum-value element at the root.

## Example
**Min-Heap**

   

            1
          /   \
         2     3
        / \   / \
       4   5 6   7

The above tree is a Min-Heap since the root is the minimum among all the nodes present in the tree.The same property is followed by all the nodes in the tree.

**Max-Heap**

            7
          /   \
         6     5
        / \   / \
       4   3 2   1

The above tree is a Max-Heap since the root is the maximum among all the nodes present in the tree.The same property is followed by all the nodes in the tree.

**Operations Supported by Min-Heap**
1) **getMin()** - It return the root element.Since it is the first element in an array we can retrieve the minimum element in `O(1)`

2) **extractMin()** - It removes the minimum element from the heap.Since after removing the element the tree must satisfy the Min-Heap property so a operation (**heapifying**) is performed to maintain the tree property.This takes `O(logn)`

3) **dereaseKey()** - It decreases the value of the key.Time complexity for this operation is `O(logn)`

4) **insert()** - The key is always inserted at the end of the tree.If the added key doesn't follow the heap property than we need to percolate up so that the tree satisfies the heap property.This step takes takes `O(logn)` time.

5) **delete()** - This steps takes `O(logn)` time.For deleting a key we first need to decrease the key value to a minimum value and then extract this minimum value.

**Application of Heap**

1) Heap Sort
2) Priority Queue

Heap is used many of the graph algorithms like `Dijkstra’s Shortest Path` and `Prim’s Minimum Spanning Tree`.

**Implementation in Java**


    public class MinHeap {
        
        int hArr[];
        int capacity;
        int heapSize;
        
        public MinHeap(int capacity){
            this.heapSize = 0;
            this.capacity = capacity;
            hArr = new int[capacity];
        }
        
        public int getparent(int i){
            return (i-1)/2;
        }
        
        public int getLeftChild(int i){
            return 2*i+1;
        }
        
        public int getRightChild(int i){
            return 2*i+2;
        }
        
        public void insertKey(int k){
            if(heapSize==capacity)
                return;
            
            heapSize++;
            int i = heapSize-1;
            hArr[i] = k;
            
            while(i!=0 && hArr[getparent(i)]>hArr[i]){
                swap(hArr[i],hArr[getparent(i)]);
                i = getparent(i);
            }
        }
        
        public int extractMin(){
            if(heapSize==0)
                return Integer.MAX_VALUE;
            
            if(heapSize==1){
                heapSize--;
                return hArr[0];
            }
            
            int root = hArr[0];
            hArr[0] = hArr[heapSize-1];
            heapSize--;
            MinHeapify(0);
            
            return root;
        }
        
        public void decreaseKey(int i , int newVal){
            hArr[i] = newVal;
            while(i!=0 && hArr[getparent(i)]>hArr[i]){
                swap(hArr[i],hArr[getparent(i)]);
                i = getparent(i);
            }
        }
        
        public void deleteKey(int i){
            decreaseKey(i, Integer.MIN_VALUE);
            extractMin();
        }
        
        public void MinHeapify(int i){
            int l = getLeftChild(i);
            int r = getRightChild(i);
            int smallest = i;
            if(l<heapSize && hArr[l] < hArr[i])
                smallest = l;
            if(l<heapSize && hArr[r] < hArr[smallest])
                smallest = r;
            
            if(smallest!=i){
                swap(hArr[i], hArr[smallest]);
                MinHeapify(smallest);
            }
        }
        
        public void swap(int x, int y){
            int temp = hArr[x];
            hArr[x] = hArr[y];
            hArr[y] = temp;
        }
    }



