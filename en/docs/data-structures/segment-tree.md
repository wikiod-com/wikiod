---
title: "Segment Tree"
slug: "segment-tree"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Introduction To Segment Tree
Suppose we have an array:

    +-------+-----+-----+-----+-----+-----+-----+
    | Index |  0  |  1  |  2  |  3  |  4  |  5  |
    +-------+-----+-----+-----+-----+-----+-----+
    | Value |  -1 |  3  |  4  |  0  |  2  |  1  |
    +-------+-----+-----+-----+-----+-----+-----+
We want to perform some query on this array. For example:

 - What is the minimum from index-**2** to index-**4**?   -> 0
 - What is the maximum from index-**0** to index-**3**?   -> 4
 - What is the summation from index-**1** to index-**5**? -> 10

How do we find it out?

**Brute Force:**<br/>
We could traverse the array from the starting index to the finishing index and answer our query. In this approach, each query takes `O(n)` time where **n** is the difference between the starting index and finishing index. But what if there are millions of numbers and millions of queries? For **m** queries, it would take `O(mn)` time. So for 10⁵ values in our array, if we conduct 10⁵ queries, for worst case, we'll need to traverse 10¹⁰ items.

**Dynamic Programming:**<br/>
We can create a 6X6 matrix to store the values for different ranges. For range minimum query(RMQ), our array would look like:

                  0     1     2     3     4     5
         +-----+-----+-----+-----+-----+-----+-----+
         |     |  -1 |  3  |  4  |  0  |  2  |  1  |
         +-----+-----+-----+-----+-----+-----+-----+
      0  |  -1 |  -1 |  -1 |  -1 |  -1 |  -1 |  -1 |
         +-----+-----+-----+-----+-----+-----+-----+
      1  |  3  |     |  3  |  3  |  0  |  0  |  0  |
         +-----+-----+-----+-----+-----+-----+-----+
      2  |  4  |     |     |  4  |  0  |  0  |  0  |
         +-----+-----+-----+-----+-----+-----+-----+
      3  |  0  |     |     |     |  0  |  0  |  0  |
         +-----+-----+-----+-----+-----+-----+-----+
      4  |  2  |     |     |     |     |  2  |  1  |
         +-----+-----+-----+-----+-----+-----+-----+
      5  |  1  |     |     |     |     |     |  1  |
         +-----+-----+-----+-----+-----+-----+-----+
Once we have this matrix build, it would be sufficient to answer all the RMQs. Here, **Matrix\[i\]\[j\]** would store the minimum value from index-**i** to index-**j**. For example: The minimum from index-**2** to index-**4** is **Matrix\[2\]\[4\]** = **0**. This matrix answers the query in `O(1)` time, but it takes **O(n²)** time to build this matrix and `O(n²)` space to store it. So if **n** is a really big number, this doesn't scale very well.

**Segment Tree:**<br/>
A [segment tree](https://en.wikipedia.org/wiki/Segment_tree) is a tree data structure for storing intervals, or segments. It allows querying which of the stored segments contain a given point. It takes `O(n)` time to build a segment tree, it takes `O(n)` space to maintain it and it answers a query in `O(logn)` time.

Segment tree is a binary tree and the elements of the given array will be the leaves of that tree. We'll create the segment tree by dividing the array in half till we reach a single element. So our division would provide us with:

[![Segmented Array][1]][1]

Now if we replace the non-leaf nodes with the minimum value of their leaves, we get:

[![Minimum values replaced][2]][2]

So this is our segment tree. We can notice that, the root node contains the minimum value of the whole array(range [0,5]), its left child contains the minimum value of our left array(range [0,2]), right child contains the minimum value of our right array(range [3,5]) and so on. The leaf nodes contain minimum value of each individual points. We get:

[![Segment Tree][3]][3]

Now let's do a range query on this tree. To do a range query, we need to check three conditions:

 - Partial Overlap: We check both leaves.
 - Total Overlap: We return the value stored in the node.
 - No Overlap: We return a very large value or infinity.

Let's say, we want to check range **[2,4]**, that means we want to find the minimum from index-**2** to **4**. We'll start from the root and check if the range in our nodes is overlapped by our query range or not. Here,

 - **[2,4]** doesn't completely overlap **[0,5]**. So we'll check both directions.
     - At left subtree, **[2,4]** partially overlaps **[0,2]**. We'll check both directions.
         - At left subtree, **[2,4]** does not overlap **[0,1]**, so this is not going to contribute to our answer. We return **infinity**.
         - At right subtree, **[2,4]** totally overlaps **[2,2]**. We return **4**.<br/>
         The minimum of these two returned values(4, infinity) is **4**. We get **4** from this portion.
    - At right subtree, **[2,4]** partially overlaps. So we'll check both directions.
        - At left subtree, **[2,4]** completely overlaps **[3,4]**. We return **0**.
        - At right subtree, **[2,4]** doesn't overlap **[5,5]**. We return **infinity**.<br/>
The minimum of these two returned values(0, infinity) is **0**. We get **0** from this portion.
 - The minimum of the returned values(4,0) is **0**. This is our desired minimum in range **[2,4]**.

Now we can check that, no matter what our query is, it would take maximum **4** steps to find the desired value from this segment tree.

**Use:**<br/>

 - Range Minimum Query
 - Lowest Common Ancestor
 - Lazy Propagation
 - Dynamic Subtree Sum
 - Neglect & Min
 - Dual Fields
 - Finding k-th Smallest Element
 - Finding Number of Unordered Pairs

  [1]: https://i.stack.imgur.com/wTS4a.png
  [2]: https://i.stack.imgur.com/xtnxJ.png
  [3]: https://i.stack.imgur.com/2vCca.png

## Implementation of Segment Tree Using Array
Let's say, we have an array: `Item = {-1, 0, 3, 6}`. We want to construct **SegmentTree** array to find out the minimum value in a given range. Our segment tree will look like:

[![Segment Tree][1]][1]

The numbers below the nodes show the indices of each values that we'll store in our **SegmentTree** array. We can see that, to store 4 elements, we needed an array of size 7. This value is determined by:

    Procedure DetermineArraySize(Item):
    multiplier := 1
    n := Item.size
    while multiplier < n
        multiplier := multiplier * 2
    end while
    size := (2 * multiplier) - 1
    Return size
So if we had an array of length 5, the size of our **SegmentTree** array would be: (**8** * **2**) - **1** = **15**. Now, to determine the position of left and right child of a node, if a node is in index **i**, then the position of its:
 - Left Child is denoted by: **(2** * **i)** + **1**.
 - Right Child is denoted by: **(2** * **i)** + **2**.

And the index of the **parent** of any **node** in index **i** can be determined by: **(i** - **1)**/**2**.

So the **SegmentTree** array representing our example would look like:

       0     1     2     3     4     5     6
    +-----+-----+-----+-----+-----+-----+-----+
    |  -1 |  -1 |  3  |  -1 |  0  |  3  |  6  |
    +-----+-----+-----+-----+-----+-----+-----+

Let's look at the pseudo-code to construct this array:

    Procedure ConstructTree(Item, SegmentTree, low, high, position):
    if low is equal to high
        SegmentTree[position] := Item[low]
    else
        mid := (low+high)/2
        constructTree(Item, SegmentTree, low, mid, 2*position+1)
        constructTree(Item, SegmentTree, mid+1, high, 2*position+2)
        SegmentTree[position] := min(SegmentTree[2*position+1], SegmentTree[2*position+2])
    end if
At first, we take input of the values and initialize the **SegmentTree** array with `infinity` using the length of the **Item** array as its size. We call the the procedure using:

 - low = Starting index of **Item** array.
 - high = Finishing index of **Item** array.
 - position = 0, indicates the **root** of our Segment Tree.

Now, let's try to understand the procedure using an example:
[![New Example (-1, 2, 4, 0)][2]][2]

The size of our **Item** array is **4**. We create an array of length **(4** * **2)** - **1** = **7** and initialize them with `infinity`. You can use a very large value for it. Our array would look like:

       0     1     2     3     4     5     6
    +-----+-----+-----+-----+-----+-----+-----+
    | inf | inf | inf | inf | inf | inf | inf |
    +-----+-----+-----+-----+-----+-----+-----+

Since this is a recursive procedure, we'll see the operation of the `ConstructTree` using a recursion table that keeps track of `low`, `high`, `position`, `mid` and `calling line` at each call. At first, we call **ConstructTree(Item, SegmentTree, 0, 3, 0)**. Here, `low` is not same as `high`, we'll get a `mid`. The `calling line` indicates which `ConstructTree` is called after this statement. We denote the `ConstructTree` calls inside the procedure as **1** and **2** respectively. Our table will look like:

    +-----+------+----------+-----+--------------+
    | low | high | position | mid | calling line |
    +-----+------+----------+-----+--------------+
    |  0  |   3  |     0    |  1  |       1      |
    +-----+------+----------+-----+--------------+
So when we call `ConstructTree-1`, we pass: `low = 0`, `high = mid = 1`, `position = 2*position+1 = 2*0+1 = 1`. One thing you can notice, that is `2*position+1` is the left child of **root**, which is **1**. Since `low` is not equal to `high`, we get a `mid`. Our table will look like:

    +-----+------+----------+-----+--------------+
    | low | high | position | mid | calling line |
    +-----+------+----------+-----+--------------+
    |  0  |   3  |     0    |  1  |       1      |
    +-----+------+----------+-----+--------------+
    |  0  |   1  |     1    |  0  |       1      |
    +-----+------+----------+-----+--------------+
In the next recursive call, we pass `low = 0`, `high = mid = 0`, `position = 2*position+1 = 2*1+1=3`. Again the left child of index **1** is **3**. Here, `low` is e`high`, so we set `SegmentTree[position] = SegmentTree[3] = Item[low] = Item[0] = -1`. Our **SegmentTree** array will look like:

       0     1     2     3     4     5     6
    +-----+-----+-----+-----+-----+-----+-----+
    | inf | inf | inf |  -1 | inf | inf | inf |
    +-----+-----+-----+-----+-----+-----+-----+
Our recursion table will look like:

    +-----+------+----------+-----+--------------+
    | low | high | position | mid | calling line |
    +-----+------+----------+-----+--------------+
    |  0  |   3  |     0    |  1  |       1      |
    +-----+------+----------+-----+--------------+
    |  0  |   1  |     1    |  0  |       1      |
    +-----+------+----------+-----+--------------+
    |  0  |   0  |     3    |     |              |
    +-----+------+----------+-----+--------------+

So you can see, **-1** has got its correct position. Since this recursive call is complete, we go back to the previous row of our recursion table. The table:

    +-----+------+----------+-----+--------------+
    | low | high | position | mid | calling line |
    +-----+------+----------+-----+--------------+
    |  0  |   3  |     0    |  1  |       1      |
    +-----+------+----------+-----+--------------+
    |  0  |   1  |     1    |  0  |       1      |
    +-----+------+----------+-----+--------------+

 In our procedure, we execute the `ConstructTree-2` call. This time, we pass `low = mid+1 = 1`, `high = 1`, `position = 2*position+2 = 2*1+2 = 4`. Our `calling line` changes to **2**. We get:

    +-----+------+----------+-----+--------------+
    | low | high | position | mid | calling line |
    +-----+------+----------+-----+--------------+
    |  0  |   3  |     0    |  1  |       1      |
    +-----+------+----------+-----+--------------+
    |  0  |   1  |     1    |  0  |       2      |
    +-----+------+----------+-----+--------------+

Since, `low` is equal to `high`, we set: `SegmentTree[position] = SegmentTree[4] = Item[low] = Item[1] = 2`. Our **SegmentTree** array:

       0     1     2     3     4     5     6
    +-----+-----+-----+-----+-----+-----+-----+
    | inf | inf | inf |  -1 |  2  | inf | inf |
    +-----+-----+-----+-----+-----+-----+-----+
Our recursion table:

    +-----+------+----------+-----+--------------+
    | low | high | position | mid | calling line |
    +-----+------+----------+-----+--------------+
    |  0  |   3  |     0    |  1  |       1      |
    +-----+------+----------+-----+--------------+
    |  0  |   1  |     1    |  0  |       2      |
    +-----+------+----------+-----+--------------+
    |  1  |   1  |     4    |     |              |
    +-----+------+----------+-----+--------------+

Again you can see, **2** has got its correct position. After this recursive call, we go back to the previous row of our recursion table. We get:

    +-----+------+----------+-----+--------------+
    | low | high | position | mid | calling line |
    +-----+------+----------+-----+--------------+
    |  0  |   3  |     0    |  1  |       1      |
    +-----+------+----------+-----+--------------+
    |  0  |   1  |     1    |  0  |       2      |
    +-----+------+----------+-----+--------------+
We execute the last line of our procedure, `SegmentTree[position] = SegmentTree[1] = min(SegmentTree[2*position+1], SegmentTree[2*position+2]) = min(SegmentTree[3], SegmentTree[4]) = min(-1,2) = -1`. Our **SegmentTree** array:

       0     1     2     3     4     5     6
    +-----+-----+-----+-----+-----+-----+-----+
    | inf |  -1 | inf |  -1 |  2  | inf | inf |
    +-----+-----+-----+-----+-----+-----+-----+

Since this recursion call is complete, we go back to the previous row of our recursion table and call `ConstructTree-2`:

    +-----+------+----------+-----+--------------+
    | low | high | position | mid | calling line |
    +-----+------+----------+-----+--------------+
    |  0  |   3  |     0    |  1  |       2      |
    +-----+------+----------+-----+--------------+

We can see that the left portion of our segment tree is complete. If we continue in this manner, after completing the whole procedure we'll finally get a completed **SegmentTree** array, that'll look like:

       0     1     2     3     4     5     6
    +-----+-----+-----+-----+-----+-----+-----+
    |  -1 |  -1 |  0  |  -1 |  2  |  4  |  0  |
    +-----+-----+-----+-----+-----+-----+-----+
The time and space complexity of constructing this **SegmentTree** array is: `O(n)`, where **n** denotes the number of elements in **Item** array. Our constructed **SegmentTree** array can be used to perform *Range Minimum Query(RMQ)*. To construct an array to perform *Range Maximum Query*, we need to replace the line:

    SegmentTree[position] := min(SegmentTree[2*position+1], SegmentTree[2*position+2])
with:

    SegmentTree[position] := max(SegmentTree[2*position+1], SegmentTree[2*position+2])

  [1]: https://i.stack.imgur.com/l1t4s.png
  [2]: https://i.stack.imgur.com/DKnDC.png

## Performing a Range Minimum Query
The procedure to perform a RMQ is already shown in introduction. The pseudo-code for checking Range Minimum Query will be:

    Procedure RangeMinimumQuery(SegmentTree, qLow, qHigh, low, high, position):
    if qLow <= low and qHigh >= high        //Total Overlap
        Return SegmentTree[position]
    else if qLow > high || qHigh < low      //No Overlap
        Return infinity
    else                                    //Partial Overlap
        mid := (low+high)/2
        Return min(RangeMinimumQuery(SegmentTree, qLow, qHigh, low, mid, 2*position+1),
                   RangeMinimumQuery(SegmentTree, qLow, qHigh, mid+1, high, 2*position+2))
    end if
Here, `qLow = The lower range of our query`, `qHigh = The upper range of our query`. `low = starting index of Item array`, `high = Finishing index of Item array`, `position = root = 0`. Now let's try to understand the procedure using the example we created before:

[![Example Segment Tree][1]][1]

Our **SegmentTree** array:

       0     1     2     3     4     5     6
    +-----+-----+-----+-----+-----+-----+-----+
    |  -1 |  -1 |  0  |  -1 |  2  |  4  |  0  |
    +-----+-----+-----+-----+-----+-----+-----+
We want to find the minimum in range **[1,3]**.<br/>
Since this is a recursive procedure, we'll see the operation of the `RangeMinimumQuery` using a recursion table that keeps track of `qLow`, `qHigh`, `low`, `high`, `position`, `mid` and `calling line`. At first, we call **RangeMinimumQuery(SegmentTree, 1, 3, 0, 3, 0**. Here, the first two conditions are not met(partial overlap). We'll get a `mid`. The `calling line` indicates which `RangeMinimumQuery` is called after this statement. We denote the `RangeMinimumQuery` calls inside the procedure as **1** and **2** respectively. Our table will look like:

    +------+-------+-----+------+----------+-----+--------------+
    | qLow | qHigh | low | high | position | mid | calling line |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   3  |     0    |  1  |       1      |
    +------+-------+-----+------+----------+-----+--------------+
So when we call `RangeMinimumQuery-1`, we pass: `low = 0`, `high = mid = 1`, `position = 2*position+1 = 1`. One thing you can notice, that is `2*position+1` is the left child of a **node**. That means we're checking the left child of **root**. Since **[1,3]** partially overlaps **[0,1]**, the first two conditions are not met, we get a `mid`. Our table:

    +------+-------+-----+------+----------+-----+--------------+
    | qLow | qHigh | low | high | position | mid | calling line |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   3  |     0    |  1  |       1      |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   1  |     1    |  0  |       1      |
    +------+-------+-----+------+----------+-----+--------------+
In the next recursive call, we pass `low = 0`, `high = mid = 0`, `position = 2*position+1 = 3`. We reach the leftmost leaf of our tree. Since **[1,3]** doesn't overlap with **[0,0]**, we return `infinity` to our calling function. Recursion table:

    +------+-------+-----+------+----------+-----+--------------+
    | qLow | qHigh | low | high | position | mid | calling line |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   3  |     0    |  1  |       1      |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   1  |     1    |  0  |       1      |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   0  |     3    |     |              |
    +------+-------+-----+------+----------+-----+--------------+
Since this recursive call is complete, we go back to the previous row of our recursion table. We get:

    +------+-------+-----+------+----------+-----+--------------+
    | qLow | qHigh | low | high | position | mid | calling line |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   3  |     0    |  1  |       1      |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   1  |     1    |  0  |       1      |
    +------+-------+-----+------+----------+-----+--------------+
In our procedure, we execute `RangeMinimumQuery-2` call. This time, we pass `low = mid+1 = 1`, `high = 1` and `position = 2*position+2 = 4`. Our `calling line changes to **2**`. We get:

    +------+-------+-----+------+----------+-----+--------------+
    | qLow | qHigh | low | high | position | mid | calling line |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   3  |     0    |  1  |       1      |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   1  |     1    |  0  |       2      |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  1  |   1  |     4    |     |              |
    +------+-------+-----+------+----------+-----+--------------+
So we are going to the right child of previous node. This time there is a total overlap. We return the value `SegmentTree[position] = SegmentTree[4] = 2`.

    +------+-------+-----+------+----------+-----+--------------+
    | qLow | qHigh | low | high | position | mid | calling line |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   3  |     0    |  1  |       1      |
    +------+-------+-----+------+----------+-----+--------------+

Back at the calling function, we are checking what is the minimum of the two returned values of two calling functions. Obviously the minimum is **2**, we return **2** to the calling function. Our  recursion table looks like:

    +------+-------+-----+------+----------+-----+--------------+
    | qLow | qHigh | low | high | position | mid | calling line |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   3  |     0    |  1  |       1      |
    +------+-------+-----+------+----------+-----+--------------+
We're done looking at the left portion of our segment tree. Now we'll call `RangeMinimumQuery-2` from here. We'll pass `low = mid+1 = 1+1 =2`, `high = 3` and `position = 2*position+2 = 2`. Our table:

    +------+-------+-----+------+----------+-----+--------------+
    | qLow | qHigh | low | high | position | mid | calling line |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  0  |   3  |     0    |  1  |       1      |
    +------+-------+-----+------+----------+-----+--------------+
    |   1  |   3   |  2  |   3  |     2    |     |              |
    +------+-------+-----+------+----------+-----+--------------+
There is a total overlap. So we return the value: `SegmentTree[position] = SegmentTree[2] = 0`. We come back to the recursion from where these two children were called and get the minimum of **(4,0)**, that is **0** and return the value.

After executing the procedure, we get **0**, which is the minimum from index-**1** to index-**3**.

The runtime complexity for this procedure is `O(logn)` where **n** is the number of elements in the **Items** array. To perform a Range Maximum Query, we need to replace the line:

    Return min(RangeMinimumQuery(SegmentTree, qLow, qHigh, low, mid, 2*position+1),
                   RangeMinimumQuery(SegmentTree, qLow, qHigh, mid+1, high, 2*position+2))
with:

    Return max(RangeMinimumQuery(SegmentTree, qLow, qHigh, low, mid, 2*position+1),
                   RangeMinimumQuery(SegmentTree, qLow, qHigh, mid+1, high, 2*position+2))

  [1]: https://i.stack.imgur.com/rWjIi.png

## Lazy Propagation
Let's say, you have already created a segment tree. You are required to update the values of the array, this will not only change the leaf nodes of your segment tree, but also the minimum/maximum in some nodes. Let's look at this with an example:

[![Example Segment Tree][1]][1]

This is our minimum segment tree of **8** elements. To give you a quick reminder, each nodes represent the minimum value of the range mentioned beside them. Let's say, we want to increment the value of the first item of our array by **3**. How can we do that? We'll follow the way in which we conducted RMQ. The process would look like:

 - At first, we traverse the root. **[0,0]** partially overlaps with **[0,7]**, we go to both directions.
      - At left subtree, **[0,0]** partially overlaps with **[0,3]**, we go to both directions.
          - At left subtree, **[0,0]** partially overlaps with **[0,1]**, we go to both directions.
              - At left subtree, **[0,0]** totally overlaps with **[0,0]**, and since its the leaf node, we update the node by increasing it s value by **3**. And return the value **-1** + **3** = **2**.
               - At right subtree, **[1,1]** doesn't overlap with **[0,0]**, we return the value at the node(**2**).<br/>
                The minimum of these two returned values(**2**, **2**) are **2**, so we update the value of the current node and return it.
           - At right subtree **[2,3]** doesn't overlap with **[0,0]**, we return the value of the node. (**1**).<br/>
            Since the minimum of these two returned values (**2**, **1**) is **1**, we update the value of the current node and return it.
    - At right subtree **[4,7]** doesn't overlap with **[0,0]**, we return the value of the node. (**1**).<br/>
 - Finally the value of the root node is updated since the minimum of the two returned values **(1,1)** is **1**.

We can see that, updating a single node requires `O(logn)` time complexity, where **n** is the number of leaf nodes. So if we are asked to update some nodes from **i** to **j**, we'll require `O(nlogn)` time complexity. This becomes cumbersome for a large value of **n**. Let's be *Lazy* i. e., do work only when needed. How? When we need to update an interval, we will update a node and mark its child that it needs to be updated and update it when needed. For this we need an array **lazy** of the same size as that of a segment tree. Initially all the elements of the **lazy** array will be **0** representing that there is no pending update. If there is non-zero element in **lazy[i]**, then this element needs to update node **i** in the segment tree before making any query operations. How are we going to do that? Let's look at an example:

Let's say, for our example tree, we want to execute some queries. These are:

 - increment **[0,3]** by **3**.
 - increment **[0,3]** by **1**.
 - increment **[0,0]** by **2**.

**increment [0,3] by 3:**
 - We start from the root node. At first, we check if this value is up-to-date. For this we check our **lazy** array which is **0**, that means the value is up-to-date. **[0,3]** partially overlaps **[0,7]**. So we go to both the directions.

     - At the left subtree, there's no pending update. **[0,3]** totally overlaps **[0,3]**. So we update the value of the node by **3**. So the value becomes **-1** + **3** = **2**. This time, we're not going to go all the way. Instead of going down, we update the corresponding child in the lazy tree of our current node and increment them by **3**. We also return the value of the current node.
    - At the right subtree, there's no pending update. **[0,3]** doesn't overlap **[4,7]**. So we return the value of the current node **(1)**.<br/>
    The minimum of two returned values (**2**, **1**) is **1**. We update the root node to **1**.

Our Segment Tree and Lazy Tree would look like:

[![Segment tree and lazy tree][2]][2]
 
The non-zero values in nodes of our Lazy Tree represents, there are updates pending in those nodes and below. We'll update them if required. If we are asked, what is the minimum in range **[0,3]**, we'll come to the left subtree of the root node and since there's no pending updates, we'll return **2**, which is correct. So this process doesn't affect the correctness of our segment tree algorithm.

**increment [0,3] by 1:**

 - We start from the root node. There's no pending update. **[0,3]** partially overlaps **[0,7]**. So we go to both directions.
     - In the left subtree, there's no pending update. **[0,3]** completely overlaps **[0,3]**. We update the current node: **2** + **1** = **3**. Since this is an internal node, we update its children in the Lazy Tree to be incremented by **1**. We'll also return the value of the current node (**3**).
     - In the right subtree, there's no pending update. **[0,3]** doesn't overlap **[4,7]**. We return the value of the current node (**1**). <br/>
 - We update the root node by taking the minimum of two returned values(**3**, **1**).

Our Segment Tree and Lazy Tree will look like:

[![Segment Tree and Lazy Tree updated][3]][3]

 As you can see, we're accumulating the updates at Lazy Tree but not pushing it down. This is what Lazy Propagation means. If we hadn't used it, we had to push the values down to the leaves, which would cost us more unnecessary time complexity.

**increment [0,0] by 2:**

 - We start from the root node. Since root is up-to-date and **[0,0]** partially overlaps **[0,7]**, we go to both directions.
     - At the left subtree, the current node is up-to-date and **[0,0]** partially overlaps **[0,3]**, we go to both directions.
        - At the left subtree, the current node in Lazy Tree has a non-zero value. So there is an update which has not been propagated yet to this node. We're going to first update this node in our Segment Tree. So this becomes **-1** + **4** = **3**. Then we're going to propagate this **4** to its children in the Lazy Tree. As we have already updated the current node, we'll change the value of current node in Lazy Tree to **0**. Then **[0,0]** partially overlaps **[0,1]**, so we go to both directions.
            - At the left node, the value needs to be updated since there is a non-zero value in the current node of Lazy Tree. So we update the value to **-1** + **4** = **3**. Now, since **[0,0]** totally overlaps **[0,0]**, we update the value of the current node to **3** + **2** = **5**. This is a leaf node, so we need not to propagate the value anymore. We update the corresponding node at the Lazy Tree to **0** since all the values have been propagated up till this node. We return the value of the current node(**5**).
            - At the right node, the value needs to be updated. So the value becomes: **4** + **2** = **6**. Since **[0,0]** doesn't overlap **[1,1]**, we return the value of the current node(**6**). We also update the value in Lazy Tree to **0**. No propagation is needed since this is a leaf node.<br/>
            We update the current node using the minimum of two returned values(**5**,**6**). We return the value of the current node(**5**).
        - At the right subtree, there's a pending update. We update the value of the node to **1** + **4** = **5**. Since this is not a leaf node, we propagate the value to its children in our Lazy Tree and update the current node to **0**. Since **[0,0]** doesn't overlap with **[2,3]**, we return the value of our current node(**5**).<br/>
        We update the current node using the minimum of the returned values(**5**, **5**) and return the value(**5**).
    - At the right subtree, there's no pending update and since **[0,0]** doesn't overlap **[4,7]**, we return the value of the current node(**1**).
 - We update the root node using the minimum of the two returned values(**5**,**1**).

Our Segment Tree and Lazy Tree will look like:

[![Segment Tree and Lazy Tree Updated][4]][4]

We can notice that, the value at **[0,0]**, when needed, got all the increment.

Now if you are asked to find the minimum in range **[3,5]**, if you have understood up to this point, you can easily figure out how the query would go and the returned value will be **1**. Our segment Tree and Lazy Tree would look like:

[![Lazy Tree and Segment Tree after query][5]][5]

We have simply followed the same process we followed in finding RMQ with added constraints of checking the Lazy Tree for pending updates.

Another thing we can do is instead of returning values from each node, since we know what will be the child node of our current node, we can simply check them to find the minimum of these two.

The pseudo-code for updating in Lazy Propagation would be:

    Procedure UpdateSegmentTreeLazy(segmentTree, LazyTree, startRange,
                                    endRange, delta, low, high, position):
    if low > high                                                //out of bounds
        Return
    end if
    if LazyTree[position] is not equal to 0                      //update needed
        segmentTree[position] := segmentTree[position] + LazyTree[position]
        if low is not equal to high                              //non-leaf node    
            LazyTree[2 * position + 1] := LazyTree[2 * position + 1] + delta
            LazyTree[2 * position + 2] := LazyTree[2 * position + 2] + delta
        end if
        LazyTree[position] := 0
    end if
    if startRange > low or endRange < high                       //doesn't overlap
        Return
    end if
    if startRange <= low && endRange >= high                     //total overlap
        segmentTree[position] := segmentTree[position] + delta
        if low is not equal to high
            LazyTree[2 * position + 1] := LazyTree[2 * position + 1] + delta
            LazyTree[2 * position + 2] := LazyTree[2 * position + 2] + delta
        end if
        Return
    end if
    //if we reach this portion, this means there's a partial overlap
    mid := (low + high) / 2
    UpdateSegmentTreeLazy(segmentTree, LazyTree, startRange,
                                    endRange, delta, low, mid, 2 * position + 1)
    UpdateSegmentTreeLazy(segmentTree, LazyTree, startRange,
                                    endRange, delta, mid + 1, high, 2 * position + 2)
    segmentTree[position] := min(segmentTree[2 * position + 1], 
                                                        segmentTree[2 * position + 2])
And the pseudo-code for RMQ in Lazy Propagation will be:

    Procedure RangeMinimumQueryLazy(segmentTree, LazyTree, qLow, qHigh, low, high, position):
    if low > high
        Return infinity
    end if
    if LazyTree[position] is not equal to 0                      //update needed
        segmentTree[position] := segmentTree[position] + LazyTree[position]
        if low is not equal to high
            segmentTree[position] := segmentTree[position] + LazyTree[position]
        if low is not equal to high                              //non-leaf node    
            LazyTree[2 * position + 1] := LazyTree[2 * position + 1] + LazyTree[position]
            LazyTree[2 * position + 2] := LazyTree[2 * position + 2] + LazyTree[position]
        end if
        LazyTree[position] := 0
    end if
    if qLow > high and qHigh < low                                //no overlap
        Return infinity
    end if
    if qLow <= low and qHigh >= high                              //total overlap
        Return segmentTree[position]
    end if
    //if we reach this portion, this means there's a partial overlap
    mid := (low + high) / 2
    Return min(RangeMinimumQueryLazy(segmentTree, LazyTree, qLow, qHigh, 
                                            low, mid, 2 * position + 1),
               RangeMinimumQueryLazy(segmentTree, LazyTree, qLow, qHigh,
                                            mid + 1, high, 2 * position + 1)
            

  [1]: https://i.stack.imgur.com/I9eWA.png
  [2]: https://i.stack.imgur.com/nOS52.png
  [3]: https://i.stack.imgur.com/WJpu0.png
  [4]: https://i.stack.imgur.com/18oYZ.png
  [5]: https://i.stack.imgur.com/0TEiq.png

