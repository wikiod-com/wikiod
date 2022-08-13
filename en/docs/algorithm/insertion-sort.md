---
title: "Insertion Sort"
slug: "insertion-sort"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

When we analyze the performance of the sorting algorithm, we interest primarily on the number of comparison and exchange.

Average Exchange
----

Let E<sub>n</sub> be the total average number of exchange to sort array of N element. E<sub>1</sub> = 0 (we do not need any exchange for array with one element). The average number of exchange to sort  N  element array is the sum of average number of number of exchange to sort  N-1  element array with the average exchange to insert the last element into  N-1  element array.

[![enter image description here][1]][1]

Simplify the summation (arithmetic series)

[![enter image description here][2]][2]

Expands the term

[![enter image description here][3]][3]

Simplify the summation again (arithmetic series)

[![enter image description here][4]][4]

Average Comparison
-----
Let C<sub>n</sub> be the total average number of comparison to sort array of N element. C<sub>1</sub> = 0 (we do not need any comparison on one element array). The average number of comparison to sort N element array is the sum of average number of number of comparison to sort  N-1  element array with the average comparison to insert the last element into  N-1  element array. If last element is largest element, we need only one comparison, if the last element is the second smallest element, we need  N-1  comparison. However, if last element is the smallest element, we do not need  N  comparison. We still only need  N-1  comparison. That is why we remove  1/N  in below equation.

[![enter image description here][5]][5]

Simplify the summation (arithmetic series)

[![enter image description here][6]][6]

Expand the term

[![enter image description here][7]][7]

Simplify the summation again (arithmetic series and harmonic number)

[![enter image description here][8]][8]


  [1]: http://i.stack.imgur.com/0I2Ba.gif
  [2]: http://i.stack.imgur.com/i6vUV.gif
  [3]: http://i.stack.imgur.com/qQGAc.gif
  [4]: http://i.stack.imgur.com/D4Iye.gif
  [5]: http://i.stack.imgur.com/CsWkN.gif
  [6]: http://i.stack.imgur.com/V4kOL.gif
  [7]: http://i.stack.imgur.com/RtUcF.gif
  [8]: http://i.stack.imgur.com/b6ViQ.gif

## Haskell Implementation
    insertSort :: Ord a => [a] -> [a]
    insertSort [] = []
    insertSort (x:xs) = insert x (insertSort xs)

    insert :: Ord a => a-> [a] -> [a]
    insert n [] = [n]
    insert n (x:xs) | n <= x    = (n:x:xs)
                    | otherwise = x:insert n xs

## Algorithm Basics
Insertion sort is a very simple, stable, in-place sorting algorithm. It performs well on small sequences but it is much less efficient on large lists.
At every step, the algorithms considers the i-th element of the given sequence, moving it to the left until it is in the correct position.

**Graphical Illustration**

[![Insertion Sort][1]][1]

**Pseudocode**

    for j = 1 to length(A)
        n = A[j]
        i = j - 1
        while j > 0 and A[i] > n
            A[i + 1] = A[i]
            i = i - 1
        A[i + 1] = n

**Example**

Consider the following list of integers:
    
    [5, 2, 4, 6, 1, 3]

The algorithm will perform the following steps:

1. <code>[5, 2, 4, 6, 1, 3]</code>
2. <code>[2, 5, 4, 6, 1, 3]</code>
3. <code>[2, 4, 5, 6, 1, 3]</code>
4. <code>[2, 4, 5, 6, 1, 3]</code>
5. <code>[1, 2, 4, 5, 6, 3]</code>
6. <code>[1, 2, 3, 4, 5, 6]</code>      

  [1]: http://i.stack.imgur.com/Jn79T.jpg  

## C# Implementation
    public class InsertionSort
    {
        public static void SortInsertion(int[] input, int n)
        {
            for (int i = 0; i < n; i++)
            {
                int x = input[i];
                int j = i - 1;
                while (j >= 0 && input[j] > x)
                {
                    input[j + 1] = input[j];
                    j = j - 1;
                }
                input[j + 1] = x;
            }
        }

        public static int[] Main(int[] input)
        {
            SortInsertion(input, input.Length);
            return input;
        }
    }

**Auxiliary Space:** `O(1)`<br>
**Time Complexity:** `O(n)`


  [1]: https://en.wikipedia.org/wiki/Insertion_sort
  [2]: http://i.stack.imgur.com/Jn79T.jpg

