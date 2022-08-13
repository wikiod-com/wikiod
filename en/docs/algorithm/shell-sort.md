---
title: "Shell Sort"
slug: "shell-sort"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Shell Sort Basic Information
[Shell sort][1], also known as the diminishing increment sort, is one of the oldest sorting algorithms, named after its inventor [Donald. L. Shell][2] (1959). It is fast, easy to understand and easy to implement. However, its complexity analysis is a little more sophisticated.

The idea of Shell sort is the following:

 1. Arrange the data sequence in a two-dimensional array
 2. Sort the columns of the array

Shell sort improves insertion sort. It starts by comparing elements far apart, then elements less far apart, and finally comparing adjacent elements (effectively an insertion sort).

The effect is that the data sequence is partially sorted.
The process above is repeated, but each time with a narrower array, i.e. with a smaller number of columns. In the last step, the array consists of only one column.

**Example of Shell sort:**

[![Shell sort Example][3]][3]

**Pseudo code for Shell Sort:**

    input
    foreach element in input
    {
        for(i = gap; i < n; i++)
        {
            temp = a[i]
            for (j = i; j >= gap and a[j - gap] > temp; j -= gap)
            {
                a[j] = a[j - gap]
            }
            a[j] = temp
        }
    }

**Auxiliary Space:** `O(n) total, O(1) auxiliary`<br>
**Time Complexity:** `O(nlogn)`
    
  [1]: https://en.wikipedia.org/wiki/Shellsort
  [2]: https://en.wikipedia.org/wiki/Donald_Shell
  [3]: https://i.stack.imgur.com/WTd9p.jpg

## C# Implementation
    public class ShellSort
    {
        static void SortShell(int[] input, int n)
        {
            var inc = 3;
            while (inc > 0)
            {
                int i;
                for (i = 0; i < n; i++)
                {
                    var j = i;
                    var temp = input[i];
                    while ((j >= inc) && (input[j - inc] > temp))
                    {
                        input[j] = input[j - inc];
                        j = j - inc;
                    }
                    input[j] = temp;
                }
                if (inc / 2 != 0)
                    inc = inc / 2;
                else if (inc == 1)
                    inc = 0;
                else
                    inc = 1;
            }
        }

        public static int[] Main(int[] input)
        {
            SortShell(input, input.Length);
            return input;
        }
    }

