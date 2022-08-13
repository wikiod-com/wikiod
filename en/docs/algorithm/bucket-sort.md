---
title: "Bucket Sort"
slug: "bucket-sort"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## C# Implementation
    public class BucketSort
    {
        public static void SortBucket(ref int[] input)
        {
            int minValue = input[0];
            int maxValue = input[0];
            int k = 0;

            for (int i = input.Length - 1; i >= 1; i--)
            {
                if (input[i] > maxValue) maxValue = input[i];
                if (input[i] < minValue) minValue = input[i];
            }

            List<int>[] bucket = new List<int>[maxValue - minValue + 1];

            for (int i = bucket.Length - 1; i >= 0; i--)
            {
                bucket[i] = new List<int>();
            }

            foreach (int i in input)
            {
                bucket[i - minValue].Add(i);
            }

            foreach (List<int> b in bucket)
            {
                if (b.Count > 0)
                {
                    foreach (int t in b)
                    {
                        input[k] = t;
                        k++;
                    }
                }
            }
        }

        public static int[] Main(int[] input)
        {
            SortBucket(ref input);
            return input;
        }
    }

## Bucket Sort Basic Information
**Bucket Sort** is a sorting algorithm in which elements of input array are distributed in buckets. After distributing all the elements, buckets are sorted individually by another sorting algorithm. Sometimes it is also sorted by recursive method.

**Pseudo code for Bucket Sort**

 1. Let n be the length of the input list L;
 2. For each element i from L
 3. If B[i] is not empty
 4. Put A[i] into B[i];
 5. Else B[i] := A[i]
 6. return Concat B[i .. n] into one sorted list;

**Example of bucket sort:**
[![Bucket Sort Example][1]][1]


  [1]: http://i.stack.imgur.com/3I54p.png

Mostly people uses insertion paradigm for little bit of optimization.<br>
**Auxiliary Space:** `O{n}`

