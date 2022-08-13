---
title: "Catalan Number Algorithm"
slug: "catalan-number-algorithm"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Catalan Number Algorithm Basic Information
Catalan numbers algorithm is Dynamic Programming algorithm.

In combinatorial mathematics, the [Catalan numbers][1] form a sequence of natural numbers that occur in various counting problems, often involving recursively-defined objects. The Catalan numbers on nonnegative integers n are a set of numbers that arise in tree enumeration problems of the type, 'In how many ways can a regular n-gon be divided into n-2 triangles if different orientations are counted separately?'

**Application of Catalan Number Algorithm:**

 1. The number of ways to stack coins on a bottom row that consists of n consecutive coins in a plane, such that no coins are allowed to be put on the two sides of the bottom coins and every additional coin must be above two other coins, is the nth Catalan number.
 2. The number of ways to group a string of n pairs of parentheses, such that each open parenthesis has a matching closed parenthesis, is the nth Catalan number.
 3. The number of ways to cut an n+2-sided convex polygon in a plane into triangles by connecting vertices with straight, non-intersecting lines is the nth Catalan number. This is the application in which Euler was interested.

Using zero-based numbering, the *n*th Catalan number is given directly in terms of binomial coefficients by the following equation.

[![Catalan Number Equation][2]][2]

**Example of Catalan Number:**

Here value of n = 4.(Best Example - From Wikipedia)

[![Catalan Number Example][3]][3]

**Auxiliary Space:** `O(n)`<br>
**Time Complexity:** `O(n^2)`

  [1]: https://en.wikipedia.org/wiki/Catalan_number
  [2]: https://i.stack.imgur.com/UP8N4.png
  [3]: https://i.stack.imgur.com/VBGLB.png

## C# Implementation
    public class CatalanNumber
    {
        public static int Main(int number)
        {
            int result = 0;
            if (number <= 1) return 1;
            for (int i = 0; i < number; i++)
            {
                result += Main(i)*Main(number - i - 1);
            }
            return result;
        }
    }

