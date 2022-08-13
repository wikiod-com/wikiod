---
title: "Matrix Chain Multiplication"
slug: "matrix-chain-multiplication"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Recursive Solution
[Matrix chain multiplication](https://en.wikipedia.org/wiki/Matrix_chain_multiplication) is an optimization problem that can be solved using dynamic programming. Given a sequence of matrices, the goal is to find the most efficient way to multiply these matrices. The problem is not actually to perform the multiplications, but merely to decide the sequence of the matrix multiplications involved.

Let's say we have two matrices **A<sub>1</sub>** and **A<sub>2</sub>** of dimension **m * n** and **p * q**. From the rules of matrix multiplication, we know that,
 1. We can multiply **A<sub>1</sub>** and **A<sub>2</sub>** if and only if **n** =  **p**. That means the number of columns of **A<sub>1</sub>** must be equal to the number of rows of **A<sub>2</sub>**.
 2. If the first condition is satisfied and we do multiply **A<sub>1</sub>** and **A<sub>2</sub>**, we'll get a new matrix, let's call it **A<sub>3</sub>** of dimension **m** * **q**.
 3. To multiply **A<sub>1</sub>** and **A<sub>2</sub>**, we need to do some scaler multiplications. The total number of scaler multiplications, we need to perform is (**m** * **n** * **q**) or (**m** * **p** * **q**).
 4. **A<sub>1</sub>** * **A<sub>2</sub>** is not equal to **A<sub>2</sub>** * **A<sub>1</sub>**.

If we have three matrices **A<sub>1</sub>**, **A<sub>2</sub>** and **A<sub>3</sub>** having dimension **m** * **n**, **n** * **p** and **p** * **q** respectively, then **A<sub>4</sub>** = **A<sub>1</sub>** * **A<sub>2</sub>** * **A<sub>3</sub>** will have dimension of **m** * **q**. Now we can perform this matrix multiplication in two ways:
 - First we multiply **A<sub>1</sub>** and **A<sub>2</sub>**, then with the result we multiply **A<sub>3</sub>**. That is: (**A<sub>1</sub>** * **A<sub>2</sub>**) * **A<sub>3</sub>**.
 - First we multiply **A<sub>2</sub>** and **A<sub>3</sub>**, then with the result we multiply **A<sub>1</sub>**. That is: **A<sub>1</sub>** * (**A<sub>2</sub>** * **A<sub>3</sub>**).

You can notice the order of the multiplication remains the same, i.e. we don't multiply (**A<sub>1</sub>** * **A<sub>3</sub>**) * **A<sub>2</sub>** because it might not be valid. We only change the *parenthesis* to multiply a set before multiplying it with the remaining. How we place these parenthesis are important. Why? Let's say, the dimension of 3 matrices **A<sub>1</sub>**, **A<sub>2</sub>** and **A<sub>3</sub>** are **10** * **100**, **100** * **5**, **5** * **50**. Then,
 1. For (**A<sub>1</sub>** * **A<sub>2</sub>**) * **A<sub>3</sub>**, the total number of scaler multiplications are: (**10** * **100** * **5**) + (**10** * **5** * **50**) = **7500** times.
 2. For **A<sub>1</sub>** * (**A<sub>2</sub>** * **A<sub>3</sub>**), the total number of scaler multiplications are: (**100** * **5** * **50**) + (**10** * **100** * **50**) = **75000** times.

For the 2nd type the number of scaler multiplication is **10** times the number of 1st type! So if you can devise a way to find out the correct orientation of parenthesis needed to minimize the total scaler multiplication, it would reduce both time and memory needed for matrix multiplication. This is where matrix chain multiplication comes in handy. Here, we'll not be concerned with the actual multiplication of matrices, we'll only find out the correct parenthesis order so that the number of scaler multiplication is minimized. We'll have matrices **A<sub>1</sub>**, **A<sub>2</sub>**, **A<sub>3</sub>** ........ **A<sub>n</sub>** and we'll find out the the minimum number of scaler multiplications needed to multiply these. We'll assume that the given dimensions are valid, i.e. it satisfies our first requirement for matrix multiplication.

We'll use divide-and-conquer method to solve this problem. Dynamic programming is needed because of common subproblems. For example: for **n** = **5**, we have **5** matrices **A<sub>1</sub>**, **A<sub>2</sub>**, **A<sub>3</sub>**, **A<sub>4</sub>** and **A<sub>5</sub>**. We want to find out the minimum number of multiplication needed to perform this matrix multiplication **A<sub>1</sub>** * **A<sub>2</sub>** * **A<sub>3</sub>** * **A<sub>4</sub>** * **A<sub>5</sub>**. Of the many ways, let's concentrate on one: (**A<sub>1</sub>** * **A<sub>2</sub>**) * (**A<sub>3</sub>** * **A<sub>4</sub>** * **A<sub>5</sub>**).

For this one, we'll find out **A<sub>left</sub>** = **A<sub>1</sub>** * **A<sub>2</sub>**. **A<sub>right</sub>** = **A<sub>3</sub>** * **A<sub>4</sub>** * **A<sub>5</sub>**. Then we'll find out **A<sub>answer</sub>** = **A<sub>left</sub>** * **A<sub>right</sub>**. The total number of scaler multiplications needed to find out **A<sub>answer</sub>**: = The total number of scaler multiplications needed to determine **A<sub>left</sub>** + The total number of scaler multiplications needed to determine **A<sub>right</sub>** + The total number of scaler multiplications needed to determine **A<sub>left</sub>** * **A<sub>right</sub>**.

The last term, The total number of scaler multiplications needed to determine **A<sub>left</sub>** * **A<sub>right</sub>** can be written as: The number of rows in **A<sub>left</sub>** * the number of columns in **A<sub>left</sub>** * the number of columns in **A<sub>right</sub>**. (According to the 2nd rule of matrix multiplication)

But we could set the parenthesis in other ways too. For example:
 - **A<sub>1</sub>** * (**A<sub>2</sub>** * **A<sub>3</sub>** * **A<sub>4</sub>** * **A<sub>5</sub>**)
 - (**A<sub>1</sub>** * **A<sub>2</sub>** * **A<sub>3</sub>**) * (**A<sub>4</sub>** * **A<sub>5</sub>**)
 - (**A<sub>1</sub>** * **A<sub>2</sub>** * **A<sub>3</sub>** * **A<sub>4</sub>**) * **A<sub>5</sub>** etc.

For each and every possible cases, we'll determine the number of scaler multiplication needed to find out **A<sub>left</sub>** and **A<sub>right</sub>**, then for **A<sub>left</sub>** * **A<sub>right</sub>**. If you have a general idea about recursion, you've already understood how to perform this task. Our algorithm will be:

    - Set parenthesis in all possible ways.
    - Recursively solve for the smaller parts.
    - Find out the total number of scaler multiplication by merging left and right.
    - Of all possible ways, choose the best one.
Why this is dynamic programming problem? To determine (**A<sub>1</sub>** * **A<sub>2</sub>** * **A<sub>3</sub>**), if you've already calculated (**A<sub>1</sub>** * **A<sub>2</sub>**), it'll be helpful in this case.

To determine the state of this recursion, we can see that to solve for each case, we'll need to know the range of matrices we're working with. So we'll need a **begin** and **end**. As we're using divide and conquer, our base case will be having less than **2** matrices (**begin** >= **end**), where we don't need to multiply at all. We'll have **2** arrays: **row** and **column**. **row\[i]** and **column\[i]** will store the number of rows and columns for matrix **A<sub>i</sub>**. We'll have a **dp\[n]\[n]** array to store the already calculated values and initialize it with **-1**, where **-1** represents the value has not been calculated yet. **dp\[i]\[j]** represents the number of scaler multiplications needed to multiply **A<sub>i</sub>**, **A<sub>i+1</sub>**, .....,**A<sub>j</sub>** inclusive. The pseudo-code will look like:

    Procedure matrixChain(begin, end):
    if begin >= end
        Return 0
    else if dp[begin][end] is not equal to -1
        Return dp[begin][end]
    end if
    answer := infinity
    for mid from begin to end
        operation_for_left := matrixChain(begin, mid)
        operation_for_right := matrixChain(mid+1, right)
        operation_for_left_and_right := row[begin] * column[mid] * column[end]
        total := operation_for_left + operation_for_right + operation_for_left_and_right
        answer := min(total, answer)
    end for
    dp[begin][end] := answer
    Return dp[begin][end]
**Complexity:**

The value of **begin** and **end** can range from **1** to **n**. There are **n<sup>2</sup>** different states. For each states, the loop inside will run **n** times. Total time complexity: `O(n³)` and memory complexity: `O(n²)`.

