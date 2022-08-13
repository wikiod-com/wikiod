---
title: "Getting started with dynamic-programming"
slug: "getting-started-with-dynamic-programming"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction To Dynamic Programming
[Dynamic programming](https://en.wikipedia.org/wiki/Dynamic_programming) solves problems by combining the solutions to subproblems. It can be analogous to divide-and-conquer method, where problem is partitioned into disjoint subproblems, subproblems are recursively solved and then combined to find the solution of the original problem. In contrast, dynamic programming applies when the subproblems overlap - that is, when subproblems share subsubproblems. In this context, a divide-and-conquer algorithm does more work than necessary, repeatedly solving the common subsubproblems. A dynamic-programming algorithm solves each subsubproblem just once and then saves its answer in a table, thereby avoiding the work of recomputing the answer every time it solves each subsubproblems.

Let's look at an example. Italian Mathematician [Leonardo Pisano Bigollo](https://en.wikipedia.org/wiki/Fibonacci), whom we commonly know as Fibonacci, discovered a number series by considering the [idealized growth of rabbit population](https://en.wikipedia.org/wiki/Fibonacci_number#Origins). The series is:

    1, 1, 2, 3, 5, 8, 13, 21, ......

We can notice that every number after the first two is the sum of the two preceding numbers. Now, let's formulate a function **F(n)** that will return us the nth Fibonacci number, that means,

    F(n) = nth Fibonacci Number
So far, we've known that,

    F(1) = 1
    F(2) = 1
    F(3) = F(2) + F(1) = 2
    F(4) = F(3) + F(2) = 3
We can generalize it by:

    F(1) = 1
    F(2) = 1
    F(n) = F(n-1) + F(n-2)
Now if we want to write it as a recursive function, we have `F(1)` and `F(2)` as our base case. So our Fibonacci Function would be:

    Procedure F(n):                //A function to return nth Fibonacci Number
    if n is equal to 1
        Return 1
    else if n is equal to 2
        Return 1
    end if
    Return F(n-1) + F(n-2)
Now if we call `F(6)`, it will call `F(5)` and `F(4)`, which will call some more. Let's represent this graphically:

[![Recursion Tree for F(6)][1]][1]

From the picture, we can see that `F(6)` will call `F(5)` and `F(4)`. Now `F(5)` will call `F(4)` and `F(3)`. After calculating `F(5)`, we can surely say that all the functions that were called by `F(5)` has been already calculated. That means, we have already calculated `F(4)`. But we are again calculating `F(4)` as `F(6)`'s right child indicates. Do we really need to recalculate? What we can do is, once we have calculated the value of `F(4)`, we'll store it in an array named **dp**, and will reuse it when needed. We'll initialize our **dp** array with **-1**(or any value that won't come in our calculation). Then we'll call **F(6)** where our modified **F(n)** will look like:

    Procedure F(n):
    if n is equal to 1
        Return 1
    else if n is equal to 2
        Return 1
    else if dp[n] is not equal to -1            //That means we have already calculated dp[n]
        Return dp[n]
    else
        dp[n] = F(n-1) + F(n-2)
        Return dp[n]
    end if

We've done the same task as before, but with a simple optimization. That is, we've used [memoization](https://en.wikipedia.org/wiki/Memoization) technique. At first, all the values of **dp** array will be **-1**. When `F(4)` is called, we check if it is empty or not. If it stores **-1**, we will calculate its value and store it in **dp[4]**. If it stores anything but **-1**, that means we've already calculated its value. So we'll simply return the value.

This simple optimization using memoization is called **Dynamic Programming**.

A problem can be solved using Dynamic Programming if it has some characteristics. These are:

 - **Subproblems:**<br/>
 A DP problem can be divided into one or more subproblems. For example: `F(4)` can be divided into smaller subproblems `F(3)` and `F(2)`. As the subproblems are similar to our main problem, these can be solved using same technique.
 - **Overlapping Subproblems:**<br/>
A DP problem must have overlapping subproblems. That means there must be some common part for which same function is called more than once. For example: `F(5)` and `F(6)` has `F(3)` and `F(4)` in common. This is the reason we stored the values in our array.

[![Overlapping Subproblem Graph][2]][2]

 - **Optimal Substructure:**<br/>
Let's say you are asked to minimize the function `g(x)`. You know that the value of `g(x)` depends on `g(y)` and `g(z)`. Now if we can minimize `g(x)` by minimizing both `g(y)` and `g(z)`, only then we can say that the problem has optimal substructure. If `g(x)` is minimized by only minimizing `g(y)` and if minimizing or maximizing `g(z)` doesn't have any effect on `g(x)`, then this problem doesn't have optimal substructure. In simple words, if optimal solution of a problem can be found from the optimal solution of its subproblem, then we can say the problem has optimal substructure property.


  [1]: https://i.stack.imgur.com/ngSbS.jpg
  [2]: https://i.stack.imgur.com/7OVJm.jpg

## Understanding State in Dynamic Programming
Let's discuss with an example. From **n** items, in how many ways you can choose **r** items? You know it is denoted by [![nCr][1]][1]. Now think of a single item.

 - If you don't select the item, after that you have to take **r** items from remaining **n-1** items, which is given by [![(n-1)Cr][2]][2].
 - If you select the item, after that you have to take **r-1** items from remaining **n-1** items, which is given by [![(n-1)C(r-1)][3]][3].

The summation of these two gives us the total number of ways. That is:<br/>
[![nCr = (n-1)Cr + (n-1)C(r-1)][4]][4]

If we think `nCr(n,r)` as a function that takes `n` and `r` as parameter and determines [![nCr][1]][1], we can write the relation mentioned above as:

    nCr(n,r) = nCr(n-1,r) + nCr(n-1,r-1)
This is a recursive relation. To terminate it, we need to determine base case(s). We know that, [![nC1 = n][5]][5] and [![nCn = 1][6]][6]. Using these two as base cases, our algorithm to determine [![nCr][1]][1] will be:

    Procedure nCr(n, r):
    if r is equal to 1
        Return n
    else if n is equal to r
        Return 1
    end if
    Return nCr(n-1,r) + nCr(n-1,r-1)
If we look at the procedure graphically, we can see some recursions are called more than once. For example: if we take **n** = **8** and **r** = **5**, we get:

[![Recursion Tree that shows overlapping subproblem][7]][7]

We can avoid this repeated call by using an array, **dp**. Since there are **2** parameters, we'll need a 2D array. We'll initialize the **dp** array with **-1**, where **-1** denotes the value hasn't been calculated yet. Our procedure will be:

    Procedure nCr(n,r):
    if r is equal to 1
        Return n
    else if n is equal to r
        Return 1
    else if dp[n][r] is not equal to -1        //The value has been calculated
        Return dp[n][r]
    end if
    dp[n][r] := nCr(n-1,r) + nCr(n-1,r-1)
    Return dp[n][r]
To determine [![nCr][1]][1], we needed two parameters **n** and **r**. These parameters are called *state*. We can simply deduce that the number of states determine the number of dimension of the **dp** array. The size of the array will change according to our need. Our dynamic programming algorithms will maintain the following general pattern:

    Procedure DP-Function(state_1, state_2, ...., state_n)
    Return if reached any base case
    Check array and Return if the value is already calculated.
    Calculate the value recursively for this state
    Save the value in the table and Return
Determining *state* is one of the most crucial part of dynamic programming. It consists of the number of parameters that define our problem and optimizing their calculation, we can optimize the whole problem. 

  [1]: https://i.stack.imgur.com/WDYXl.png
  [2]: https://i.stack.imgur.com/UnhcW.png
  [3]: https://i.stack.imgur.com/TfAm6.png
  [4]: https://i.stack.imgur.com/2ayHg.png
  [5]: https://i.stack.imgur.com/r1oSW.png
  [6]: https://i.stack.imgur.com/af8ba.png
  [7]: https://i.stack.imgur.com/DQatV.jpg

## Constructing a DP Solution
No matter how many problems you solve using dynamic programming(DP), it can still surprise you. But as everything else in life, practice makes you better. Keeping these in mind, we'll look at the process of constructing a solution for DP problems. Other examples on this topic will help you understand what DP is and how it works. In this example, we'll try to understand how to come up with a DP solution from scratch.

**Iterative VS Recursive:**<br/>
There are two techniques of constructing DP solution. They are:
 - Iterative (using for-cycles)
 - Recursive (using recursion)

For example, algorithm for calculating the length of the [Longest Common Subsequence](https://www.wikiod.com/dynamic-programming/subsequence-related-algorithms#Longest Common Subsequence)  of two strings **s1** and **s2** would look like:

    Procedure LCSlength(s1, s2):
    Table[0][0] = 0
    for i from 1 to s1.length
        Table[0][i] = 0
    endfor
    for i from 1 to s2.length
        Table[i][0] = 0
    endfor
    for i from 1 to s2.length
        for j from 1 to s1.length
            if s2[i] equals to s1[j]
                Table[i][j] = Table[i-1][j-1] + 1
            else
                Table[i][j] = max(Table[i-1][j], Table[i][j-1])
            endif
        endfor
    endfor
    Return Table[s2.length][s1.length]
This is an iterative solution. There are a few reasons why it is coded in this way:

 - Iteration is faster than recursion.
 - Determining time and space complexity is easier.
 - Source code is short and clean

Looking at the source code, you can easily understand how and why it works, but it is difficult to understand how to come up with this solution. However the two approaches mentioned above translates into two different pseudo-codes. One uses iteration(Bottom-Up) and another uses recursion(Top-Down) approach. The latter one is also known as memoization technique. However, the two solutions are more or less equivalent and one can be easily transformed into another. For this example, we'll show how to come up with a recursive solution for a problem.

**Example Problem:**<br/>
Let's say, you have **N** (**1, 2, 3, ...., N**) wines placed next to each other on a shelf. The price of **i**th wine is **p\[i]**. The price of the wines increase every year. Suppose this is year **1**, on year **y** the price of the **i**th wine will be: year * price of the wine = **y*****p\[i]**. You want to sell the wines you have, but you have to sell exactly one wine per year, starting from this year. Again, on each year, you are allowed to sell only the leftmost or the rightmost wine and you can't rearrange the wines, that means they must stay in same order as they are in the beginning.

For example: let's say you have **4** wines in the shelf, and their prices are(from left to right):

    +---+---+---+---+
    | 1 | 4 | 2 | 3 |
    +---+---+---+---+
The optimal solution would be to sell the wines in the order **1** -> **4** -> **3** -> **2**, which will give us a total profit of: [![1*1+3*2+2*3+4*4=29][1]][1]

**Greedy Approach:**<br/>
After brainstorming for a while, you might come up with the solution to sell the expensive wine as late as possible. So your *greedy* strategy will be:

    Every year, sell the cheaper of the two (leftmost and rightmost) available wines.
Although the strategy doesn't mention what to do when the two wines cost the same, the strategy kinda feels right. But unfortunately, it isn't. If the prices of the wines are:

    +---+---+---+---+---+
    | 2 | 3 | 5 | 1 | 4 |
    +---+---+---+---+---+
The greedy strategy would sell them in the order **1** -> **2** -> **5** -> **4** -> **3** for a total profit of: [![2*1+3*2+4*3+1*4+5*5=49][2]][2]

But we can do better if we sell the wines in the order **1** -> **5** -> **4** -> **2** -> **3** for a total profit of: [![2*1+4*2+1*3+3*4+5*5=50][3]][3]

This example should convince you that the problem is not so easy as it looks on the first sight. But it can be solved using Dynamic Programming.

**Backtracking:**<br/>
To come up with the memoization solution for a problem finding a backtrack solution comes handy. Backtrack solution evaluates all the valid answers for the problem and chooses the best one. For most of the problems it is easier to come up with such solution. There can be three strategies to follow in approaching a backtrack solution:

 1. it should be a function that calculates the answer using recursion.
 2. it should return the answer with *return* statement.
 3. all the non-local variables should be used as read-only, i.e. the function can modify only local variables and its arguments.

For our example problem, we'll try to sell the leftmost or rightmost wine and recursively  calculate the answer and return the better one. The backtrack solution would look like:

    // year represents the current year
    // [begin, end] represents the interval of the unsold wines on the shelf
    Procedure profitDetermination(year, begin, end):
    if begin > end                  //there are no more wines left on the shelf
        Return 0
    Return max(profitDetermination(year+1, begin+1, end) + year * p[begin], //selling the leftmost item
               profitDetermination(year+1, begin, end+1) + year * p[end])   //selling the rightmost item

If we call the procedure using `profitDetermination(1, 0, n-1)`, where **n** is the total number of wines, it will return the answer.

This solution simply tries all the possible valid combinations of selling the wines. If there are **n** wines in the beginning, it will check [![2^n][4]][4] possibilities. Even though we get the correct answer now, the time complexity of the algorithm grows exponentially.

The correctly written backtrack function should always represent an answer to a well-stated question. In our example, the procedure `profitDetermination`  represents an answer to the question: *What is the best profit we can get from selling the wines with prices stored in the array p, when the current year is year and the interval of unsold wines spans through [begin, end], inclusive?* You should always try to create such a question for your backtrack function to see if you got it right and understand exactly what it does.

**Determining State:** <br/>
*State* is the number of parameters used in DP solution. In this step, we need to think about which of the arguments you pass to the function are redundant, i.e. we can construct them from the other arguments or we don't need them at all. If there are any such arguments, we don't need to pass them to the function, we'll calculate them inside the function.

In the example function `profitDetermination` shown above, the argument `year` is redundant. It is equivalent to the number of wines we have already sold plus one. It can be determined using the total number of wines from the beginning minus the number of wines we have not sold plus one. If we store the total number of wines **n** as a global variable, we can rewrite the function as:

    Procedure profitDetermination(begin, end):
    if begin > end
        Return 0
    year := n - (end-begin+1) + 1        //as described above
    Return max(profitDetermination(begin+1, end) + year * p[begin],
               profitDetermination(begin, end+1) + year * p[end])

We also need to think about the range of possible values of the parameters can get from a valid input. In our example, each of the parameters `begin` and `end` can contain values from **0** to **n-1**. In a valid input, we'll also expect `begin <= end + 1`. There can be `O(n²)` different arguments our function can be called with.

**Memoization:**<br/>
We are now almost done. To transform the backtrack solution with time complexity [![O(2^n)][5]][5] into memoization solution with time complexity [![O(n^2)][6]][6], we will use a little trick which doesn't require much effort.

As noted above, there are only [![O(n^2)][6]][6] different parameters our function can be called with. In other words, there are only [![O(n^2)][6]][6] different things we can actually compute. So where does [![O(2^n)][5]][5] time complexity come from and what does it compute!!

The answer is: the exponential time complexity comes from the repeated recursion and because of that, it computes the same values again and again. If you run the code mentioned above for an arbitrary array of **n** = **20** wines and calculate how many times was the function called for arguments **begin** = **10** and **end** = **10**, you will get a number **92378**. That is a huge waste of time to compute the same answer that many times. What we could do to improve this is to store the values once we have computed them and every time the function asks for an already calculated value, we don't need to run the whole recursion again. We'll have an array **dp[N][N]**, initialize it with **-1** (or any value that will not come in our calculation), where **-1** denotes the value hasn't yet been calculated. The size of the array is determined by the maximum possible value of **begin** and **end** as we'll store the corresponding values of certain **begin** and **end** values in our array. Our procedure would look like:

    Procedure profitDetermination(begin, end):
    if begin > end
        Return 0
    if dp[begin][end] is not equal to -1                //Already calculated
        Return dp[begin][end]
    year := n - (end-begin+1) + 1
    dp[begin][end] := max(profitDetermination(year+1, begin+1, end) + year * p[begin],
               profitDetermination(year+1, begin, end+1) + year * p[end])
    Return dp[begin][end]
This is our required DP solution. With our very simple trick, the solution runs [![O(n^2)][6]][6] time, because there are [![O(n^2)][6]][6] different parameters our function can be called with and for each of them, the function runs only once with [![O(1)][7]][7] complexity.

**Summery:**<br/>
If you can identify a problem that can be solved using DP, use the following steps to construct a DP solution:
 - Create a backtrack function to provide the correct answer.
 - Remove the redundant arguments.
 - Estimate and minimize the maximum range of possible values of function parameters.
 - Try to optimize the time complexity of one function call.
 - Store the values so that you don't have to calculate it twice.

The complexity of a DP solution is: **range of possible values the function can be called with** * **time complexity of each call**.

  [1]: https://i.stack.imgur.com/4QXSS.png
  [2]: https://i.stack.imgur.com/VSftx.png
  [3]: https://i.stack.imgur.com/BAecO.png
  [4]: https://i.stack.imgur.com/R8ime.png
  [5]: https://i.stack.imgur.com/sbk1b.png
  [6]: https://i.stack.imgur.com/GpKoh.png
  [7]: https://i.stack.imgur.com/ZiTVI.png

