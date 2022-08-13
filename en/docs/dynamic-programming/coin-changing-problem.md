---
title: "Coin Changing Problem"
slug: "coin-changing-problem"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Number of Ways to Get Total
Given coins of different denominations and a total, in how many ways can we combine these coins to get the total? Let's say we have `coins = {1, 2, 3}` and a `total = 5`, we can get the total in **5** ways:

 - 1 1 1 1 1
 - 1 1 1 2
 - 1 1 3
 - 1 2 2
 - 2 3

The problem is closely related to knapsack problem. The only difference is we have unlimited supply of coins. We're going to use dynamic programming to solve this problem.

We'll use a 2D array **dp[n][total + 1]** where **n** is the number of different denominations of coins that we have. For our example, we'll need **dp\[3]\[6]**. Here **dp\[i]\[j]** will denote the number of ways we can get **j** if we had coins from **coins\[0]** up to **coins\[i]**. For example **dp\[1]\[2]** will store if we had **coins\[0]** and **coins\[1]**, in how many ways we could make **2**. Let's begin:

For **dp\[0]\[0]**, we are asking ourselves if we had only **1** denomination of coin, that is **coins\[0]** = **1**, in how many ways can we get **0**? The answer is **1** way, which is if we don't take any coin at all. Moving on, **dp\[0]\[1]** will represent if we had only **coins\[0]**, in how many ways can we get **1**? The answer is again **1**. In the same way, **dp\[0]\[2]**, **dp\[0]\[3]**, **dp\[0]\[4]**, **dp\[0]\[5]** will be **1**. Our array will look like:

         +---+---+---+---+---+---+---+
    (den)|   | 0 | 1 | 2 | 3 | 4 | 5 |
         +---+---+---+---+---+---+---+
     (1) | 0 | 1 | 1 | 1 | 1 | 1 | 1 |
         +---+---+---+---+---+---+---+
     (2) | 1 |   |   |   |   |   |   |
         +---+---+---+---+---+---+---+
     (3) | 2 |   |   |   |   |   |   |
         +---+---+---+---+---+---+---+
For **dp\[1]\[0]**, we are asking ourselves if we had coins of **2** denominations, that is if we had **coins\[0]** = **1** and **coins\[1]** = **2**, in how many ways we could make **0**? The answer is **1**, which is by taking no coins at all. For **dp\[1]\[1]**, since **coins\[1]** = **2** is greater than our current total, **2** will not contribute to getting the total. So we'll exclude **2** and count number of ways we can get the total using **coins\[0]**. But this value is already stored in **dp\[0]\[1]**! So we'll take the value from the top. Our first formula:

    if coins[i] > j
        dp[i][j] := dp[i-1][j]
    end if
For **dp\[1]\[2]**, in how many ways can we get **2**, if we had coins of denomination **1** and **2**? We can make **2** using coins of denomination of **1**, which is represented by **dp\[0]\[2]**, again we can take **1** denomination of **2** which is stored in **dp\[1]\[2-coins[i]]**, where **i** = **1**. Why? It'll be apparent if we look at the next example. For **dp\[1]\[3]**, in how many ways can we get **3**, if we had coins of denomination **1** and **2**? We can make **3** using coins of denomination **1** in **1** way, which is stored in **dp\[0]\[3]**. Now if we take **1** denomination of **2**, we'll need **3** - **2** = **1** to get the total. The number of ways to get **1** using the coins of denomination **1** and **2** is stored in **dp\[1]\[1]**, which can be written as, **dp\[i]\[j-coins[i]]**, where **i** = **1**. This is why we wrote the previous value in this way. Our second and final formula will be:

    if coins[i] <= j
        dp[i][j] := dp[i-1][j] + dp[i][j-coins[i]]
    end if
This is the two required formulae to fill up the whole **dp** array. After filling up the array will look like:

         +---+---+---+---+---+---+---+
    (den)|   | 0 | 1 | 2 | 3 | 4 | 5 |
         +---+---+---+---+---+---+---+
     (1) | 0 | 1 | 1 | 1 | 1 | 1 | 1 |
         +---+---+---+---+---+---+---+
     (2) | 1 | 1 | 1 | 2 | 2 | 3 | 3 |
         +---+---+---+---+---+---+---+
     (3) | 2 | 1 | 1 | 2 | 3 | 4 | 5 |
         +---+---+---+---+---+---+---+
**dp\[2]\[5]** will contain our required answer. The algorithm:

    Procedure CoinChange(coins, total):
    n := coins.length
    dp[n][total + 1]
    for i from 0 to n
        dp[i][0] := 1
    end for
    for i from 0 to n
        for j from 1 to (total + 1)
            if coins[i] > j
                dp[i][j] := dp[i-1][j]
            else
                dp[i][j] := dp[i-1][j] + dp[i][j-coins[i]]
            end if
        end for
    end for
    Return dp[n-1][total]
The time complexity of this algorithm is `O(n * total)`, where **n** is the number of denominations of coins we have.

## Minimum Number of Coins to Get Total
Given coins of different denominations and a total, how many coins do we need to combine to get the total if we use minimum number of coins? Let's say we have `coins = {1, 5, 6, 8}` and a `total = 11`, we can get the total using **2** coins which is `{5, 6}`. This is indeed the minimum number of coins required to get **11**. We'll also assume that there are unlimited supply of coins. We're going to use dynamic programming to solve this problem.

We'll use a 2D array **dp[n][total + 1]** where **n** is the number of different denominations of coins that we have. For our example, we'll need **dp\[4]\[12]**. Here **dp\[i]\[j]** will denote the minimum number of coins needed to get **j** if we had coins from **coins\[0]** up to **coins\[i]**. For example **dp\[1]\[2]** will store if we had **coins\[0]** and **coins\[1]**, what is the minimum number of coins we can use to make **2**. Let's begin:

At first, for the **0**th column, can make **0** by not taking any coins at all. So all the values of **0**th column will be **0**. For **dp\[0]\[1]**, we are asking ourselves if we had only **1** denomination of coin, that is **coins\[0]** = **1**, what is the minimum number of coins needed to get **1**? The answer is **1**. For **dp\[0]\[2]**, if we had only **1**, what is the minimum number of coins needed to get **2**. The answer is **2**. Similarly **dp\[0]\[3]** = **3**, **dp\[0]\[4]** = **4** and so on. One thing to mention here is that, if we didn't have a coin of denomination **1**, there might be some cases where the total can't be achieved using **1** coin only. For simplicity we take **1** in our example. After first iteration our **dp** array will look like:

            +---+---+---+---+---+---+---+---+---+---+---+---+---+
     (denom)|   | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11|
            +---+---+---+---+---+---+---+---+---+---+---+---+---+
        (1) | 0 | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11|
            +---+---+---+---+---+---+---+---+---+---+---+---+---+
        (5) | 1 | 0 |   |   |   |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+---+---+---+---+
        (6) | 2 | 0 |   |   |   |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+---+---+---+---+
        (8) | 3 | 0 |   |   |   |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+---+---+---+---+
Moving on, for **dp\[1]\[1]**, we are asking ourselves if we had **coins\[0]** = **1** and **coins\[1]** = **5**, what is the minimum number of coins needed to get **1**? Since **coins\[1]** is greater than our current total, it will not affect our calculation. We'll need to exclude **coins\[5]** and get **1** using **coins\[0]** only. This value is stored in **dp\[0]\[1]**. So we take the value from the top. Our first formula is:

    if coins[i] > j
        dp[i][j] := dp[i-1][j]
This condition will be true until our total is **5** in **dp\[1]\[5]**, for that case, we can make **5** in two ways:
 - We take **5** denominations of **coins\[0]**, which is stored on **dp\[0]\[5]** (from the top).
 - We take **1** denomination of **coins\[1]** and (**5** - **5**) = **0** denominations of **coins\[0]**.

We'll choose the minimum of these two. So **dp\[1]\[5]** = min(**dp\[0]\[5]**, **1** + **dp\[1]\[0]**) = **1**. Why did we mention **0** denominations of **coins\[0]** here will be apparent in our next position.

For **dp\[1]\[6]**, we can make **6** in two ways:
 - We take **6** denominations of **coins\[0]**, which is stored on the top.
 - We can take **1** denomination of **5**, we'll need **6** - **5** = **1** to get the total. The minimum number of ways to get **1** using the coins of denomination **1** and **5** is stored in **dp\[1]\[1]**, which can be written as **dp\[i][j-coins[i]]**, where **i** = **1**. This is why we wrote the previous value in that fashion.

We'll take the minimum of these two ways. So our second formula will be:

    if coins[i] >= j
        dp[i][j] := min(dp[i-1][j], dp[i][j-coins[i]])
Using these two formulae we can fill up the whole table. Our final result will be:

            +---+---+---+---+---+---+---+---+---+---+---+---+---+
     (denom)|   | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11|
            +---+---+---+---+---+---+---+---+---+---+---+---+---+
        (1) | 0 | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11|
            +---+---+---+---+---+---+---+---+---+---+---+---+---+
        (5) | 1 | 0 | 1 | 2 | 3 | 4 | 1 | 2 | 3 | 4 | 5 | 2 | 3 |
            +---+---+---+---+---+---+---+---+---+---+---+---+---+
        (6) | 2 | 0 | 1 | 2 | 3 | 4 | 1 | 1 | 2 | 3 | 4 | 2 | 2 |
            +---+---+---+---+---+---+---+---+---+---+---+---+---+
        (8) | 3 | 0 | 1 | 2 | 3 | 4 | 1 | 1 | 2 | 1 | 2 | 2 | 2 |
            +---+---+---+---+---+---+---+---+---+---+---+---+---+
Our required result will be stored at **dp\[3]\[11]**. The procedure will be:

    Procedure coinChange(coins, total):
    n := coins.length
    dp[n][total + 1]
    for i from 0 to n
        dp[i][0] := 0
    end for
    for i from 1 to (total + 1)
        dp[0][i] := i
    end for
    for i from 1 to n
        for j from 1 to (total + 1)
            if coins[i] > j
                dp[i][j] := dp[i-1][j] 
            else
                dp[i][j] := min(dp[i-1][j], dp[i][j-coins[i]])
            end if
        end for
    end for
    Return dp[n-1][total]
The runtime complexity of this algorithm is: O(n*total) where **n** is the number of denominations of coins.

To print the coins needed, we need to check:
 - if the value came from top, then the current coin is not included.
 - if the value came from left, then the current coin is included.

The algorithm would be:

    Procedure printChange(coins, dp, total):
    i := coins.length - 1
    j := total
    min := dp[i][j]
    while j is not equal to 0
        if dp[i-1][j] is equal to min
            i := i - 1
        else
            Print(coins[i])
            j := j - coins[i]
        end if
    end while
For our example, the direction will be:

[![Direction Array][1]][1]

The values will be **6**, **5**.


  [1]: https://i.stack.imgur.com/hFZil.jpg

