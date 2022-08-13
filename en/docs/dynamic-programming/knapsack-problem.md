---
title: "Knapsack Problem"
slug: "knapsack-problem"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

The knapsack problem or rucksack problem is a problem in [combinatorial optimization](https://en.wikipedia.org/wiki/Combinatorial_optimization). Given a set of items, each with a weight and a value, determine the number of each item to include in a collection so that the total weight is less than or equal to a given limit and the total value is as large as possible. It derives its name from the problem faced by someone who is constrained by fixed-size knapsack and must fill it with the most valuable items.

The problem often arises in resource allocation where there are financial constraints and is studied in fields such as [combinatorics](https://en.wikipedia.org/wiki/Combinatorics), [computer science](https://en.wikipedia.org/wiki/Computer_science), [complexity theory](https://en.wikipedia.org/wiki/Computational_complexity_theory), [cryptography](https://en.wikipedia.org/wiki/Cryptography), [applied mathematics](https://en.wikipedia.org/wiki/Applied_mathematics) and [daily fantasy sports](https://en.wikipedia.org/wiki/Daily_fantasy_sports).

The knapsack problem has been studied for more than a century, with early works dating as far back as 1897. The name "knapsack problem" dates back to the early works of mathematician [Tobias Dantzig](https://en.wikipedia.org/wiki/Tobias_Dantzig) (1884-1956) and refers to the commonplace problem of packing your mosty valuable or useful items without overloading your luggage.

## 0-1 Knapsack Problem
Suppose you are asked, given the total weight you can carry on your knapsack and some items with their weight and values, how can you take those items in such a way that the sum of their values are maximum, but the sum of their weights don't exceed the total weight you can carry? The **0-1** indicates either you pick the item or you don't. Also we have one quantity of each item. It means that, you can't split the item. If it was not a **0-1 knapsack problem**, that means if you could have split the items, there's a greedy solution to it, which is called **fractional knapsack problem**.

Let's, for now, concentrate on our problem at hand. For example, let's say we have a knapsack capacity of **7**. We have **4** items. Their weights and values are:

    +----------+---+---+---+---+
    |   Item   | 1 | 2 | 3 | 4 |
    +----------+---+---+---+---+
    |  Weight  | 1 | 3 | 4 | 5 |
    +----------+---+---+---+---+
    |   Value  | 1 | 4 | 5 | 7 |
    +----------+---+---+---+---+
One brute force method would be taking all possible combinations of items. Then we can calculate their total weights and exclude them which exceed our knapsack's capacity and find out the one that gives us maximum value. For **4** items, we'll need to check (**4!** - **1**) = **23** possible combinations (excluding one with no items). This is quite cumbersome when the number of items increase. Here, a few aspects we can notice, that is:

 - We can take lesser items and calculate the maximum value we can get using those items and combine them. So our problem can be divided into subproblems.
 - If we calculate the combinations for item **{1,2}**, we can use it when we calculate **{1, 2, 3}**.
 - If we minimize the weight and maximize the value, we can find out our optimal solution.

For these reasons, we'll use dynamic programming to solve our problem. Our strategy will be: whenever a new item comes, we'll check if we can pick the item or not and again we'll pick the items that give us maximum value. Now if we pick the item, our value will be the value of the item, plus whatever the value we can get by subtracting the value from our capacity and the maximum we can get for that remaining weight. If we don't pick the item, we'll pick the items that gives us maximum value without including the item. Let's try to understand it with our example:

We'll take a 2D array **table**, where the number of columns will be the maximum value we can get by taking the items + 1. And the number of rows will be same as the number of items. Our array will look like:

    +-------+--------+---+---+---+---+---+---+---+---+
    | Value | Weight | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
    +-------+--------+---+---+---+---+---+---+---+---+
    |   1   |    1   | 0 |   |   |   |   |   |   |   |
    +-------+--------+---+---+---+---+---+---+---+---+
    |   4   |    3   | 0 |   |   |   |   |   |   |   |
    +-------+--------+---+---+---+---+---+---+---+---+
    |   5   |    4   | 0 |   |   |   |   |   |   |   |
    +-------+--------+---+---+---+---+---+---+---+---+
    |   7   |    5   | 0 |   |   |   |   |   |   |   |
    +-------+--------+---+---+---+---+---+---+---+---+

We've incorporate the weight and value of each item to the array for our convenience. Remember these are not part of the array, these are for calculation purpose only, you need not store these values in **table** array.

Our first column is filled with **0**. It means if our maximum capacity is **0**, no matter whatever item we have, since we can't pick any items, our maximum value will be **0**. Let's start from **Table\[0]\[1]**. When we are filling **Table\[1]\[1]**, we are asking ourselves if our maximum capacity was **1** and we had only the first item, what would be our maximum value? The best we can do is **1**, by picking the item. For **Table\[0]\[2]** that means if our maximum capacity is **2** and we only have the first item, the maximum value we can get is **1**. This will be same for **Table\[0]\[3]**, **Table\[0]\[4]**, **Table\[0]\[5]**, **Table\[0]\[6]** and **Table\[0]\[7]**. This is because we only have one item, which gives us value **1**. Since we have only **1** quantity of each item, no matter how we increase the capacity of our knapsack, from one item, **1** is the best value we can make. So our array will look like:

    +-------+--------+---+---+---+---+---+---+---+---+
    | Value | Weight | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
    +-------+--------+---+---+---+---+---+---+---+---+
    |   1   |    1   | 0 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
    +-------+--------+---+---+---+---+---+---+---+---+
    |   4   |    3   | 0 |   |   |   |   |   |   |   |
    +-------+--------+---+---+---+---+---+---+---+---+
    |   5   |    4   | 0 |   |   |   |   |   |   |   |
    +-------+--------+---+---+---+---+---+---+---+---+
    |   7   |    5   | 0 |   |   |   |   |   |   |   |
    +-------+--------+---+---+---+---+---+---+---+---+

Moving on, for **Table\[1]\[1]**, we are asking ourselves, if we had item **1** and **2** and if the maximum capacity of our knapsack was **1**, what is the maximum value we can get? If we take both item **1** and **2**, the total weight will be **4**, which will exceed our current knapsack capacity. So item **2** can't be selected. Now what is the best we can do without taking item **2**? The value from the top, that is **Table\[0]\[1]** which contains the maximum value we can get if we had maximum capacity **1** and we didn't pick the second item. For **Table\[1]\[2]**, since **2** is less than **weight\[2]**, that is the weight of the second item, we can't take the item. So we can establish that, if the weight of the current item is greater than our maximum capacity, we can't take that item. In this case, we'll simply take the value from top, which represents the maximum value we can take excluding the item.

    if weight[i] > j
        Table[i][j] := Table[i-1][j]
    end if
Now for **Table\[1]\[3]** since our maximum capacity is equal to our current weight, we have two choices.
 - We pick the item and add its value with the maximum value we can get from other remaining items after taking this item.
 - We can exclude this item.

Among the two choices, we'll pick the one from which we can get maximum value. If we select the item, we get: value for this item + maximum value from rest of the items after taking this item = **4** + **0** = **4**. We get **4**(value of the item) from our **weight** array and the **0**(maximum value we can get from rest of the items after taking this item) comes by going **1** step above and **3** steps back. That is **Table\[0]\[0]**. Why? If we take the item, our remaining capacity will be **3** - **3** = **0** and remaining item will be the first item. Well, if you recall **Table\[0]\[0]** stores the maximum value we can get if our capacity was **0** and we only had the first item. Now if we don't select the item, the maximum value we can get is comes from **1** step above, that is **1**. Now we take the maximum of these two values(**4**, **1**) and set **Table\[1]\[2]** = **4**. For **Table\[1]\[4]**, since **4**, the maximum knapsack capacity is greater than **3**, the weight of our current item, we again have two options. We take max(**Weight\[2]** + **Table\[0]\[4-Weight\[2]]**, **Table\[0]\[4]**) = max(**Weight\[2] + Table\[0]\[1]**, **Table\[0]\[4]**) = max(**4** + **1**, **1**) = **5**.

    if weight[i] <= j
        w := weight[i]
        Table[i][j] := max(w + Table[i][j-w], Table[i-1][j])
    end if
Using these two formulae, we can populate the whole **Table** array. Our array will look like:

    +-------+--------+---+---+---+---+---+---+---+---+
    | Value | Weight | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
    +-------+--------+---+---+---+---+---+---+---+---+
    |   1   |    1   | 0 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
    +-------+--------+---+---+---+---+---+---+---+---+
    |   4   |    3   | 0 | 1 | 1 | 4 | 5 | 5 | 5 | 5 |
    +-------+--------+---+---+---+---+---+---+---+---+
    |   5   |    4   | 0 | 1 | 1 | 4 | 5 | 6 | 6 | 9 |
    +-------+--------+---+---+---+---+---+---+---+---+
    |   7   |    5   | 0 | 1 | 1 | 4 | 5 | 7 | 8 | 9 |
    +-------+--------+---+---+---+---+---+---+---+---+
Here, The last value that we inserted in our array, **Table\[3]\[7]** contains our required maximum value. This is the maximum value we can get if we had **4** items and our maximum capacity of the knapsack was **7**.

Here one thing we must remember that, even for the first row, the weight can be greater than the capacity of the knapsack. We'll need to keep another constraint to check the value while filling the first row. Or we can simply take another row and set all the values of the first row to **0**. The pseudo-code would look like:

    Procedure Knapsack(Weight, Value, maxCapacity):
    n := Item.size - 1
    Table[n+1][maxCapacity+1]
    for i from 0 to n
        Table[i][0] := 0
    end for
    for j from 1 to maxCapacity
        if j >= Weight[0]
            Table[0][j] := Weight[0]
        else
            Table[0][j] := 0
        end if
    end for
    for i from 1 to n
        for j from 1 to maxCapacity
            if Weight[i] >= j                                            //can't pick the item
                Table[i][j] := Table[i-1][j]        
            else                                                        //can pick the item
                w := Weight[i]
                Table[i][j] := max(w + Table[i-1][j-w], Table[i-1][j])
            end if
        end for
    end for
    Return Table[n][maxCapacity]
The time complexity of this algorithm is `O(n*maxCapacity)`, where **n** is the number of items and `maxCapacity` is the maximum capacity of our knapsack.

So far, we've found the maximum value we can get from our example. One question still remains. What are the actual items? We'll retrace the values in our **Table** array to find out the items we've taken. We'll follow two strategies:
 - For any item, if the value is coming from the position above, we didn't take the current item. We go 1 step above.
 - If the value is not coming from the position above, we took the item. So we go 1 step above and x steps back where x is the weight of the current item.

The pseudo-code will be:

    Procedure printItems(Table, maxCapacity, Value):
    i := Item.size - 1
    j := maxCapacity
    while j is not equal to 0
        if Table[i][j] is equal to Table[i-1][j]
            i := i - 1
        else
            Print: i
            j := j - weight[i]
            i := i - 1
        end if
    end while
If we retrace our example, we'll get:

[![Retraced Array][1]][1]

From this, we can say that, we can take item **2** and **3** to get the maximum value.


  [1]: https://i.stack.imgur.com/UrImV.jpg

