---
title: "Weighted Activity Selection"
slug: "weighted-activity-selection"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Weighted Job Scheduling Algorithm
Weighted Job Scheduling Algorithm can also be denoted as Weighted Activity Selection Algorithm.

The problem is, given certain jobs with their start time and end time, and a profit you make when you finish the job, what is the maximum profit you can make given no two jobs can be executed in parallel?

This one looks like Activity Selection using Greedy Algorithm, but there's an added twist. That is, instead of maximizing the number of jobs finished, we focus on making the maximum profit. The number of jobs performed doesn't matter here.

Let's look at an example:

    +-------------------------+---------+---------+---------+---------+---------+---------+
    |          Name           |    A    |    B    |    C    |    D    |    E    |    F    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |(Start Time, Finish Time)|  (2,5)  |  (6,7)  |  (7,9)  |  (1,3)  |  (5,8)  |  (4,6)  |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Profit          |    6    |    4    |    2    |    5    |    11   |    5    |
    +-------------------------+---------+---------+---------+---------+---------+---------+

The jobs are denoted with a name, their start and finishing time and profit. After a few iterations, we can find out if we perform **Job-A** and **Job-E**, we can get the maximum profit of 17. Now how to find this out using an algorithm?

The first thing we do is sort the jobs by their finishing time in non-decreasing order. Why do we do this? It's because if we select a job that takes less time to finish, then we leave the most amount of time for choosing other jobs. We have:

    +-------------------------+---------+---------+---------+---------+---------+---------+
    |          Name           |    D    |    A    |    F    |    B    |    E    |    C    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |(Start Time, Finish Time)|  (1,3)  |  (2,5)  |  (4,6)  |  (6,7)  |  (5,8)  |  (7,9)  |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Profit          |    5    |    6    |    5    |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+

We'll have an additional temporary array **Acc_Prof** of size **n** (Here, **n** denotes the total number of jobs). This will contain the maximum accumulated profit of performing the jobs. Don't get it? Wait and watch. We'll initialize the values of the array with the profit of each jobs. That means, **Acc_Prof[i]** will at first hold the profit of performing **i-th** job.

    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Acc_Prof        |    5    |    6    |    5    |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
Now let's denote **position 2** with **i**, and **position 1** will be denoted with **j**. Our strategy will be to iterate **j** from **1** to **i-1** and after each iteration, we will increment **i** by 1, until **i** becomes **n+1**.

                                   j        i
    
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |          Name           |    D    |    A    |    F    |    B    |    E    |    C    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |(Start Time, Finish Time)|  (1,3)  |  (2,5)  |  (4,6)  |  (6,7)  |  (5,8)  |  (7,9)  |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Profit          |    5    |    6    |    5    |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Acc_Prof        |    5    |    6    |    5    |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
We check if **Job[i]** and **Job[j]** overlap, that is, if the **finish time** of **Job[j]** is greater than **Job[i]**'s start time, then these two jobs can't be done together. However, if they don't overlap, we'll check if **Acc_Prof[j] + Profit[i] > Acc_Prof[i]**. If this is the case, we will update **Acc_Prof[i] = Acc_Prof[j] + Profit[i]**. That is:

    if Job[j].finish_time <= Job[i].start_time
        if Acc_Prof[j] + Profit[i] > Acc_Prof[i]
            Acc_Prof[i] = Acc_Prof[j] + Profit[i]
        endif
    endif
Here **Acc_Prof[j] + Profit[i]** represents the accumulated profit of doing these two jobs toegther. Let's check it for our example:

Here **Job[j]** overlaps with **Job[i]**. So these to can't be done together. Since our **j** is equal to **i-1**, we increment the value of **i** to **i+1** that is **3**. And we make **j = 1**.

                                   j                   i
    
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |          Name           |    D    |    A    |    F    |    B    |    E    |    C    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |(Start Time, Finish Time)|  (1,3)  |  (2,5)  |  (4,6)  |  (6,7)  |  (5,8)  |  (7,9)  |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Profit          |    5    |    6    |    5    |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Acc_Prof        |    5    |    6    |    5    |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
Now **Job[j]** and **Job[i]** don't overlap. The total amount of profit we can make by picking these two jobs is: **Acc_Prof[j] + Profit[i] = 5 + 5 = 10** which is greater than **Acc_Prof[i]**. So we update **Acc_Prof[i] = 10**. We also increment **j** by 1. We get,

                                             j         i
     
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |          Name           |    D    |    A    |    F    |    B    |    E    |    C    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |(Start Time, Finish Time)|  (1,3)  |  (2,5)  |  (4,6)  |  (6,7)  |  (5,8)  |  (7,9)  |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Profit          |    5    |    6    |    5    |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Acc_Prof        |    5    |    6    |    10   |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
Here, **Job[j]** overlaps with **Job[i]** and **j** is also equal to **i-1**. So we increment **i** by 1, and make **j = 1**. We get,

                                   j                             i
     
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |          Name           |    D    |    A    |    F    |    B    |    E    |    C    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |(Start Time, Finish Time)|  (1,3)  |  (2,5)  |  (4,6)  |  (6,7)  |  (5,8)  |  (7,9)  |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Profit          |    5    |    6    |    5    |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Acc_Prof        |    5    |    6    |    10   |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
Now, **Job[j]** and **Job[i]** don't overlap, we get the accumulated profit **5 + 4 = 9**, which is greater than **Acc_Prof[i]**. We update **Acc_Prof[i] = 9** and increment **j** by 1.

                                         

                                             j                   i
     
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |          Name           |    D    |    A    |    F    |    B    |    E    |    C    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |(Start Time, Finish Time)|  (1,3)  |  (2,5)  |  (4,6)  |  (6,7)  |  (5,8)  |  (7,9)  |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Profit          |    5    |    6    |    5    |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Acc_Prof        |    5    |    6    |    10   |    9    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
Again **Job[j]** and **Job[i]** don't overlap. The accumulated profit is: **6 + 4 = 10**, which is greater than **Acc_Prof[i]**. We again update **Acc_Prof[i] = 10**. We increment **j** by 1. We get:
                                     

                                                       j         i
     
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |          Name           |    D    |    A    |    F    |    B    |    E    |    C    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |(Start Time, Finish Time)|  (1,3)  |  (2,5)  |  (4,6)  |  (6,7)  |  (5,8)  |  (7,9)  |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Profit          |    5    |    6    |    5    |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Acc_Prof        |    5    |    6    |    10   |    10   |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
If we continue this process, after iterating through the whole table using **i**, our table will finally look like:

    +-------------------------+---------+---------+---------+---------+---------+---------+
    |          Name           |    D    |    A    |    F    |    B    |    E    |    C    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |(Start Time, Finish Time)|  (1,3)  |  (2,5)  |  (4,6)  |  (6,7)  |  (5,8)  |  (7,9)  |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Profit          |    5    |    6    |    5    |    4    |    11   |    2    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
    |         Acc_Prof        |    5    |    6    |    10   |    14   |    17   |    8    |
    +-------------------------+---------+---------+---------+---------+---------+---------+
<sub>* A few steps have been skipped to make the document shorter.</sub>

If we iterate through the array **Acc_Prof**, we can find out the maximum profit to be **17**! The pseudo-code:

    Procedure WeightedJobScheduling(Job)
    sort Job according to finish time in non-decreasing order
    for i -> 2 to n
        for j -> 1 to i-1
            if Job[j].finish_time <= Job[i].start_time
                if Acc_Prof[j] + Profit[i] > Acc_Prof[i]
                    Acc_Prof[i] = Acc_Prof[j] + Profit[i]
                endif
            endif
        endfor
    endfor
    
    maxProfit = 0
    for i -> 1 to n
        if maxProfit < Acc_Prof[i]
            maxProfit = Acc_Prof[i]
    return maxProfit
The complexity of populating the **Acc_Prof** array is **O(n<sup>2</sup>).** The array traversal takes **O(n)**. So the total complexity of this algorithm is **O(n<sup>2</sup>).**

Now, If we want to find out which jobs were performed to get the maximum profit, we need to traverse the array in reverse order and if the **Acc_Prof** matches the **maxProfit**, we will push the **name** of the job in a **stack** and subtract **Profit** of that job from **maxProfit**. We will do this until our **maxProfit > 0** or we reach the beginning point of the **Acc_Prof** array. The pseudo-code will look like:

    Procedure FindingPerformedJobs(Job, Acc_Prof, maxProfit):
    S = stack()
    for i -> n down to 0 and maxProfit > 0
        if maxProfit is equal to Acc_Prof[i]
            S.push(Job[i].name
            maxProfit = maxProfit - Job[i].profit
        endif
    endfor
The complexity of this procedure is: **O(n)**.

One thing to remember, if there are multiple job schedules that can give us maximum profit, we can only find one job schedule via this procedure.
            

