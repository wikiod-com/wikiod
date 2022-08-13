---
title: "Rod Cutting"
slug: "rod-cutting"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Cutting the Rod to get the maximum profit
Given a rod of length **n** inches and an array of length **m** of prices that contains prices of all pieces of size smaller than n. We have to find the maximum value obtainable by cutting up the rod and selling the pieces. For example, if length of the rod is **8** and the values of different pieces are given as following, then the maximum obtainable value is **22**.

 

           +---+---+---+---+---+---+---+---+
     (price)| 1 | 5 | 8 | 9 | 10| 17| 17| 20|
            +---+---+---+---+---+---+---+---+

We'll use a 2D array **dp[m][n + 1]** where n is the length of the rod and m is the length of the price array. For our example, we'll need **dp[8][9]**. Here **dp[i][j]** will denote the maximum price by selling the rod of length j.We can have the maximum value of length j as a whole or we could have broken the length to maximize the profit.

At first, for the 0th column, it will not contribute anything hence marking all the values as 0. So all the values of 0th column will be 0. For **dp[0][1]**, what is the maximum value we can get by selling rod of length 1.It will be 1.Similarly for rod of length 2 dp[0][2] we can have 2(1+1).This continues till **dp[0][8]**.So after the first iteration our dp[][] array will look like.


 

          +---+---+---+---+---+---+---+---+---+
     (price)| 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
            +---+---+---+---+---+---+---+---+---+
      (1) 1 | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 9 |
            +---+---+---+---+---+---+---+---+---+
      (5) 2 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+
      (8) 3 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+
      (9) 4 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+
     (10) 5 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+
     (17) 6 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+
     (17) 7 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+
     (20) 8 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+

For **dp[2][2]** we hae to ask ourselves that what is the best I can get if I break the rod in two pieces(1,1) or taking the rod as a whole(length=2).We can see that if I break the rod in two pieces the maximum profit I can make is 2 and if if I have the rod as a whole I can sell it for 5.After second iteration the dp[][] array will look like:

 

         +---+---+---+---+---+---+---+---+---+
     (price)| 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
            +---+---+---+---+---+---+---+---+---+
      (1) 1 | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 9 |
            +---+---+---+---+---+---+---+---+---+
      (5) 2 | 0 | 1 | 5 | 6 | 10| 11| 15| 16| 20|
            +---+---+---+---+---+---+---+---+---+
      (8) 3 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+
      (9) 4 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+
     (10) 5 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+
     (17) 6 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+
     (17) 7 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+
     (20) 8 | 0 |   |   |   |   |   |   |   |   |
            +---+---+---+---+---+---+---+---+---+ 

So to calculate dp[i][j] our formula will look like:

    if j>=i
        dp[i][j] = Max(dp[i-1][j], price[i]+arr[i][j-i]);
    else
        dp[i][j] = dp[i-1][j];


After the last iteration our dp[][] array will look like

 

           +---+---+---+---+---+---+---+---+---+
     (price)| 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
            +---+---+---+---+---+---+---+---+---+
      (1) 1 | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 9 |
            +---+---+---+---+---+---+---+---+---+
      (5) 2 | 0 | 1 | 5 | 6 | 10| 11| 15| 16| 20|
            +---+---+---+---+---+---+---+---+---+
      (8) 3 | 0 | 1 | 5 | 8 | 10| 13| 16| 18| 21|
            +---+---+---+---+---+---+---+---+---+
      (9) 4 | 0 | 1 | 5 | 8 | 10| 13| 16| 18| 21|
            +---+---+---+---+---+---+---+---+---+
     (10) 5 | 0 | 1 | 5 | 8 | 10| 13| 16| 18| 21|
            +---+---+---+---+---+---+---+---+---+
     (17) 6 | 0 | 1 | 5 | 8 | 10| 13| 17| 18| 22|
            +---+---+---+---+---+---+---+---+---+
     (17) 7 | 0 | 1 | 5 | 8 | 10| 13| 17| 18| 22|
            +---+---+---+---+---+---+---+---+---+
     (20) 8 | 0 | 1 | 5 | 8 | 10| 13| 17| 18| 22|
            +---+---+---+---+---+---+---+---+---+

We will have the result at **dp[n][m+1]**.

**Implementation in Java**

    public int getMaximumPrice(int price[],int n){
            int arr[][] = new int[n][price.length+1];
            
            for(int i=0;i<n;i++){
                for(int j=0;j<price.length+1;j++){
                    if(j==0 || i==0)
                        arr[i][j] = 0;
                    else if(j>=i){
                        arr[i][j] = Math.max(arr[i-1][j], price[i-1]+arr[i][j-i]);
                    }else{
                        arr[i][j] = arr[i-1][j];
                    }
                }
            }
            return arr[n-1][price.length];
        }
    
**Time Complexity**

    O(n^2)



