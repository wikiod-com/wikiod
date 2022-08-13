---
title: "Subsequence Related Algorithms"
slug: "subsequence-related-algorithms"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Longest Increasing Subsequence
The task is to find the length of the longest subsequence in a given array of integers such that all elements of the subsequence are sorted in ascending order. For example, the length of the longest increasing subsequence(LIS) for **{15, 27, 14, 38, 26, 55, 46, 65, 85}** is **6** and the longest increasing subsequence is **{15, 27, 38, 55, 65, 85}**. Again for **{3, 4, -1, 0, 6, 2, 3}** length of LIS is **4** and the subsequence is **{-1, 0, 2, 3}**. We'll use dynamic programming to solve this problem.

Let's take the second example: `Item = {3, 4, -1, 0, 6, 2, 3}`. We'll start by taking the an array **dp** of the same size of our sequence. **dp\[i]** represents the length of the LIS if we include the **i**th item of our original sequence. At the very beginning we know that for each and every item at least the longest increasing subsequence is of length **1**. That is by considering the single element itself. So we'll initialize the **dp** array with **1**. We'll have two variables **i** and **j**. Initially **i** will be **1** and **j** will be **0**. Our array will look like:

               3     4    -1     0     6     2     3
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Index |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Value |  1  |  1  |  1  |  1  |  1  |  1  |  1  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
               j     i
The number above the array represents the corresponding element of our sequence. Our strategy will be:

    if Item[i] > Item[j]
        dp[i] := dp[j] + 1
That means if element at **i** is greater than element at **j**, the length of the LIS that contains element at **j**, will increase by length **1** if we include element at **i** with it. In our example, for **i** = **1** and **j** = **0**, **Item\[i]**  is greater than **Item\[j]**. So **dp\[i]** = **dp\[j]** + **1**. Our array will look like:

               3     4    -1     0     6     2     3
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Index |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Value |  1  |  2  |  1  |  1  |  1  |  1  |  1  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
               j     i

At each step, we'll increase **j** up to **i** and then reset **j** to **0** and increment **i**. For now, **j** has reached **i**, so we increment **i** to **2**.

               3     4    -1     0     6     2     3
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Index |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Value |  1  |  2  |  1  |  1  |  1  |  1  |  1  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
               j           i
For **i** = **2**, **j** = **0**, **Item\[i]** is not greater than **Item\[j]**, so we do nothing and increment **j**.

               3     4    -1     0     6     2     3
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Index |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Value |  1  |  2  |  1  |  1  |  1  |  1  |  1  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
                     j     i
For **i** = **2**, **j** = **1**, **Item\[i]** is not greater than **Item\[j]**, so we do nothing and since **j** has reached its limit, we increment **i** and reset **j** to **0**.

               3     4    -1     0     6     2     3
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Index |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Value |  1  |  2  |  1  |  1  |  1  |  1  |  1  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
               j                 i
For **i** = **3**, **j** = **0**, **Item\[i]** is not greater than **Item\[j]**, so we do nothing and increment **j**.

               3     4    -1     0     6     2     3
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Index |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Value |  1  |  2  |  1  |  1  |  1  |  1  |  1  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
                     j           i
For **i** = **3**, **j** = **1**, **Item\[i]** is not greater than **Item\[j]**, so we do nothing and increment **j**.

               3     4    -1     0     6     2     3
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Index |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Value |  1  |  2  |  1  |  1  |  1  |  1  |  1  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
                           j     i
For **i** = **3**, **j** = **2**, **Item\[i]** is greater than **Item\[j]**, so **dp\[i]** = **dp\[j]** + **1**. After that since **j** has reached its limit, again we reset **j** to **0** and increment **i**.

               3     4    -1     0     6     2     3
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Index |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Value |  1  |  2  |  1  |  2  |  1  |  1  |  1  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
               j                       i
For **i** = **4** and **j** = **0**, **Item\[i]** is greater than **Item\[j]**, so **dp\[i]** = **dp\[j]** + **1**. After that, we increment **j**.

               3     4    -1     0     6     2     3
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Index |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Value |  1  |  2  |  1  |  2  |  2  |  1  |  1  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
                     j                 i
For **i** = **4** and **j** = **1**, **Item\[i]** is greater than **Item\[j]**. We can also notice that **dp\[i]** = **dp\[j]** + **1** will provide us **3**, which means if we take the LIS for **Item\[j]** and add **Item\[i]** with it, we'll get a better LIS{3,4,6} than before {3,6}. So we set **dp\[i]** = **dp\[j]** + **1**. Then we increment **j**.

               3     4    -1     0     6     2     3
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Index |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Value |  1  |  2  |  1  |  2  |  3  |  1  |  1  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
                           j           i
For **i** = **4** and **j** = **2**, **Item\[i]** is greater than **Item\[j]**. But for this case, if we set **dp\[i]** = **dp\[j]** + **1**, we'll get **2**, which is{-1,6} not the best{3,4,6} we can do using **Item\[i]**. So we discard this one. We'll add a condition to our strategy, that is:

    if Item[i]>Item[j] and dp[i]<dp[j] + 1
        dp[i] := dp[j] + 1
We increment **j** by **1**. Following this strategy, if we complete our **dp** array, it will look like:

               3     4    -1     0     6     2     3
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Index |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
    | Value |  1  |  2  |  1  |  2  |  3  |  3  |  4  |
    +-------+-----+-----+-----+-----+-----+-----+-----+
                                             j     i
Now we'll iterate through this array and find out the maximum value, which will be the length of the LIS. Our pseudo-code will be:

    Procedure LISLength(Item):
    n := Item.length
    dp[] := new Array(n)
    for i from 0  to n
        dp[i] := 1
    end for
    for i from 1 to n
        for j from 0 to i
            if Item[i]>Item[j] and dp[i]<dp[j] + 1
                dp[i] := dp[j] + 1
            end if
        end for
    end for
    l := -1
    for i from 0 to n
        l := max(l, dp[i])
    end for
    Return l
The time complexity of this algorithm is `O(n²)` where **n** is the length of the sequence.

To find out the original sequence, we need to iterate backwards and match it with our length. The procedure is:

    Procedure LIS(Item, dp, maxLength):
    i := Item.length
    while dp[i] is not equal to maxLength
        i := i - 1
    end while
    s = new Stack()
    s.push(Item[i])
    maxLength := maxLength - 1
    current := Item[i]
    while maxLength is not equal to 0
        i := i-1
        if dp[i] := maxLength and Item[i] < current
            current := Item[i]
            s.push(current)
            maxLength := maxLength - 1
        end if
    end while
    while s is not empty
        x := s.pop
        Print(s)
    end while
The time complexity of this algorithm is: `O(n)`.

## Longest Palindromic Subsequence
Given a string what is the longest palindromic subsequence(LPS) of it? Let's take a string **agbdba**. The LPS of this string is **abdba** of length **5**. Remember, since we're looking for *subsequence*, the characters need not to be continuous in the original string. The longest palindromic *substring* of the sequence would be **bdb** of length **3**. But we'll concentrate on the *subsequence* here. We're going to use dynamic programming to solve this problem.

At first, we'll take a 2D array of the same dimension of our original sequence. For our example: `S = "agbdba"`, we'll take **dp\[6]\[6]** array. Here, **dp\[i]\[j]** represents the length of the LPS we can make if we consider the characters from **s\[i]** to **s\[j]**. For example. if our string was **aa**, **dp\[0]\[1]** would store **2**. Now we'll consider different lengths of our string and find out the longest possible length we can make out of it.

**Length = 1**:<br/>
Here, we are considering only **1** character at a time. So if we had a string of length **1**, what is the LPS we can have? Of course the answer is **1**. How to store it? **dp\[i]\[j]** where **i** is equal to **j** represents a string of length **1**. So we'll set **dp\[0]\[0]**, **dp\[1]\[1]**, **dp\[2]\[2]**, **dp\[3]\[3]**, **dp\[4]\[4]**, **dp\[5]\[5]** to **1**. Our array will look like:

    +---+---+---+---+---+---+---+
    |   | 0 | 1 | 2 | 3 | 4 | 5 |
    +---+---+---+---+---+---+---+
    | 0 | 1 |   |   |   |   |   |
    +---+---+---+---+---+---+---+
    | 1 |   | 1 |   |   |   |   |
    +---+---+---+---+---+---+---+
    | 2 |   |   | 1 |   |   |   |
    +---+---+---+---+---+---+---+
    | 3 |   |   |   | 1 |   |   |
    +---+---+---+---+---+---+---+
    | 4 |   |   |   |   | 1 |   |
    +---+---+---+---+---+---+---+
    | 5 |   |   |   |   |   | 1 |
    +---+---+---+---+---+---+---+
**Length = 2**:<br/>
This time we'll consider strings of length **2**. Now considering strings of length **2**, the maximum length of LPS can be **2** if and only if the two characters of the string is same. So our strategy will be:

    j := i + Length - 1
    if s[i] is equal to s[j]
        dp[i][j] := 2
    else
        dp[i][j] := 1
If we fill our array following the strategy for **Length** = **2**, we'll get:

    +---+---+---+---+---+---+---+
    |   | 0 | 1 | 2 | 3 | 4 | 5 |
    +---+---+---+---+---+---+---+
    | 0 | 1 | 1 |   |   |   |   |
    +---+---+---+---+---+---+---+
    | 1 |   | 1 | 1 |   |   |   |
    +---+---+---+---+---+---+---+
    | 2 |   |   | 1 | 1 |   |   |
    +---+---+---+---+---+---+---+
    | 3 |   |   |   | 1 | 1 |   |
    +---+---+---+---+---+---+---+
    | 4 |   |   |   |   | 1 | 1 |
    +---+---+---+---+---+---+---+
    | 5 |   |   |   |   |   | 1 |
    +---+---+---+---+---+---+---+
**Length = 3**:<br/>
Now we're looking at **3** characters at a time for our original string. From now on the LPS we can make from our string will be determined by:
 - If the first and last characters match we'll have at least **2** items from which we can make the LPS + if we exclude the first and last character, what ever the best we can make from the remaining string.
 - If the first and last characters do not match, the LPS we can make will come from either excluding the first character or the last character, which we've already calculated.

To summerize,

    j := i + Length - 1
    if s[i] is equal to s[j]
        dp[i][j] := 2 + dp[i+1][j-1]
    else
        dp[i][j] := max(dp[i+1][j], dp[i][j-1])
    end if
If we fill the **dp** array for **Length** = **3** to **Length** = **6**, we'll get:

    +---+---+---+---+---+---+---+
    |   | 0 | 1 | 2 | 3 | 4 | 5 |
    +---+---+---+---+---+---+---+
    | 0 | 1 | 1 | 1 | 1 | 3 | 5 |
    +---+---+---+---+---+---+---+
    | 1 |   | 1 | 1 | 1 | 3 | 3 |
    +---+---+---+---+---+---+---+
    | 2 |   |   | 1 | 1 | 3 | 3 |
    +---+---+---+---+---+---+---+
    | 3 |   |   |   | 1 | 1 | 1 |
    +---+---+---+---+---+---+---+
    | 4 |   |   |   |   | 1 | 1 |
    +---+---+---+---+---+---+---+
    | 5 |   |   |   |   |   | 1 |
    +---+---+---+---+---+---+---+
This is our required **dp** array and **dp\[0]\[5]** will contain the length of the LPS. Our procedure will look like:

    Procedure LPSLength(S):
    n := S.length
    dp[n][n]
    for i from 0 to n
        dp[i][i] := 1
    end for
    for i from 0 to (n-2)
        if S[i] := S[i+1]
            dp[i][i+1] := 2
        else
            dp[i][i+1] := 1
        end if
    end for
    Length := 3
    while Length <= n
        for i from 0 to (n - Length)
            j := i + Length - 1
            if S[i] is equal to s[j]
                dp[i][j] := 2 + dp[i+1][j-1]
            else
                dp[i][j] := max(dp[i+1][j], dp[i][j-1])
            end if
        end for
    Length := Length + 1
    end while
    Return dp[0][n-1]
The time complexity of this algorithm is `O(n²)`, where **n** is the length of our given string. Longest Palindromic Subsequence problem is closely related to Longest Common Subsequence. If we take the second string as the reverse of the first string and calculate the length and print the result, that will be the longest palindromic subsequence of the given string. The complexity of that algorithm is also `O(n²)`.

## Longest Common Subsequence
One of the most important implementations of Dynamic Programming is finding out the [Longest Common Subsequence](https://en.wikipedia.org/wiki/Longest_common_subsequence_problem). Let's define some of the basic terminologies first.

**Subsequence:**

A subsequence is a sequence that can be derived from another sequence by deleting some elements without changing the order of the remaining elements. Let's say we have a string **ABC**. If we erase zero or one or more than one character from this string we get the subsequence of this string. So the subsequences of string **ABC** will be {**"A"**, **"B"**, **"C"**, **"AB"**, **"AC"**, **"BC"**, **"ABC"**, **" "**}. Even if we remove all the characters, the empty string will also be a subsequence. To find out the subsequence, for each characters in a string, we have two options - either we take the character, or we don't. So if the length of the string is **n**, there are **2<sup>n</sup>** subsequences of that string.

**Longest Common Subsequence:**

As the name suggest, of all the common subsequencesbetween two strings, the longest common subsequence(LCS) is the one with the maximum length. For example: The common subsequences between **"HELLOM"** and **"HMLD"** are **"H"**, **"HL"**, **"HM"** etc. Here **"HLL"** is the longest common subsequence which has length 3.

**Brute-Force Method:**

We can generate all the subsequences of two strings using *backtracking*. Then we can compare them to find out the common subsequences. After we'll need to find out the one with the maximum length. We have already seen that, there are **2<sup>n</sup>** subsequences of a string of length **n**. It would take years to solve the problem if our **n** crosses **20-25**.

**Dynamic Programming Method:**

Let's approach our method with an example. Assume that, we have two strings **abcdaf** and **acbcf**. Let's denote these with **s1** and **s2**. So the longest common subsequence of these two strings will be **"abcf"**, which has length 4. Again I remind you, subsequences need not be continuous in the string. To construct **"abcf"**, we ignored **"da"** in **s1** and **"c"** in **s2**. How do we find this out using Dynamic Programming?

We'll start with a table (a 2D array) having all the characters of **s1** in a row and all the characters of **s2** in column. Here the table is 0-indexed and we put the characters from 1 to onwards. We'll traverse the table from left to right for each row. Our table will look like:

                  0     1     2     3     4     5     6
         +-----+-----+-----+-----+-----+-----+-----+-----+
         | chʳ |     |  a  |  b  |  c  |  d  |  a  |  f  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      0  |     |     |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      1  |  a  |     |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      2  |  c  |     |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      3  |  b  |     |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      4  |  c  |     |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      5  |  f  |     |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
Here each row and column represent the length of the longest common subsequence between two strings if we take the characters of that row and column and add to the prefix before it. For example: **Table[2][3]** represents the length of the longest common subsequence between **"ac"** and **"abc"**.

The 0-th column represents the empty subsequence of **s1**. Similarly the 0-th row represents the empty subsequence of **s2**. If we take an empty subsequence of a string and try to match it with another string, no matter how long the length of the second substring is, the common subsequence will have 0 length. So we can fill-up the 0-th rows and 0-th columns with 0's. We get:

                  0     1     2     3     4     5     6
         +-----+-----+-----+-----+-----+-----+-----+-----+
         | chʳ |     |  a  |  b  |  c  |  d  |  a  |  f  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      0  |     |  0  |  0  |  0  |  0  |  0  |  0  |  0  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      1  |  a  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      2  |  c  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      3  |  b  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      4  |  c  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      5  |  f  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
Let's begin. When we're filling **Table[1][1]**, we're asking ourselves, if we had a string **a** and another string **a** and nothing else, what will be the longest common subsequence here? The length of the LCS here will be 1. Now let's look at **Table[1][2]**. We have string **ab** and string **a**. The length of the LCS will be 1. As you can see, the rest of the values will be also 1 for the first row as it considers only string **a** with **abcd**, **abcda**, **abcdaf**. So our table will look like:

                  0     1     2     3     4     5     6
         +-----+-----+-----+-----+-----+-----+-----+-----+
         | chʳ |     |  a  |  b  |  c  |  d  |  a  |  f  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      0  |     |  0  |  0  |  0  |  0  |  0  |  0  |  0  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      1  |  a  |  0  |  1  |  1  |  1  |  1  |  1  |  1  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      2  |  c  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      3  |  b  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      4  |  c  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      5  |  f  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
For row 2, which will now include **c**. For **Table[2][1]** we have **ac** on one side and **a** on the other side. So the length of the LCS is 1. Where did we get this 1 from? From the top, which denotes the LCS **a** between two substrings. So what we are saying is, if **s1[2]** and **s2[1]** are not same, then the length of the LCS will be the maximum of the length of LCS at the **top**, or at the **left**. Taking the length of the LCS at the top denotes that, we don't take the current character from **s2**. Similarly, Taking the length of the LCS at the left denotes that, we don't take the current character from **s1** to create the LCS. We get:

                  0     1     2     3     4     5     6
         +-----+-----+-----+-----+-----+-----+-----+-----+
         | chʳ |     |  a  |  b  |  c  |  d  |  a  |  f  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      0  |     |  0  |  0  |  0  |  0  |  0  |  0  |  0  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      1  |  a  |  0  |  1  |  1  |  1  |  1  |  1  |  1  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      2  |  c  |  0  |  1  |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      3  |  b  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      4  |  c  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      5  |  f  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
So our first formula will be:

    if s2[i] is not equal to s1[j]
        Table[i][j] = max(Table[i-1][j], Table[i][j-1]
    endif
Moving on, for **Table[2][2]** we have string **ab** and **ac**. Since **c** and **b** are not same, we put the maximum of the top or left here. In this case, it's again 1. After that, for **Table[2][3]** we have string **abc** and **ac**. This time current values of both row and column are same. Now the length of the LCS will be equal to the maximum length of LCS so far + 1. How do we get the maximum length of LCS so far? We check the diagonal value, which represents the best match between **ab** and **a**. From this state, for the current values, we added one more character to **s1** and **s2** which happened to be the same. So the length of LCS will of course increase. We'll put **1 + 1 = 2** in **Table[2][3]**. We get,

                  0     1     2     3     4     5     6
         +-----+-----+-----+-----+-----+-----+-----+-----+
         | chʳ |     |  a  |  b  |  c  |  d  |  a  |  f  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      0  |     |  0  |  0  |  0  |  0  |  0  |  0  |  0  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      1  |  a  |  0  |  1  |  1  |  1  |  1  |  1  |  1  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      2  |  c  |  0  |  1  |  1  |  2  |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      3  |  b  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      4  |  c  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      5  |  f  |  0  |     |     |     |     |     |     |
         +-----+-----+-----+-----+-----+-----+-----+-----+
So our second formula will be:

    if s2[i] equals to s1[j]
        Table[i][j] = Table[i-1][j-1] + 1
    endif
We have defined both the cases. Using these two formulas, we can populate the whole table. After filling up the table, it will look like this:

                  0     1     2     3     4     5     6
         +-----+-----+-----+-----+-----+-----+-----+-----+
         | chʳ |     |  a  |  b  |  c  |  d  |  a  |  f  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      0  |     |  0  |  0  |  0  |  0  |  0  |  0  |  0  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      1  |  a  |  0  |  1  |  1  |  1  |  1  |  1  |  1  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      2  |  c  |  0  |  1  |  1  |  2  |  2  |  2  |  2  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      3  |  b  |  0  |  1  |  2  |  2  |  2  |  2  |  2  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      4  |  c  |  0  |  1  |  2  |  3  |  3  |  3  |  3  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
      5  |  f  |  0  |  1  |  2  |  3  |  3  |  3  |  4  |
         +-----+-----+-----+-----+-----+-----+-----+-----+
The length of the LCS between **s1** and **s2** will be **Table[5][6] = 4**. Here, 5 and 6 are the length of **s2** and **s1** respectively. Our pseudo-code will be:

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
The time complexity for this algorithm is: **O(mn)** where **m** and **n** denotes the length of each strings.

How do we find out the longest common subsequence? We'll start from the bottom-right corner. We will check from where the value is coming. If the value is coming from the diagonal, that is if **Table[i-1][j-1]** is equal to **Table[i][j] - 1**, we push either **s2[i]** or **s1[j]** (both are the same) and move diagonally. If the value is coming from top, that means, if **Table[i-1][j]** is equal to **Table[i][j]**, we move to the top. If the value is coming from left, that means, if **Table[i][j-1]** is equal to **Table[i][j]**, we move to the left. When we reach the leftmost or topmost column, our search ends. Then we pop the values from the stack and print them. The pseudo-code:

    Procedure PrintLCS(LCSlength, s1, s2)
    temp := LCSlength
    S = stack()
    i := s2.length
    j := s1.length
    while i is not equal to 0 and j is not equal to 0
         if Table[i-1][j-1] == Table[i][j] - 1 and s1[j]==s2[i]
            S.push(s1[j])   //or S.push(s2[i])
            i := i - 1
            j := j - 1
        else if Table[i-1][j] == Table[i][j]
            i := i-1
        else
            j := j-1
        endif
    endwhile
    while S is not empty
        print(S.pop)
    endwhile
Point to be noted: if both **Table[i-1][j]** and **Table[i][j-1]** is equal to **Table[i][j]** and **Table[i-1][j-1]** is not equal to **Table[i][j] - 1**, there can be two LCS for that moment. This pseudo-code doesn't consider this situation. You'll have to solve this recursively to find multiple LCSs.

The time complexity for this algorithm is: **O(max(m, n))**.

