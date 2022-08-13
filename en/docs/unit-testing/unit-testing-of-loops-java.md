---
title: "Unit testing of Loops (Java)"
slug: "unit-testing-of-loops-java"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Loops considered as one of the important control structures in any programming language.
There are different ways in which we can achieve loop coverage. 

These methods differ based on type of loop.

Single loops

Nested Loops

Concatenated loops



## Single loop test
These are loops in which their loop body contains no other loops (the innermost loop in case of nested).

In order to have loop coverage, testers should exercise the tests given below.

Test 1 :Design a test in which loop body shouldn’t execute at all (i.e. zero iterations)

Test 2 :Design a test in which loop–control variable be negative (Negative number of iterations)

Test 3 :Design a test in which loop iterates only once

Test 4 :Design a test in which loop iterates twice

Test 5 :Design a test in which loop iterates certain number of times , say m where m < maximum number of iterations possible

Test 6 :Design a test in which loop iterates one less than the maximum number of iterations

Test 7 :Design a test in which loop iterates the maximum number of iterations

Test 8 :Design a test in which loop iterates one more than the maximum number of iterations

Consider the below code example which applies all the conditions specified.

public class SimpleLoopTest {
    
private int[] numbers = {5,-77,8,-11,4,1,-20,6,2,10};
    
    /** Compute total of  positive numbers in the array 
     *  @param numItems number of items to total.
     */
    public int findSum(int numItems)
    {
        int total = 0;
        if (numItems <= 10) 
        {
            for (int count=0; count < numItems; count = count + 1)
            {
              if (numbers[count] > 0)
                 {
                    total = total + numbers[count];
                 }
            }                
        }
        return total;
    }

}

public class TestPass extends TestCase {
    
    public void testname() throws Exception {
        
        SimpleLoopTest s = new SimpleLoopTest();        
        assertEquals(0, s.findSum(0));    //Test 1
        assertEquals(0, s.findSum(-1));   //Test 2
        assertEquals(5, s.findSum(1));    //Test 3
        assertEquals(5, s.findSum(2));    //Test 4
        assertEquals(17, s.findSum(5));   //Test 5
        assertEquals(26, s.findSum(9));   //Test 6
        assertEquals(36, s.findSum(10));  //Test 7
        assertEquals(0, s.findSum(11));   //Test 8
    }

}

## Nested Loops Test
A nested loop is a loop within a loop.

The outer loop changes only after the inner loop is completely finished / interrupted.

In this case, test cases should be designed in such a way that

Start at the innermost loop. Set all the outer loops to their minimum values.
Perform Simple loop testing on the innermost loop (Test3 / Test4 / Test5 / Test6 / Test7).
Continue till all the loops tested

## Concatenated loops Test
Two loops are concatenated if it’s possible to reach one after exiting the other on same path from entrance to exit. Sometimes these two loops are independent to each other. In those cases we can apply the design techniques specified as part of single loop testing.

But if the iteration values in one loop are directly or indirectly related to the iteration values of another loop and they can occur on the same path, then we can consider them as nested loops.

