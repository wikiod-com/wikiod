---
title: "Various ways of accessing arrays"
slug: "various-ways-of-accessing-arrays"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Iterating an Array with a For Loop in C
    int arr[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    int i;
    for(i = 0; i < 10; i++)
    {
        printf("%d\n", arr[i]);
    }

## Iterating an Array with a While Loop in C
    int arr[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    int i = 0;
    while(i < 10)
    {
        printf("%d\n", arr[i]);
        i++;
    }

## Iterating an Array with a For Each loop in Java
    int [] arr = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    for(int value : arr) {
        System.out.print(value);
        System.out.print("\n");
    }
*Note that the Java foreach is just a for loop with different syntax. Some languages do this and some such as C# use foreach.

## Iterating an Array using recursion in C
    int sumArrayRecursive(int * arr, int index, int arraySize)
    {
        if (index == (arraySize - 1))
        {
            return arr[index];
        }
        return arr[index] + sumArrayRecursive(arr, index + 1, arraySize);
    }

## Applying a Function to Every Value of an Array in Javascript
    var numbers = [1,2,3,4,5];
    var squares = numbers.map(function(x) {
        return x*x;
    });

    // squares is [1,4,9,16,25]

## Calculate Single Value from Array in Javascript
    var arr = [1, 2, 3, 4, 5];
    var sum = arr.reduce((prev, curr) => prev + curr);
    console.log(sum);
    // Output: 15

You can also specify an initial value

    var arr = [1, 2, 3, 4, 5];
    var sum = arr.reduce(function (previousValue, currentValue, currentIndex, array) {
        return previousValue + currentValue;
    }, 100);
    console.log(sum)
    // Output: 115

## Iterating an Array with a Do While loop in C
    int arr[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    int i = 0;
    do
    {
        printf("%d\n", arr[i]);
        i++;
    } while (i < 10);

