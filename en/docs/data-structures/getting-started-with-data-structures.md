---
title: "Getting started with data-structures"
slug: "getting-started-with-data-structures"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Intro to Data Structures
A data structure is a way of organizing and storing information.

Let a "Hello, World!" string be the information that we need to organize and store in byte-addressable memory.

Each ASCII character requires 7 bits of storage. Most systems reserve 8 bits (1 byte) for each character, so each character in "Hello, World!" is stored in an individual byte-sized unit of memory, one after another, consecutively.

We need a single reference to our string even though it spans multiple memory addresses, so we use the address of the first character in the string, 'H'. Every other character can be accessed at *the address of 'H' + the index of that character* using zero-indexed characters.

We want to print our string, "Hello, World!" We know its address in memory, which we supply to the print function, but how does the print function know to stop printing consecutive memory locations? One common approach is to append the null character, '\0', to the string. When the print function encounters the null character, it knows that it has reached the end of the string.

We have defined a way of organizing and storing our string, i.e. a data structure! This very simple data structure is a null-terminated character array, which is one way to organize and store a string.

## Array : A Simple Data Structure
> An Array Data Structure is used to store similar objects (or data
> values) in a contiguous block of memory. Array Data Structure has
> fixed size, which determines the number of data values that can be
> stored in it.


----------
**Array : The C++ Way**

 

In C++ Programming Language, we can declare a static array as follow

    int arrayName[100];

Here we have declared an array named as "arrayName" which can store up to 100 values, all of which are of the same type, that is an integer.

Now, we will discuss some advantages and disadvantages of this Data Structure
 1. We can access Data Values stored in Array in Constant Time, that is the time complexity is [O(1)][1]. So if we want to access the data value stored at the i-th position,  we need not start from starting position and move up to the i-th position, but we can directly jump to i-th position thus saving computing time. 
 2. Inserting an element in middle of an array is not an efficient task. Suppose we want to add a new element in the array at i-th position, then we need to first move all element at (i-th) and (i+1 th) position to create space for new element.  Example : `1 4 2 0 ` is an array with 4 elements , now we want to insert 3 at 2nd position then we need to move 4,2 and 0 one position further to create space for 3. 

  

      1 3 4 2 0
3. Similar to insertion of the element, deletion of an element from an i-th position in an array is also not efficient since we need to move all elements ahead of the deleted element by 1 block in order to fill the vacant space created by the deleted element. 

These are 3 simple characteristics of an array, Here you might believe that array is not an efficient Data Structure, but in practice, Advantage of an array can outweight its dis-advantages. This largely depend upon the kind of purpose you want to serve, it might be possible that you don't want to insert or delete element as often as you want to access them, in that case, an array is an absolutely perfect Data Structure. 

The sole purpose of introducing this data structure is to make sure you simply don't choose Data Structure based on the number of advantages and disadvantage, but you should always try to analyse importance of Data Structure by considering your problem in mind, for example, if you will be spending a lot of time accessing data values as compared to inserting or deleting them, in that case, we need to give more weight to advantage over disadvantage.

  [1]: https://stackoverflow.com/questions/697918/what-does-o1-access-time-mean

