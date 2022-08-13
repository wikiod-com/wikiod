---
title: "Dequeue Interface"
slug: "dequeue-interface"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

A Deque is linear collection that supports element insertion and removal at both ends. 

The name deque is short for "double ended queue" and is usually pronounced "deck". 

Most Deque implementations place no fixed limits on the number of elements they may contain, but this interface supports capacity-restricted deques as well as those with no fixed size limit.

The Deque interface is a richer abstract data type than both Stack and Queue because it implements both stacks and queues at same time

Generics can be used with Deque.

    Deque<Object> deque = new LinkedList<Object>();

When a deque is used as a queue, FIFO (First-In-First-Out) behavior results. 

Deques can also be used as LIFO (Last-In-First-Out) stacks.

For more information about methods, go through [this][1] Documentation.


  [1]: https://docs.oracle.com/javase/7/docs/api/java/util/Deque.html

## Adding Elements to Deque
    Deque deque = new LinkedList();
    
    //Adding element at tail
    deque.add("Item1");
    
    //Adding element at head 
    deque.addFirst("Item2");
    
    //Adding element at tail 
    deque.addLast("Item3");

## Removing Elements from Deque
    //Retrieves and removes the head of the queue represented by this deque
    Object headItem = deque.remove();
    
    //Retrieves and removes the first element of this deque.
    Object firstItem = deque.removeFirst();
    
    //Retrieves and removes the last element of this deque.
    Object lastItem = deque.removeLast();

## Retrieving Element without Removing
    //Retrieves, but does not remove, the head of the queue represented by this deque
    Object headItem = deque.element();
    
    //Retrieves, but does not remove, the first element of this deque.
    Object firstItem = deque.getFirst();

    //Retrieves, but does not remove, the last element of this deque.    
    Object lastItem  = deque.getLast();

## Iterating through Deque
    //Using Iterator
    Iterator iterator = deque.iterator();
    while(iterator.hasNext(){
      String Item = (String) iterator.next();
    }
   
    //Using For Loop
    for(Object object : deque) {
        String Item = (String) object;
    }

