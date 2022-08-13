---
title: "Queue Module"
slug: "queue-module"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

The Queue module implements multi-producer, multi-consumer queues. It is especially useful in threaded programming when information must be exchanged safely between multiple threads.

There are three types of queues provides by queue module,Which are as following :

 1. Queue
 2. LifoQueue
 3. PriorityQueue

Exception which could be come:

 1. Full (queue overflow)
 2. Empty (queue underflow)

## Simple example
    from Queue import Queue
     
    question_queue = Queue()
    
    for x in range(1,10):
        temp_dict = ('key', x)
        question_queue.put(temp_dict)
    
    while(not question_queue.empty()):
        item = question_queue.get()
        print(str(item))

Output:

    ('key', 1)
    ('key', 2)
    ('key', 3)
    ('key', 4)
    ('key', 5)
    ('key', 6)
    ('key', 7)
    ('key', 8)
    ('key', 9)

