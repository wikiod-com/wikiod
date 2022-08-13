---
title: "Race condition in pthreads"
slug: "race-condition-in-pthreads"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

When writing multi-threaded applications, one of the most common problems experienced are race conditions. So we document the How do you detect them? and How do you handle them?

## Example: Consider will have two threads T1 and T2.
> How do you detect them?

If the same _variable/resource/memory location_ is accessible by multiple threads and at least of the thread is changing the value of _variable/resource/memory location_, then **Race Condition** can occurred. Because if a thread is changing the value of _variable/resource/memory location_ and another thread tries to read the same then it will not get the updated value.

**Note**: If all threads are just reading the _variable/resource/memory location_ then **Race Condition** will not occur.

**Example: Program suffers from Race Condition**  
    
    #include <stdio.h>
    #include <pthread.h>
    
    int x= 0;
    
    void* fun(void* in)
    {
        int i;
        for ( i = 0; i < 10000000; i++ )
        {
            x++;
        }
    }
    
    int main()
    {
        pthread_t t1, t2;
        printf("Point 1 >> X is: %d\n", x);
    
        pthread_create(&t1, NULL, fun, NULL);
        pthread_create(&t2, NULL, fun, NULL);
        pthread_join(t1, NULL);
        pthread_join(t2, NULL);
    
        printf("Point 2 >> X is: %d\n", x);
        return 0;
    }

The output on my screen is:  

    Point 1 >> X is: 0
    Point 2 >> X is: 9925047

Your output will vary. But for sure it will not be 20,000,000. Since both Thread executing the same loop and having global variable `int x;` 

    for ( i = 0; i < 10000000; i++ )
    {
       x++; 
    }

So final value of `x` in line `Point 2 >> X is: 9925047` should be 20,000,000. But it is not so.

The state of x can be changed by another thread during the time between x is being read and when it is written back.

Let's say a thread retrieves the value of x, but hasn't stored it yet. Another thread can also retrieve the same value of x (because no thread has changed it yet) and then they would both be storing the same value (x+1) back in x!

Example:

Thread 1: reads x, value is 7

Thread 1: add 1 to x, value is now 8

Thread 2: reads x, value is 7

Thread 1: stores 8 in x

Thread 2: adds 1 to x, value is now 8

Thread 2: stores 8 in x

> How do you handle them?

Race conditions can be avoided by employing some sort of locking mechanism before the code that accesses the shared resource or mutual exclusion.

Following is modified program:

**Example: Race Condition problem resolved** 

    #include <stdio.h>
    #include <pthread.h>
    
    int x= 0;
    //Create mutex
    pthread_mutex_t test_mutex;
    
    void* fun(void* in)
    {
        int i;
        for ( i = 0; i < 10000000; i++ )
        {
            //Lock mutex before going to change variable
            pthread_mutex_lock(&test_mutex);
            x++;
            //Unlock mutex after changing the variable
            pthread_mutex_unlock(&test_mutex);
        }
    }
    
    int main()
    {
        pthread_t t1, t2;
        printf("Point 1 >> X is: %d\n", x);
    
        //Initlize mutex
        pthread_mutex_init(&test_mutex, NULL);
    
        pthread_create(&t1, NULL, fun, NULL);
        pthread_create(&t2, NULL, fun, NULL);
        pthread_join(t1, NULL);
        pthread_join(t2, NULL);
    
        //Destroy mutex after use
        pthread_mutex_destroy(&test_mutex);
        printf("Point 2 >> X is: %d\n", x);
        return 0;
    }

Following is the output:

    Point 1 >> X is: 0
    Point 2 >> X is: 20000000

Here, the answer comes out as 20,000,000 every time.

**Note**: Modified program, which is free from race condition error, will take much longe to execute. Because there is overburden on `mutex` lock and unlock.



