---
title: "Getting started with pthreads"
slug: "getting-started-with-pthreads"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting pthreads set up or installed.

## Minimal "Hello World" with pthreads
<!-- language: lang-c -->

    #include <pthread.h>
    #include <stdio.h>
    #include <string.h>
    
    /* function to be run as a thread always must have the same signature:
       it has one void* parameter and returns void */
    void *threadfunction(void *arg)
    {
      printf("Hello, World!\n"); /*printf() is specified as thread-safe as of C11*/
      return 0;
    }
    
    int main(void)
    {
      pthread_t thread;
      int createerror = pthread_create(&thread, NULL, threadfunction, NULL);
      /*creates a new thread with default attributes and NULL passed as the argument to the start routine*/
      if (!createerror) /*check whether the thread creation was successful*/
        {
          pthread_join(thread, NULL); /*wait until the created thread terminates*/
          return 0;
        }
      fprintf("%s\n", strerror(createerror), stderr);
      return 1;
    }

## Passing arguments to threads
    #include <stdio.h>
    #include <pthread.h>
    
    void *thread_func(void *arg)
    {
        printf("I am thread #%d\n", *(int *)arg);
        return NULL;
    }
    
    int main(int argc, char *argv[])
    {
        pthread_t t1, t2;
        int i = 1;
        int j = 2;
    
        /* Create 2 threads t1 and t2 with default attributes which will execute
        function "thread_func()" in their own contexts with specified arguments. */
        pthread_create(&t1, NULL, &thread_func, &i);
        pthread_create(&t2, NULL, &thread_func, &j);
    
        /* This makes the main thread wait on the death of t1 and t2. */
        pthread_join(t1, NULL);
        pthread_join(t2, NULL);
    
        printf("In main thread\n");
        return 0;
    }

How to compile:

    $ gcc -pthread -o hello hello.c

This prints:

    I am thread #1
    I am thread #2
    In main thread

## Returning result from thread
A pointer to a concrete data type, converted to `void *`, can be used to pass values to and return results from the thread function. 

    #include <stdio.h>
    #include <stdlib.h>
    #include <pthread.h>
    
    struct thread_args
    {
        int a;
        double b;
    };
    

    struct thread_result
    {
        long x;
        double y;
    };

    void *thread_func(void *args_void)
    {
        struct thread_args *args = args_void;
        /* The thread cannot return a pointer to a local variable */
        struct thread_result *res = malloc(sizeof *res);

        res->x  = 10 + args->a;
        res->y = args->a * args->b;
        return res;
    }
    
    int main()
    {
        pthread_t threadL;
        struct thread_args in = { .a = 10, .b = 3.141592653 };
        void *out_void;
        struct thread_result *out;
   
        pthread_create(&threadL, NULL, thread_func, &in);
        pthread_join(threadL, &out_void);
        out = out_void;
        printf("out -> x = %ld\tout -> b = %f\n", out->x, out->y);
        free(out);
    
        return 0;
    }

In many cases it is unnecessary to pass a return value in this way - for example, space in the argument struct can also be used to return results, or a pointer to a shared data structure can be passed to the thread and the results stored there.

