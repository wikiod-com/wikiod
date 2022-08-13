---
title: "Threads"
slug: "threads"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Simple Mutex Usage
POSIX thread library provides implementation of the mutex primitive, used for the mutual exclusion. Mutex is created using [`pthread_mutex_init`][1], and destroyed using [`pthread_mutex_destroy`][1]. Obtaining a mutex can be done using [`pthread_mutex_lock`][2] or [`pthread_mutex_trylock`][2], (depending if the timeout is desired) and releasing a mutex is done via [`pthread_mutex_unlock`][2].

A simple example using a mutex to serialize access to critical section follows. First, the example without using a mutex. Note that this program has [*data race*][3] due to unsynchronized access to `global_resource` by the two threads. As a result, this program has [undefined behaviour][4]:

<!-- language: lang-c -->
    #include <pthread.h>
    #include <unistd.h>
    #include <stdio.h>
    #include <errno.h>
    #include <stdlib.h>

    // Global resource accessible to all threads
    int global_resource;

    // Threading routine which increments the resource 10 times and prints
    // it after every increment
    void* thread_inc (void* arg)
    {
        for (int i = 0; i < 10; i++)
        {
            global_resource++;
            printf("Increment: %d\n", global_resource);
            // Make this thread slower, so the other one
            // can do more work
            sleep(1);
        }

        printf("Thread inc finished.\n");

        return NULL;
    }

    // Threading routine which decrements the resource 10 times and prints
    // it after every decrement
    void* thread_dec (void* arg)
    {
        for (int i = 0; i < 10; i++)
        {
            global_resource--;
            printf("Decrement: %d\n", global_resource);
        }

        printf("Thread dec finished.\n");

        return NULL;
    }

    int main (int argc, char** argv)
    {
        pthread_t threads[2];

        if (0 != (errno = pthread_create(&threads[0], NULL, thread_inc, NULL)))
        {
            perror("pthread_create() failed");
            return EXIT_FAILURE;
        }

        if (0 != (errno = pthread_create(&threads[1], NULL, thread_dec, NULL)))
        {
            perror("pthread_create() failed");
            return EXIT_FAILURE;
        }

        // Wait for threads to finish
        for (int i = 0; i < 2; i++)
        {
            if (0 != (errno = pthread_join(threads[i], NULL))) {
                perror("pthread_join() failed");
                return EXIT_FAILURE;
            }
        }

        return EXIT_SUCCESS;
    }

A possible output is:

```
Increment: 1
Decrement: 0
Decrement: -1
Decrement: -2
Decrement: -3
Decrement: -4
Decrement: -5
Decrement: -6
Decrement: -7
Decrement: -8
Decrement: -9
Thread dec finished.
Increment: -8
Increment: -7
Increment: -6
Increment: -5
Increment: -4
Increment: -3
Increment: -2
Increment: -1
Increment: 0
Thread inc finished.
```

Now, if we want to synchronise these threads so that we want first to increment or decrement all the way up or down, and then do it in the different way, we need to use a synchronization primitive, such as mutex:

<!-- language: lang-c -->
    #include <pthread.h>
    #include <unistd.h>
    #include <stdio.h>
    #include <errno.h>
    #include <stdlib.h>

    // Global resource accessible to all threads
    int global_resource;
    // Mutex protecting the resource
    pthread_mutex_t mutex;

    // Threading routine which increments the resource 10 times and prints
    // it after every increment
    void* thread_inc (void* arg)
    {
        // Pointer to mutex is passed as an argument
        pthread_mutex_t* mutex = arg;

        // Execute the following code without interrupts, all the way to the
        // point B
        if (0 != (errno = pthread_mutex_lock(mutex)))
        {
            perror("pthread_mutex_lock failed");
            exit(EXIT_FAILURE);
        }

        for (int i = 0; i < 10; i++)
        {
            global_resource++;
            printf("Increment: %d\n", global_resource);
            // Make this thread slower, so the other one
            // can do more work
            sleep(1);
        }

        printf("Thread inc finished.\n");

        // Point B:
        if (0 != (errno = pthread_mutex_unlock(mutex)))
        {
            perror("pthread_mutex_unlock failed");
            exit(EXIT_FAILURE);
        }

        return NULL;
    }

    // Threading routine which decrements the resource 10 times and prints
    // it after every decrement
    void* thread_dec (void* arg)
    {
        // Pointer to mutex is passed as an argument
        pthread_mutex_t* mutex = arg;

        if (0 != (errno = pthread_mutex_lock(mutex)))
        {
            perror("pthread_mutex_lock failed");
            exit(EXIT_FAILURE);
        }

        for (int i = 0; i < 10; i++)
        {
            global_resource--;
            printf("Decrement: %d\n", global_resource);
        }

        printf("Thread dec finished.\n");

        // Point B:
        if (0 != (errno = pthread_mutex_unlock(mutex)))
        {
            perror("pthread_mutex_unlock failed");
            exit(EXIT_FAILURE);
        }

        return NULL;
    }

    int main (int argc, char** argv)
    {
        pthread_t threads[2];
        pthread_mutex_t mutex;

        // Create a mutex with the default parameters
        if (0 != (errno = pthread_mutex_init(&mutex, NULL)))
        {
            perror("pthread_mutex_init() failed");
            return EXIT_FAILURE;
        }

        if (0 != (errno = pthread_create(&threads[0], NULL, thread_inc, &mutex)))
        {
            perror("pthread_create() failed");
            return EXIT_FAILURE;
        }

        if (0 != (errno = pthread_create(&threads[1], NULL, thread_dec, &mutex)))
        {
            perror("pthread_create() failed");
            return EXIT_FAILURE;
        }

        // Wait for threads to finish
        for (int i = 0; i < 2; i++)
        {
            if (0 != (errno = pthread_join(threads[i], NULL))) {
                perror("pthread_join() failed");
                return EXIT_FAILURE;
            }
        }

        // Both threads are guaranteed to be finished here, so we can safely
        // destroy the mutex
        if (0 != (errno = pthread_mutex_destroy(&mutex)))
        {
            perror("pthread_mutex_destroy() failed");
            return EXIT_FAILURE;
        }

        return EXIT_SUCCESS;
    }

One of the possible outputs is

```
Increment: 1
Increment: 2
Increment: 3
Increment: 4
Increment: 5
Increment: 6
Increment: 7
Increment: 8
Increment: 9
Increment: 10
Thread inc finished.
Decrement: 9
Decrement: 8
Decrement: 7
Decrement: 6
Decrement: 5
Decrement: 4
Decrement: 3
Decrement: 2
Decrement: 1
Decrement: 0
Thread dec finished.
```

The other possible output would be inverse, in case that `thread_dec` obtained the mutex first.


  [1]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/pthread_mutex_destroy.html
  [2]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/pthread_mutex_lock.html
  [3]: https://en.wikipedia.org/wiki/Race_condition#Software
  [4]: https://en.wikipedia.org/wiki/Undefined_behavior

## Simple Thread without Arguments
This basic example counts at different rates on two threads that we name sync (main) and async (new thread). The main thread counts to 15 at 1Hz (1s) while the second counts to 10 at 0.5Hz (2s). Because the main thread finishes earlier, we use [`pthread_join`][1] to make it wait async to finish.

<!-- language: lang-c -->
    #include <pthread.h>
    #include <unistd.h>
    #include <errno.h>
    #include <stdlib.h>
    #include <stdio.h>

    /* This is the function that will run in the new thread. */
    void * async_counter(void * pv_unused) {
        int j = 0;
        while (j < 10) {
            printf("async_counter: %d\n", j);
            sleep(2);
            j++;
        }

        return NULL;
    }

    int main(void) {
        pthread_t async_counter_t;
        int i;
        /* Create the new thread with the default flags and without passing
         * any data to the function. */
        if (0 != (errno = pthread_create(&async_counter_t, NULL, async_counter, NULL))) {
            perror("pthread_create() failed");
            return EXIT_FAILURE;
        }

        i = 0;
        while (i < 15) {
            printf("sync_counter: %d\n", i);
            sleep(1);
            i++;
        }

        printf("Waiting for async counter to finish ...\n");

        if (0 != (errno = pthread_join(async_counter_t, NULL))) {
            perror("pthread_join() failed");
            return EXIT_FAILURE;
        }

        return EXIT_SUCCESS;
    }

<sup>*Copied from here: https://www.wikiod.com/c where it had initially been created by [*M. Rubio-Roy*][2].*</sup>


  [1]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/pthread_join.html
  [2]: http://stackoverflow.com/users/3328215/m-rubio-roy*

