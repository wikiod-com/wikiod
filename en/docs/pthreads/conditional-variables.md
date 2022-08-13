---
title: "Conditional Variables"
slug: "conditional-variables"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Conditional variables are useful in cases where you want a thread to wait for something that happens in another thread. For instance, in a producer/consumer scenario with one or or more producing threads and one consuming thread, conditional variables can be used to signal the consuming thread that new data is available. 

**General process**

A wait on a conditional variable (queueCond in the producer/consumer example) is always coupled to a mutex (the queueMutex in the producer/consumer example), and should always be coupled to a "normal" state variable also (queue.empty() in the producer/consumer example). When used correctly, this ensures that no new data is missed in the consumer.

In general, the process should be:
 - For the signalling thread:
   1. Lock the mutex
   2. Update any data and state variables
   3. Signal the condition variable
   4. Unlock the mutex
 - For the waiting thread:
   1. Lock the mutex
   2. Do a `while` loop on the state variable, looping as long as the data is not ready
   3. In the `while` loop, do a wait on the condition variable with `pthread_cond_wait()`
   4. When the `while` loop exits, we are now sure that new data is ready, and that the mutex is locked
   5. Do something with the data
   6. Unlock the mutex and repeat

With this scheme, no matter when the signalling and waiting threads are scheduled, the waiting thread will never miss data (as in, it will never be stuck waiting forever with valid data ready). This can be realized by manually trying to run through the steps for the signalling thread, recording the states of the mutex, condition and state variables, for each of the steps in the waiting thread.


**`pthread_cond_wait` and the mutex**

To facilitate the above process, it is required to call `pthread_cond_wait()` with the mutex locked. When called, `pthread_cond_wait()` will then unlock the mutex before putting the thread to sleep, and, just before returning for whatever reason, the mutex will be relocked. This also means that if some other thread currently has the mutex locked, `pthread_cond_wait()` will wait for the mutex to be unlocked, and until the waiting thread can actually acquire the mutex - it will contend on it together with any other threads trying to lock the mutex at the same time.


**Spurious wakeups**

Also, it may seem as if the `while` loop waiting on the state variable could be substituted for a simple `if` statement. However, the `while` loop is needed, as the Posix standard allows `pthread_cond_wait()` to do so-called "spurious" wakeups during the wait, without actually being signalled. Thus, the code needs to recheck the state variable to see if `pthread_cond_wait()` returned due to actually being signalled, or due to one of these spurious wakeups.

## Producer / consumer example
    pthread_mutex_t queueMutex;
    pthread_cond_t queueCond;
    Queue queue;

    void Initialize() {
        //Initialize the mutex and the condition variable
        pthread_mutex_init(&queueMutex, NULL);
        pthread_cond_init(&queueCond, NULL);
    }

    void Producer() {
        //First we get some new data
        Data *newData = MakeNewData();

        //Lock the queue mutex to make sure that adding data to the queue happens correctly
        pthread_mutex_lock(&queueMutex);

        //Push new data to the queue
        queue.push(newData);

        //Signal the condition variable that new data is available in the queue
        pthread_cond_signal(&queueCond);

        //Done, unlock the mutex
        pthread_mutex_unlock(&queueMutex);
    }


    void Consumer() {

        //Run the consumer loop
        while(1) {

            //Start by locking the queue mutex
            pthread_mutex_lock(&queueMutex);

            //As long as the queue is empty,
            while(queue.empty()) {
                // - wait for the condition variable to be signalled
                //Note: This call unlocks the mutex when called and
                //relocks it before returning!
                pthread_cond_wait(&queueCond, &queueMutex);
            }

            //As we returned from the call, there must be new data in the queue - get it,
            Data *newData = queue.front();
            // - and remove it from the queue
            queue.pop();

            //Now unlock the mutex
            pthread_mutex_unlock(&queueMutex);
            
            // - and process the new data
            ProcessData(newData);
        }
    }

