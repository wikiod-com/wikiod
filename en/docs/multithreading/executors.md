---
title: "Executors"
slug: "executors"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Syntax
 -  **ThreadPoolExecutor**
 - `ThreadPoolExecutor(int corePoolSize, int maximumPoolSize, long
   keepAliveTime, TimeUnit unit, BlockingQueue<Runnable> workQueue)`
 - `ThreadPoolExecutor(int corePoolSize, int maximumPoolSize, long
   keepAliveTime, TimeUnit unit, BlockingQueue<Runnable> workQueue,
   RejectedExecutionHandler handler)`

 - `ThreadPoolExecutor(int corePoolSize, int maximumPoolSize, long
   keepAliveTime, TimeUnit unit, BlockingQueue<Runnable> workQueue,
   ThreadFactory threadFactory)`

 - `ThreadPoolExecutor(int corePoolSize, int maximumPoolSize, long keepAliveTime, TimeUnit unit, BlockingQueue<Runnable> workQueue, ThreadFactory threadFactory, RejectedExecutionHandler handler)`

 -    `Executors.callable(PrivilegedAction<?> action)`
 -    `Executors.callable(PrivilegedExceptionAction<?> action)`
 -    `Executors.callable(Runnable task)`
 -    `Executors.callable(Runnable task, T result)`
 -    `Executors.defaultThreadFactory()`
 -    `Executors.newCachedThreadPool()`
 -    `Executors.newCachedThreadPool(ThreadFactory threadFactory)`
 -    `Executors.newFixedThreadPool(int nThreads)`
 -    `Executors.newFixedThreadPool(int nThreads, ThreadFactory threadFactory)`
 -    `Executors.newScheduledThreadPool(int corePoolSize)`
 -    `Executors.newScheduledThreadPool(int corePoolSize, ThreadFactory threadFactory)`
 -    `Executors.newSingleThreadExecutor()`
 -    `Executors.newSingleThreadExecutor(ThreadFactory threadFactory)`
 

## Parameters
| Parameter | Detail |
| ------ | ------ |
| corePoolSize   | Minimum number of threads to keep in the pool.  |
| maximumPoolSize   |  Maximum number of threads to allow in the pool. |
| keepAliveTime   |  When number of threads is greater than the core, the noncore threads (excess idle threads) will wait for time defined by this parameter for new tasks before terminating. |
| unit   | Time unit for `keepAliveTime`.  |
| timeout |  the maximum time to wait
| workQueue   |  The type of queue that our Executor is going to use |
| threadFactory | The factory to use when creating new threads |
| nThreads| The number of threads in the pool|
| executor | The underlying implementation | 
| task | the task to run |
| result | The result to return |
| action | The privileged action to run |
| callable |The underlying task |


The different types of Threadpools and Queues that are explained below, have been taken from the information and knowledge from [oracle documentation][1] and [Jakob Jenkov][2] blog where you can learn a lot about concurrency in java.

Different types of ThreadPools
------------------------------
**SingleThreadExecutor:** Executor that uses a single worker thread operating off an unbounded queue, and uses the provided ThreadFactory to create a new thread when needed. Unlike the otherwise equivalent newFixedThreadPool(1, threadFactory) the returned executor is guaranteed not to be reconfigurable to use additional threads.

**FixedThreadPool:**  thread pool that reuses a fixed number of threads operating off a shared unbounded queue, using the provided ThreadFactory to create new threads when needed. At any point, at most nThreads threads will be active processing tasks. If additional tasks are submitted when all threads are active, they will wait in the queue until a thread is available. If any thread terminates due to a failure during execution prior to shutdown, a new one will take its place if needed to execute subsequent tasks. The threads in the pool will exist until it is explicitly shutdown.


**CachedThreadPool:** Thread pool that creates new threads as needed, but will reuse previously constructed threads when they are available, and uses the provided ThreadFactory to create new threads when needed.

**SingleThreadScheduledExecutor:** Single-threaded executor that can schedule commands to run after a given delay, or to execute periodically. (Note however that if this single thread terminates due to a failure during execution prior to shutdown, a new one will take its place if needed to execute subsequent tasks.) Tasks are guaranteed to execute sequentially, and no more than one task will be active at any given time. Unlike the otherwise equivalent newScheduledThreadPool(1, threadFactory) the returned executor is guaranteed not to be reconfigurable to use additional threads.


**ScheduledThreadPool:** Thread pool that can schedule commands to run after a given delay, or to execute periodically.
Different types of Work Queues

## Custom Runnables instead of Callables
Another good practice to check when our threads have finished without block the thread waiting to recover the Future object from our Callable is to create our own implemetation for Runnables, using it together with the `execute()` method.

In the next example, I show a custom class which implements Runnable with a internal callback, with allow us to know when the runnables are finished and use it later in our ThreadPool:

   
<!-- language: java -->
    public class CallbackTask implements Runnable {
        private final Runnable mTask;
        private final RunnableCallback mCallback;
    
        public CallbackTask(Runnable task, RunnableCallback runnableCallback) {
            this.mTask = task;
            this.mCallback = runnableCallback;
        }
    
        public void run() {
            long startRunnable = System.currentTimeMillis();
            mTask.run();
            mCallback.onRunnableComplete(startRunnable);
        }
    
        public interface RunnableCallback {
            void onRunnableComplete(long runnableStartTime);
        }
    }

And here is our ThreadExecutor Implementation:

<!-- language: java -->
    public class ThreadExecutorExample implements ThreadExecutor {
    
        private static String TAG = "ThreadExecutorExample";
        public static final int THREADPOOL_SIZE = 4;
        private long mSubmittedTasks;
        private long mCompletedTasks;
        private long mNotCompletedTasks;
    
        private ThreadPoolExecutor mThreadPoolExecutor;
    
        public ThreadExecutorExample() {
            Log.i(TAG, "[ThreadExecutorImpl] Initializing ThreadExecutorImpl");
            Log.i(TAG, "[ThreadExecutorImpl] current cores: " + Runtime.getRuntime().availableProcessors());
            this.mThreadPoolExecutor =
                (ThreadPoolExecutor) Executors.newFixedThreadPool(THREADPOOL_SIZE);
    
        }
    
        @Override
        public void execute(Runnable runnable) {
            try {
                if (runnable == null) {
                    Log.e(TAG, "[execute] Runnable to execute cannot be null");
                    return;
                }
                Log.i(TAG, "[execute] Executing new Thread");
    
                this.mThreadPoolExecutor.execute(new CallbackTask(runnable, new CallbackTask.RunnableCallback() {
    
                    @Override
                    public void onRunnableComplete(long RunnableStartTime) {
                        mSubmittedTasks = mThreadPoolExecutor.getTaskCount();
                        mCompletedTasks = mThreadPoolExecutor.getCompletedTaskCount();
                        mNotCompletedTasks = mSubmittedTasks - mCompletedTasks; // approximate
    
                        Log.i(TAG, "[execute] [onRunnableComplete] Runnable complete in " + (System.currentTimeMillis() - RunnableStartTime) + "ms");
                        Log.i(TAG, "[execute] [onRunnableComplete] Current threads working " + mNotCompletedTasks);
                    }
                }));
            }
            catch (Exception e) {
                e.printStackTrace();
                Log.e(TAG, "[execute] Error, shutDown the Executor");
                this.mThreadPoolExecutor.shutdown();
            }
        }
    }
  

     /**
     * Executor thread abstraction created to change the execution context from any thread from out ThreadExecutor.
     */
    interface ThreadExecutor  extends Executor {
    
        void execute(Runnable runnable);
    
    }

  
I did this example to check speed of my threads in milliseconds when they are executed, without use Future. You can take this example and add it to your app to control the concurrent task working, and the completed/finished ones. Checking in all moment, the time that you needed to execute that threads.



 

## Defining a new ThreadPool
A `ThreadPool` is an `ExecutorService` that executes each submitted task using one of possibly several pooled threads, normally configured using Executors factory methods.

Here is a basic code to initialize a new ThreadPool as a singleton to use in your app:

   
<!-- language: java -->
    public final class ThreadPool {

        private static final String TAG = "ThreadPool";
        private static final int CORE_POOL_SIZE = 4;
        private static final int MAX_POOL_SIZE = 8;
        private static final int KEEP_ALIVE_TIME = 10; // 10 seconds
        private final Executor mExecutor;

        private static ThreadPool sThreadPoolInstance;

        private ThreadPool() {
            mExecutor = new ThreadPoolExecutor(
                CORE_POOL_SIZE, MAX_POOL_SIZE, KEEP_ALIVE_TIME,
                TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>());
        }

        public void execute(Runnable runnable) {
            mExecutor.execute(runnable);
        }

        public synchronized static ThreadPool getThreadPoolInstance() {
            if (sThreadPoolInstance == null) {
                Log.i(TAG, "[getThreadManagerInstance] New Instance");
                sThreadPoolInstance = new ThreadPool();
            }
            return sThreadPoolInstance;
        }
    }

You have two ways to call your runnable method, use `execute()` or `submit()`. the difference between them is that `submit()` returns a `Future` object which allows  you a way to programatically cancel the running thread when the object T is returned from the `Callable` callback. You can read more about `Future` [here][1]


  [1]: http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/Future.html

## Future and Callables
One of the features that we can use with Threadpools is the `submit()` method which allow us to know when the thread as finish his work. We can do this thanks to the `Future` object, which return us an object from the Callable that we can use to our own objetives.

Here is an example about how to use the Callable instance:

    public class CallablesExample{
    
    //Create MyCustomCallable instance
    List<Future<String>> mFutureList = new ArrayList<Future<String>>();
    
    //Create a list to save the Futures from the Callable
    Callable<String> mCallable = new MyCustomCallable();
    
    public void main(String args[]){
        //Get ExecutorService from Executors utility class, Creating a 5 threads pool.
        ExecutorService executor = Executors.newFixedThreadPool(5);
       
        
       
        for (int i = 0; i < 100; i++) {
            //submit Callable tasks to be executed by thread pool
            Future<String> future = executor.submit(mCallable);
            //add Future to the list, we can get return value using Future
            mFutureList.add(future);
        }
        for (Future<String> fut : mFutureList) {
            try {
                //Print the return value of Future, Notice the output delay in console
                //because Future.get() stop the thread till the task have been completed
                System.out.println(new Date() + "::" + fut.get());
            } catch (InterruptedException | ExecutionException e) {
                e.printStackTrace();
            }
        }
        //Shut down the service
        executor.shutdown();
    }

     class MyCustomCallable implements Callable<String> {

        @Override
        public String call() throws Exception {
            Thread.sleep(1000);
            //return the thread name executing this callable task
            return Thread.currentThread().getName();
        }
    }
    }

As you can see, we create a Threadpool with 5 Threads, this means that we can throw 5 callables parallel. When the threads finish, we will get and Future object from the callable, in this case the name of the thread. 

**WARNING**

In this example, we just use the Futures as a object inside the array to know how many threads we are executing and print that many times a log in console with the data that we want. But, if we want to use the method `Future.get()`, to return us the data that we saved before in the callable, we will block the thread till the task is completed. Be care with this kind of calls when you want perform this as fast as possible

## Adding ThreadFactory to Executor
We use ExecutorService to assign threads from the internal thread pool or create them on-demand to perform tasks. Each ExecutorService has an ThreadFactory, but The ExecutorService will use always a default one if we don't set a custom one.
Why we should do this?

 - To set a more descriptive thread name. Default ThreadFactory gives thread names in the form of pool-m-thread-n, such as pool-1-thread-1, pool-2-thread-1, pool-3-thread-1, etc. If you are trying to debug or monitoring something, it's hard to know what are that threads doing


 - Set a custom Daemon status, the default ThreadFactory produces non-daemon results.

 - Set priority to our threads, the default ThreadFactory set a medium priority to all their threads.
 - You can specify `UncaughtExceptionHandler` for our thread using `setUncaughtExceptionHandler()` on thread object. This gets called back when Thread's run method throws uncaught exception.

Here is a easy implementation of a ThreadFactory over a ThreadPool.


    public class ThreadExecutorExample implements ThreadExecutor {
    private static String TAG = "ThreadExecutorExample";
    private static final int INITIAL_POOL_SIZE = 3;
    private static final int MAX_POOL_SIZE = 5;

    // Sets the amount of time an idle thread waits before terminating
    private static final int KEEP_ALIVE_TIME = 10;

    // Sets the Time Unit to seconds
    private static final TimeUnit KEEP_ALIVE_TIME_UNIT = TimeUnit.SECONDS;

    private final BlockingQueue<Runnable> workQueue;

    private final ThreadPoolExecutor threadPoolExecutor;

    private final ThreadFactory threadFactory;
    private ThreadPoolExecutor mThreadPoolExecutor;

    public ThreadExecutorExample() {
        this.workQueue = new LinkedBlockingQueue<>();
        this.threadFactory = new CustomThreadFactory();
        this.threadPoolExecutor = new ThreadPoolExecutor(INITIAL_POOL_SIZE, MAX_POOL_SIZE,
                KEEP_ALIVE_TIME, KEEP_ALIVE_TIME_UNIT, this.workQueue, this.threadFactory);
    }

    public void execute(Runnable runnable) {
        if (runnable == null) {
            return;
        }
        this.threadPoolExecutor.execute(runnable);
    }

    private static class CustomThreadFactory implements ThreadFactory {
        private static final String THREAD_NAME = "thread_";
        private int counter = 0;

        @Override public Thread newThread(Runnable runnable) {
            return new Thread(runnable, THREAD_NAME + counter++);
        }
    }
    }
    
    /**
     * Executor thread abstraction created to change the execution context from any thread from out ThreadExecutor.
     */
    interface ThreadExecutor extends Executor {
        
        void execute(Runnable runnable);
        
    }

This example just modify the name of the Thread with a counter, but we can modify it as long as we want.

