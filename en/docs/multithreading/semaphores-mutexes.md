---
title: "Semaphores & Mutexes"
slug: "semaphores--mutexes"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Semaphores & Mutexes are concurrency controls used to synchronize multiple thread access to shared resources.

# Semaphore #

Here's a brilliant explanation from [this Stackoverflow question](https://stackoverflow.com/questions/34519/what-is-a-semaphore):

> Think of semaphores as bouncers at a nightclub. There are a dedicated
> number of people that are allowed in the club at once. If the club is
> full no one is allowed to enter, but as soon as one person leaves
> another person might enter.
> 
> It's simply a way to limit the number of
> consumers for a specific resource. For example, to limit the number of
> simultaneous calls to a database in an application.

# Mutex #
A mutex is a semaphore of 1 (i.e. only one thread at a time).  Using the nightclub metaphor, think of a mutex in terms of a bathroom stall in the nightclub.  Only one occupant allowed at a time.

## Mutex in Java & C++
Although Java doesn't have a Mutex class, you can mimic a Mutex with the use of a Semaphore of 1.  The following example executes two threads with and without locking.  Without locking, the program spits out a somewhat random order of output characters ($ or #).  With locking, the program spits out nice, orderly character sets of either ##### or $$$$$, but never a mix of # & $.

    import java.util.concurrent.Semaphore;
    import java.util.concurrent.ThreadLocalRandom;
    
    public class MutexTest {
       static Semaphore semaphore = new Semaphore(1);
    
       static class MyThread extends Thread {
          boolean lock;
          char c = ' ';
    
          MyThread(boolean lock, char c) {
             this.lock = lock;
             this.c = c;
          }
    
          public void run() {
             try {
                // Generate a random number between 0 & 50
                // The random nbr is used to simulate the "unplanned"
                // execution of the concurrent code
                int randomNbr = ThreadLocalRandom.current().nextInt(0, 50 + 1);
    
                for (int j=0; j<10; ++j) {
                   if(lock) semaphore.acquire();
                   try {
                      for (int i=0; i<5; ++i) {
                         System.out.print(c);
                         Thread.sleep(randomNbr);
                      }
                   } finally {
                      if(lock) semaphore.release();
                   }
                   System.out.print('|');
                }
             } catch (InterruptedException e) {
                e.printStackTrace();
             }
          }
       }
    
       public static void main(String[] args) throws Exception {
          System.out.println("Without Locking:");
          MyThread th1 = new MyThread(false, '$');
          th1.start();
          MyThread th2 = new MyThread(false, '#');
          th2.start();
          
          th1.join();
          th2.join();
    
          System.out.println('\n');
    
          System.out.println("With Locking:");
          MyThread th3 = new MyThread(true, '$');
          th3.start();
          MyThread th4 = new MyThread(true, '#');
          th4.start();
          
          th3.join();
          th4.join();
    
          System.out.println('\n');
       }
    }

Run <code>javac MutexTest.java; java MutexTest</code>, and you will get something like this:

> Without Locking:
> #$$$$$|$$$$$|$$#$$$|$$$$$|$$$$#$|$$$$$|$$$$$|$#$$$$|$$$$$|$$$#$$||#####|#####|#####|#####|#####|#####|#####|#####|#####|
> 
> With Locking:
> $$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|

Here's the same example in C++:

    #include <iostream>       // std::cout
    #include <thread>         // std::thread
    #include <mutex>          // std::mutex
    #include <random>         // std::random_device
    
    class MutextTest {
       private:
          static std::mutex mtx;  // mutex for critical section
    
       public:
          static void run(bool lock, char c) {
             // Generate a random number between 0 & 50
             // The random nbr is used to simulate the "unplanned"
             // execution of the concurrent code
             std::uniform_int_distribution<int> dist(0, 50);
             std::random_device rd;
             int randomNbr = dist(rd);
             //std::cout << randomNbr << '\n';
    
            for(int j=0; j<10; ++j) {
               if(lock) mtx.lock();
               for (int i=0; i<5; ++i) {
                  std::cout << c << std::flush;
                  std::this_thread::sleep_for(std::chrono::milliseconds(randomNbr));
               }
               std::cout << '|';
               if(lock) mtx.unlock();
            }
          }
    };
    
    std::mutex MutextTest::mtx;
    
    int main()
    {
      std::cout << "Without Locking:\n";
      std::thread th1 (MutextTest::run, false, '$');
      std::thread th2 (MutextTest::run, false, '#');
    
      th1.join();
      th2.join();
    
      std::cout << "\n\n";
    
      std::cout << "With Locking:\n";
      std::thread th3 (MutextTest::run, true, '$');
      std::thread th4 (MutextTest::run, true, '#');
    
      th3.join();
      th4.join();
    
      std::cout << '\n';
    
      return 0;
    }

Run <code>g++ --std=c++11 MutexTest.cpp; ./a.out</code>, and you will get something like this:

> Without Locking:
> $#$#$#$#$#|$|#$#$#$#$#|$$|#$#$#$#|$#$|#$#$#$#|$$#$|#$#$#|$#$#$|#$#$#|$#$$#$|#$#|$#$#$#$|#$#|$#$#$#$$|#|$#$#$#$#$|#|####|
> 
> With Locking:
> $$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|$$$$$|#####|

