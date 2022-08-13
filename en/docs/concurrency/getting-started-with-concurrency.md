---
title: "Getting started with concurrency"
slug: "getting-started-with-concurrency"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting concurrency set up or installed.

## Example of concurrent execution in Java
    import java.util.stream.IntStream;

    public class Concurrent {
        public static void printAndWait(String s) {
            System.out.println(s);
            try {
                Thread.sleep(1000);
            } catch (Exception e) {}
        }
        
        public static void main(String[] args) {
            Thread myThread = new Thread() {
                public void run() {
                    IntStream.range(1,32)
                    .forEach(x -> printAndWait(""+x));
                }
             };
             myThread.start();
             IntStream.range('a', 'z').forEach(x -> printAndWait(""+(char)x));
         }
    }

This will produce an output of something similar to

    a
    1
    b
    2
    c
    3

and so on, though results may vary. This is because the code in `myThread` is executed simultaneously, in a different thread, as the main flow. That is, the range 1-32 is handled by one thread, and the range a-z is handled by another.

Since there is no synchronization between the threads, there is no guarantee which one will execute first or indeed even that they will produce a result that is perfectly intertwined. 



