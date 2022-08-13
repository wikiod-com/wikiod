---
title: "Making a variable thread safe"
slug: "making-a-variable-thread-safe"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Controlling access to a variable in a Parallel.For loop
    using System;
    using System.Threading;
    using System.Threading.Tasks;
    
    class Program
    {
        static void Main( string[] args )
        {
            object sync = new object();
            int sum = 0;
            Parallel.For( 1, 1000, ( i ) => {
                lock( sync ) sum = sum + i; // lock is necessary
    
                // As a practical matter, ensure this `parallel for` executes
                // on multiple threads by simulating a lengthy operation.
                Thread.Sleep( 1 );
            } );
            Console.WriteLine( "Correct answer should be 499500.  sum is: {0}", sum );
        }
    }

It is not sufficient to just do <code>sum = sum + i</code> without the lock because the read-modify-write operation is not atomic.  A thread will overwrite any external modifications to <code>sum</code> that occur after it has read the current value of <code>sum</code>, but before it stores the modified value of <code>sum + i</code> back into <code>sum</code>.

