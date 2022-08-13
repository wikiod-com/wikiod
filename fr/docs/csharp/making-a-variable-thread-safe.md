---
title: "Sécuriser un thread variable"
slug: "securiser-un-thread-variable"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Contrôler l'accès à une variable dans une boucle Parallel.For
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

Il ne suffit pas de faire <code>sum = sum + i</code> sans le verrou car l'opération de lecture-modification-écriture n'est pas atomique. Un thread écrasera toutes les modifications externes à <code>sum</code> qui se produisent après avoir lu la valeur actuelle de <code>sum</code>, mais avant de stocker la valeur modifiée de <code>sum + i< /code> dans <code>sum</code>.

