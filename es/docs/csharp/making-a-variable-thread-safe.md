---
title: "Hacer que un subproceso variable sea seguro"
slug: "hacer-que-un-subproceso-variable-sea-seguro"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Controlar el acceso a una variable en un bucle Parallel.For
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

No es suficiente hacer <code>sum = sum + i</code> sin el bloqueo porque la operación de lectura-modificación-escritura no es atómica. Un subproceso sobrescribirá cualquier modificación externa a <code>sum</code> que ocurra después de haber leído el valor actual de <code>sum</code>, pero antes de almacenar el valor modificado de <code>sum + i< /code> de nuevo en <code>sum</code>.

