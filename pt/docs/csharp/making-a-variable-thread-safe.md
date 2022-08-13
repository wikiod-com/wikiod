---
title: "Tornando um thread variável seguro"
slug: "tornando-um-thread-variavel-seguro"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Controlando o acesso a uma variável em um loop Parallel.For
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

Não é suficiente apenas fazer <code>sum = sum + i</code> sem o lock porque a operação read-modify-write não é atômica. Uma thread irá sobrescrever quaisquer modificações externas em <code>sum</code> que ocorram após ele ter lido o valor atual de <code>sum</code>, mas antes de armazenar o valor modificado de <code>sum + i< /code> de volta para <code>soma</code>.

