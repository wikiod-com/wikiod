---
title: "Değişken bir ipliği güvenli hale getirme"
slug: "degisken-bir-ipligi-guvenli-hale-getirme"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Parallel.For döngüsündeki bir değişkene erişimi kontrol etme
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

Okuma-değiştirme-yazma işlemi atomik olmadığı için kilit olmadan sadece <code>sum = sum + i</code> yapmak yeterli değildir. Bir iş parçacığı, <code>sum</code>'un geçerli değerini okuduktan sonra, ancak değiştirilmiş <code>sum + i< değerini saklamadan önce, <code>sum</code>'da yapılan tüm harici değişikliklerin üzerine yazar. /code> <code>sum</code>'a geri dönün.

