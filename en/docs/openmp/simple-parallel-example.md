---
title: "Simple parallel example"
slug: "simple-parallel-example"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
 - `#pragma omp parallel` indicates that the following block shall be executed by all the threads.

 - `int omp_get_num_threads (void)` : returns the number of the threads working on the parallel region (aka team of threads).

 - `int omp_get_thread_num (void)` : returns the identifier of the calling thread (ranges from 0 to N-1 where N is bounded to `omp_get_num_threads()`).


You can use the `OMP_NUM_THREADS` environment variable or the `num_threads` directive within the `#pragma parallel` to indicate the number of executing threads for the whole application or for the specified region, respectively.

## Parallel hello world using OpenMP
The following C code uses the OpenMP parallel programming model to write the thread ID and number of threads to `stdout` using multiple threads.

    #include <omp.h>
    #include <stdio.h>

    int main ()
    {
        #pragma omp parallel
        {   
            // ID of the thread in the current team
            int thread_id = omp_get_thread_num();
            // Number of threads in the current team
            int nthreads = omp_get_num_threads();

            printf("I'm thread %d out of %d threads.\n", thread_id, nthreads);
        }
        return 0;
    }

In Fortran 90+ the equivalent program looks like:

    program Hello
      use omp_lib, only: omp_get_thread_num, omp_get_num_threads

      implicit none

      integer :: thread_id
      integer :: nthreads

      !$omp parallel private( thread_id, nthreads )

      ! ID of the thread in the current team
      thread_id = omp_get_thread_num()
      ! Number of threads in the current team
      nthreads = omp_get_num_threads()

      print *, "I'm thread", thread_id, "out of", nthreads, "threads."
      !$omp end parallel
    end program Hello

