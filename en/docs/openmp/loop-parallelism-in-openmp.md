---
title: "Loop parallelism in OpenMP"
slug: "loop-parallelism-in-openmp"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Parameters
| Clause | Parameter |
| ------ | ------ |
| `private`| Comma-separated list of private variables |
| `firstprivate` | Like `private`, but initialized to the value of the variable before entering the loop |
| `lastprivate` | Like `private`, but the variable will get the value corresponding to the last iteration of the loop upon exit |
| `reduction` | reduction operator `:` comma-separated list of corresponding reduction variables |
| `schedule` | `static`, `dynamic`, `guided`, `auto` or `runtime` with an optional chunk size after a coma for the 3 former |
| `collapse` | Number of perfectly nested loops to collapse and parallelize together |
| `ordered` | Tells that some parts of the loop will need to be kept in-order (these parts will be specifically identified with some `ordered` clauses inside the loop body) |
| `nowait` | Remove the implicit barrier existing by default at the end of the loop construct | 



The meaning of the `schedule` clause is as follows:

 - `static[,chunk]`: Distribute statically (meaning that the distribution is done before entering the loop) the loop iterations in batched of `chunk` size in a round-robin fashion. If `chunk` isn't specified, then the chunks are as even as possible and each thread gets at most one of them.
 - `dynamic[,chunk]`: Distribute the loop iterations among the threads by batches of `chunk` size with a first-come-first-served policy, until no batch remains. If not specified, `chunk` is set to 1
 - `guided[,chunk]`: Like `dynamic` but with batches which sizes get smaller and smaller, down to 1
 - `auto`: Let the compiler and/or run time library decide what is best suited
 - `runtime`: Deffer the decision at run time by mean of the `OMP_SCHEDULE` environment variable. If at run time the environment variable is not defined, the default scheduling will be used

The default for `schedule` is **implementation define**. On many environments it is `static`, but can also be `dynamic` or could very well be `auto`. Therefore, be careful that your implementation doesn't implicitly rely on it without explicitly setting it.

In the above examples, we used the fused form `parallel for` or `parallel do`. However, the loop construct can be used without fusing it with the `parallel` directive, in the form of a `#pragma omp for [...]` or `!$omp do [...]` standalone directive within a `parallel` region.

For the Fortran version only, the loop index variable(s) of the parallized loop(s) is (are) always `private` by default. There is therefore no need of explicitly declaring them `private` (although doing so isn't a error).<br>
For the C and C++ version, the loop indexes are just like any other variables. Therefore, if their scope extends outside of the parallelized loop(s) (meaning if they are not declared like `for ( int i = ...)` but rather like `int i; ... for ( i = ... )` then they **have to** be declared `private`.

## Typical example in C
<!-- language: lang-c -->

    #include <stdio.h>
    #include <math.h>
    #include <omp.h>

    #define N 1000000

    int main() {
        double sum = 0;

        double tbegin = omp_get_wtime();
        #pragma omp parallel for reduction( +: sum )
        for ( int i = 0; i < N; i++ ) {
            sum += cos( i );
        }
        double wtime = omp_get_wtime() - tbegin;

        printf( "Computing %d cosines and summing them with %d threads took %fs\n",
                N, omp_get_max_threads(), wtime );

        return sum;
    }

In this example, we just compute 1 million cosines and sum their values in parallel. We also time the execution to see whether the parallelization has any effect on the performance. Finally, since we do measure the time, we have to make sure that the compiler won't optimize away the work we've done, so we pretend using the result by just returning it.

## Same example in Fortran
    program typical_loop
        use omp_lib
        implicit none
        integer, parameter :: N = 1000000, kd = kind( 1.d0 )
        real( kind = kd ) :: sum, tbegin, wtime
        integer :: i
    
        sum = 0

        tbegin = omp_get_wtime()
        !$omp parallel do reduction( +: sum )
        do i = 1, N
            sum = sum + cos( 1.d0 * i )
        end do
        !$omp end parallel do
        wtime = omp_get_wtime() - tbegin

        print "( 'Computing ', i7, ' cosines and summing them with ', i2, &
            & ' threads took ', f6.4,'s' )", N, omp_get_max_threads(), wtime

        if ( sum > N ) then
            print *, "we only pretend using sum"
        end if
    end program typical_loop

Here again we compute and accumulate 1 million cosines. We time the loop and to avoid unwanted compiler optimization-away of it, we pretend using the result. 

## Compiling and running the examples
On a 8 cores Linux machine using GCC version 4.4, the C codes can be compiled and run the following way:

    $ gcc -std=c99 -O3 -fopenmp loop.c -o loopc -lm
    $ OMP_NUM_THREADS=1 ./loopc
    Computing 1000000 cosines and summing them with 1 threads took 0.095832s
    $ OMP_NUM_THREADS=2 ./loopc
    Computing 1000000 cosines and summing them with 2 threads took 0.047637s
    $ OMP_NUM_THREADS=4 ./loopc
    Computing 1000000 cosines and summing them with 4 threads took 0.024498s
    $ OMP_NUM_THREADS=8 ./loopc
    Computing 1000000 cosines and summing them with 8 threads took 0.011785s

For the Fortran version, it gives:

    $ gfortran -O3 -fopenmp loop.f90 -o loopf
    $ OMP_NUM_THREADS=1 ./loopf
    Computing 1000000 cosines and summing them with  1 threads took 0.0915s
    $ OMP_NUM_THREADS=2 ./loopf
    Computing 1000000 cosines and summing them with  2 threads took 0.0472s
    $ OMP_NUM_THREADS=4 ./loopf
    Computing 1000000 cosines and summing them with  4 threads took 0.0236s
    $ OMP_NUM_THREADS=8 ./loopf
    Computing 1000000 cosines and summing them with  8 threads took 0.0118s


## Addition of two vectors using OpenMP parallel for construct
    void parallelAddition (unsigned N, const double *A, const double *B, double *C)
    {
        unsigned i;

        #pragma omp parallel for shared (A,B,C,N) private(i) schedule(static)
        for (i = 0; i < N; ++i)
        {
            C[i] = A[i] + B[i];
        }
    }

This example adds two vector (`A` and `B` into `C`) by spawning a team of threads (specified by the `OMP_NUM_THREADS` environtment variable, for instance) and assigning each thread a chunk of work (in this example, assigned statically through the `schedule(static)` expression).

See remarks section with respect to the `private(i)` optionality. 

