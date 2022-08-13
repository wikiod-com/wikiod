---
title: "Getting started with openmp"
slug: "getting-started-with-openmp"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Compilation
There are many compilers that support different versions of the OpenMP specification. OpenMP maintains a list [here][1] with the compiler that support it and the supported version. In general, to compile (and link) an application with OpenMP support you need only to add a compile flag and if you use the OpenMP API you need to include the OpenMP header (omp.h). While the header file has a fixed name, the compile flag depends on the compiler. The following is a non-exhaustive list of compilers and the flag that enables OpenMP.

 - GCC (including gcc, g++ and gfortran) : `-fopenmp`
 - LLVM: -fopenmp
 - Intel compiler-suite (including icc, icpc and ifort) : `-qopenmp` (and `-fopenmp` for compatibility with GCC/LLVM)
 - IBM XL compiler-suite (including xlc, xlC and xlf) : `-xlsmp=omp`
 - PGI compiler-suite (including pgcc pgc++ pgfortran) : '-mp'

  [1]: http://openmp.org/wp/openmp-compilers "Supported OpenMP compilers and their OpenMP specs support"

## Parallel hello world using OpenMP
<!-- language-all: lang-c -->

    #include <omp.h>
    #include <stdio.h>

    int main (int argc, char *argv[])
    {
       #pragma omp parallel
       {
         printf ("Hello world! I'm thread %d out of %d threads.\n",
                 omp_get_thread_num(), omp_get_num_threads());
       }
       return 0;
    }

  This code simply creates a team of threads (according to the environment variable `OMP_NUM_THREADS` - and if not defined will create one per logical core on the system) and each thread will identify itself besides printing the typical Hello world message.

## Work Sharing construct - Example of For loop
    double  res[MAX];  int i;
    #pragma omp parallel 
    {    
        #pragma omp for
        for (i=0;i< MAX; i++) {
            res[i] = huge();
        } 
    }    

The for loop will be executed in parallel. huge() is some method which can take too long to get execute. OpenMP supports a shortcut to write the above code as :

    double  res[MAX];  int i;
    #pragma omp parallel for
    for (i=0;i< MAX; i++) {
        res[i] = huge();
    } 
 
We can also have a schedule clause which effects how loop iterations are mapped to threads. For example:

    #pragma omp parallel
    #pragma omp for schedule(static)
    for(i=0;I<N;i++) {
        a[i] = a[i] + b[i];
    }

Different styles of scheduling are:

**schedule(static [,chunk])**\
Deal-out blocks of iterations of size “chunk” to each thread.\
If not specified:  allocate as evenly as possible to the available threads

**schedule(dynamic[,chunk])**\
Each thread grabs “chunk” iterations off a queue until all iterations have been handled.

**schedule(guided[,chunk])**\
Threads dynamically grab blocks of iterations. The size of the block starts large and shrinks down to size “chunk” as the calculation proceeds.

**schedule(runtime)**\
Schedule and chunk size taken from the OMP_SCHEDULE environment variable.

## Reduction Example
    #include <omp.h>
    void main ()
    {     
        int i;       
        double ZZ, func(), res=0.0;
    
        #pragma omp parallel for reduction(+:res) private(ZZ) 
        for (i=0; i< 1000; i++){
            ZZ = func(I);
            res = res + ZZ; 
        }
    }
    
In the last line: Actually added to a private copy, then combined after the loop.  Compiler takes care of the details.

