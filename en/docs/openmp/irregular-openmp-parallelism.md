---
title: "Irregular OpenMP parallelism"
slug: "irregular-openmp-parallelism"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

A common pitfall is to believe that all threads of a parallel region should instantiate (create) tasks but this is not typically the case unless you want to create as many tasks as the number of threads times the number of elements to process. Therefore, in OpenMP task codes you'll find something similar to

    #pragma omp parallel
    #pragma omp single
    ...
       #pragma omp task
       { code for a given task; }
    ...

## Parallel processing of a c++ list container using OpenMP tasks
    #include <omp.h>
    #include <unistd.h>
    #include <iostream>
    #include <list>

    static void processElement (unsigned n)
    {
        // Tell who am I. The #pragma omp critical ensures that
        // only one thread sends data to std::cout
        #pragma omp critical
        std::cout <<
          "Thread " << omp_get_thread_num() << " processing element " << n
          << std::endl;

        // Simulate some work
        usleep (n*1000);
    }

    int main (void)
    {
        std::list<unsigned> lst;

        // Fill the list
        for (unsigned u = 0; u < 16; ++u)
                lst.push_back (1+u);

        // Now process each element of the list in parallel

        #pragma omp parallel  // Create a parallel region
        #pragma omp single    // Only one thread will instantiate tasks
        {
                for (auto element : lst)
                {
                        #pragma omp task firstprivate (element)
                        processElement (element);
                }
    
                // Wait for all tasks to be finished
                #pragma omp taskwait
        }

        return 0;
    }


This example simulates the processing of a STL list (named `lst` in the code) in parallel through the OpenMP task constructs (using the `#pragma omp task` directive). The example creates/instantiates one OpenMP task for each element in `lst` and the OpenMP threads execute the tasks as soon as they're ready to run.

    $ OMP_NUM_THREADS=4 ./a.out
    Thread 0 processing element 16
    Thread 3 processing element 3
    Thread 2 processing element 1
    Thread 1 processing element 2
    Thread 2 processing element 4
    Thread 1 processing element 5
    Thread 3 processing element 6
    Thread 2 processing element 7
    Thread 1 processing element 8
    Thread 3 processing element 9
    Thread 2 processing element 10
    Thread 1 processing element 11
    Thread 0 processing element 15
    Thread 3 processing element 12
    Thread 2 processing element 13
    Thread 1 processing element 14


## Recursive calculation for pi using OpenMP tasks
The code below calculates the value of PI using a recursive approach. Modify the `MAX_PARALLEL_RECURSIVE_LEVEL` value to determine at which recursion depth stop creating tasks. With this approach to create parallelism out of recursive applications: the more tasks you create, the more parallel tasks created but also the lesser work per task. So it is convenient to experiment with the application to understand at which level it creating further tasks do not benefit in terms of performance.

    #include <stdio.h>
    #include <omp.h>
    
    double pi_r (double h, unsigned depth, unsigned maxdepth, unsigned long long begin, unsigned long long niters)
    {
        if (depth < maxdepth)
        {
            double area1, area2;
    
            // Process first half
            #pragma omp task shared(area1)
            area1 = pi_r (h, depth+1, maxdepth, begin, niters/2-1);
    
            // Process second half
            #pragma omp task shared(area2)
            area2 = pi_r (h, depth+1, maxdepth, begin+niters/2, niters/2);
    
            #pragma omp taskwait
    
            return area1+area2;
        }
        else
        {
    
            unsigned long long i;
            double area = 0.0;
    
            for (i = begin; i <= begin+niters; i++)
            {
                double x = h * (i - 0.5);
                area += (4.0 / (1.0 + x*x));
            }
    
            return area;
        }
    }
    
    double pi (unsigned long long niters)
    {
        double res;
        double h = 1.0 / (double) niters;
    
        #pragma omp parallel shared(res)
        {
    #define MAX_PARALLEL_RECURSIVE_LEVEL 4
    
            #pragma omp single
            res = pi_r (h, 0, MAX_PARALLEL_RECURSIVE_LEVEL, 1, niters);
        }
        return res * h;
    }
    
    
    int main (int argc, char *argv[])
    {
    #define NITERS (100*1000*1000ULL)
    
        printf ("PI (w/%d iters) is %lf\n", NITERS, pi(NITERS));
    
        return 0;
    }



