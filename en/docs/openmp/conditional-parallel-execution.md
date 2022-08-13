---
title: "Conditional parallel execution"
slug: "conditional-parallel-execution"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Conditional clauses in OpenMP parallel regions
    #include <omp.h>
    #include <stdio.h>

    int main (void)
    {
      int t = (0 == 0); // true value
      int f = (1 == 0); // false value

      #pragma omp parallel if (f)
      { printf ("FALSE: I am thread %d\n", omp_get_thread_num()); }

      #pragma omp parallel if (t)
      { printf ("TRUE : I am thread %d\n", omp_get_thread_num()); }

      return 0;
    }

Its output is:

    $ OMP_NUM_THREADS=4 ./test
    FALSE: I am thread 0
    TRUE : I am thread 0
    TRUE : I am thread 1
    TRUE : I am thread 3
    TRUE : I am thread 2

