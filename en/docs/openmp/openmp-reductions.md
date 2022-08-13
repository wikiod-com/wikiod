---
title: "OpenMP reductions"
slug: "openmp-reductions"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Approximation of PI using #pragma omp reduction clause
    h = 1.0 / n;
    #pragma omp parallel for private(x) shared(n, h) reduction(+:area) 
    for (i = 1; i <= n; i++)
    {
      x = h * (i - 0.5);
      area += (4.0 / (1.0 + x*x));
    }
    pi = h * area;

In this example, each threads execute a subset of the iteration count. Each thread has its local private copy of `area` and at the end of the parallel region they all apply the addition operation (`+`) so as to generate the final value for `area`.

## Approximation of PI using reductions based on #pragma omp critical
    h = 1.0 / n;
    #pragma omp parallel for private(x) shared(n, h, area) 
    for (i = 1; i <= n; i++)
    {
      x = h * (i - 0.5);
      #pragma omp critical
      {
        area += (4.0 / (1.0 + x*x));
      }
    }
    pi = h * area;

In this example, each threads execute a subset of the iteration count and they accumulate atomically into the shared variable `area`, which ensures that there are no lost updates.

## Approximation of PI using reductions based on #pragma atomic
    h = 1.0 / n;
    #pragma omp parallel for private(x) shared(n, h, area) 
    for (i = 1; i <= n; i++)
    {
      x = h * (i - 0.5);
      #pragma atomic
      area += (4.0 / (1.0 + x*x));
    }
    pi = h * area;

In this example, each threads execute a subset of the iteration count and they accumulate atomically into the shared variable `area`, which ensures that there are no lost updates. We can use the `#pragma atomic` in here because the given operation (`+=`) can be done atomically, which simplifies the readability compared to the usage of the `#pragma omp critical`.

## Approximation of PI hand-crafting the #pragma omp reduction
    h = 1.0 / n;

    #pragma omp parallel private(x) shared(n, h)
    {
      double thread_area = 0;                      // Private / local variable

      #pragma omp for
      for (i = 1; i <= n; i++)
      {
        x = h * (i - 0.5);
        thread_area += (4.0 / (1.0 + x*x));
      }

      #pragma omp atomic                       // Applies the reduction manually
      area += thread_area;                     // All threads aggregate into area
    }

    pi = h * area;


The threads are spawned in the `#pragma omp parallel`. Each thread will have an independent/private `thread_area` that stores its partial addition. The following loop is distributed among threads using `#pragma omp for`. In this loop, each thread calculates its own `thread_area` and after this loop, the code sequentially aggregates the area atomically through `#pragma omp atomic`.



