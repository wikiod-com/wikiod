---
title: "Running an MPI Program"
slug: "running-an-mpi-program"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ---- | --- |
| `-n <num_procs>` | The number of MPI processes to start up for the job |

## Execute your job
The simplest way to run your job is to use `mpiexec` or `mpirun` (they are usually the same thing and aliases of each other).

    mpiexec -n 2 ./my_prog

