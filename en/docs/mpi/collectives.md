---
title: "Collectives"
slug: "collectives"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Collectives operations are MPI calls designed communicate the processes pointed out by a communicator in a single operation or to perform a synchronization among them. These are often used to calculate one or more values based on data contributed by other processes or to distribute or collect data from all other processes.

Note that all the processes in the communicator should invoke the same collective operations in order, otherwise the application would block.

## Broadcast
The following code broadcasts the contents in `buffer` among all the processes belonging to the `MPI_COMM_WORLD` communicator (i.e. all the processes running in parallel) using the `MPI_Bcast` operation.  

    int rank;
    int res;

    res = MPI_Comm_rank (MPI_COMM_WORLD, &rank);
    if (res != MPI_SUCCESS)
    {
        fprintf (stderr, "MPI_Comm_rank failed\n");
        exit (0);
    }

    int buffer[100];
    if (rank == 0)
    {
        // Process with rank id 0 should fill the buffer structure
        // with the data it wants to share.
    }

    res = MPI_Bcast (buffer, 100, MPI_INT, 0, MPI_COMM_WORLD);
    if (res != MPI_SUCCESS)
    {
        fprintf (stderr, "MPI_Bcast failed\n");
        exit (0);
    }


## Barrier
The `MPI_Barrier` operation performs a synchronization among the processes belonging to the given communicator. That is, all the processes from a given communicator will wait within the `MPI_Barrier` until all of them are inside, and at that point, they will leave the operation.

    int res;

    res = MPI_Barrier (MPI_COMM_WORLD); /* all processes will wait */
    if (res != MPI_SUCCESS)
    {
        fprintf (stderr, "MPI_Barrier failed\n");
        exit (0);
    }

## Scatter
The root process scatters the contents in `sendbuf` to all processes (including itself) using the `MPI_Scatter` operation.

    int rank;
    int size;
    int sendcount = 1;
    int recvcount = sendcount;
    int sendbuf[3];
    int recvbuf;
    int root = 0;

    MPI_Comm_size (MPI_COMM_WORLD, &size);

    if (size != 3)
    {
        fprintf (stderr, "Number of processes must be 3\n");
        exit (0);
    }

    MPI_Comm_rank (MPI_COMM_WORLD, &rank);

    if (rank == 0)
    {
        sendbuf[0] = 3;
        sendbuf[1] = 5;
        sendbuf[2] = 7;
    }

    MPI_Scatter (sendbuf, sendcount, MPI_INT, &recvbuf, recvcount, MPI_INT, root, MPI_COMM_WORLD);

    printf ("rank: %i, value: %i\n", rank, recvbuf);

    /* Output:
     * rank: 0, value: 3
     * rank: 1, value: 5
     * rank: 2, value: 7
     */



