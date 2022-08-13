---
title: "Process Topologies"
slug: "process-topologies"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Graph topology creation and communication
Creates a graph topology in a distributed manner so that each node defines its neighbors. Each node communicates its rank among neighbors with `MPI_Neighbor_allgather`.

[![enter image description here][1]][1]

    #include <mpi.h>
    #include <stdio.h>
    
    #define nnode 4
    
    int main()
    {
        MPI_Init(NULL, NULL);
    
        int rank;
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    
        int source = rank;
        int degree;
        int dest[nnode];
        int weight[nnode] = {1, 1, 1, 1};
        int recv[nnode] = {-1, -1, -1, -1};
        int send = rank;
    
        // set dest and degree.
        if (rank == 0)
        {
            dest[0] = 1;
            dest[1] = 3;
            degree = 2;
        }
        else if(rank == 1)
        {
            dest[0] = 0;
            degree = 1;
        }
        else if(rank == 2)
        {
            dest[0] = 3;
            dest[1] = 0;
            dest[2] = 1;
            degree = 3;
        }
        else if(rank == 3)
        {
            dest[0] = 0;
            dest[1] = 2;
            dest[2] = 1;
            degree = 3;
        }
    
        // create graph.
        MPI_Comm graph;
        MPI_Dist_graph_create(MPI_COMM_WORLD, 1, &source, &degree, dest, weight, MPI_INFO_NULL, 1, &graph);
    
        // send and gather rank to/from neighbors.
        MPI_Neighbor_allgather(&send, 1, MPI_INT, recv, 1, MPI_INT, graph);
    
        printf("Rank: %i, recv[0] = %i, recv[1] = %i, recv[2] = %i, recv[3] = %i\n", rank, recv[0], recv[1], recv[2], recv[3]);
    
        MPI_Finalize();
        return 0;
    }


  [1]: https://i.stack.imgur.com/YRGBh.png

