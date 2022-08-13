---
title: "Process creation and management"
slug: "process-creation-and-management"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Spawn
Master process spawns two worker processes and scatters `sendbuf` to workers.

**master.c**

    #include "mpi.h"
    
    int main(int argc, char *argv[])
    { 
       int n_spawns = 2;
       MPI_Comm intercomm;
    
       MPI_Init(&argc, &argv);
    
       MPI_Comm_spawn("worker_program", MPI_ARGV_NULL, n_spawns, MPI_INFO_NULL, 0, MPI_COMM_SELF, &intercomm, MPI_ERRCODES_IGNORE); 
    
       int sendbuf[2] = {3, 5};
       int recvbuf; // redundant for master.
    
       MPI_Scatter(sendbuf, 1, MPI_INT, &recvbuf, 1, MPI_INT, MPI_ROOT, intercomm);
    
       MPI_Finalize();
       return 0;
    }

**worker.c**

    #include "mpi.h"
    #include <stdio.h>
    
    int main(int argc, char *argv[])
    {  
       MPI_Init(&argc, &argv);
    
       MPI_Comm intercomm; 
       MPI_Comm_get_parent(&intercomm);
    
       int sendbuf[2]; // redundant for worker.
       int recvbuf;
    
       MPI_Scatter(sendbuf, 1, MPI_INT, &recvbuf, 1, MPI_INT, 0, intercomm);
       printf("recvbuf = %d\n", recvbuf);
    
       MPI_Finalize();
       return 0;
    }

**Execution**

    mpicc master.c -o master_program
    mpicc worker.c -o worker_program
    mpirun -n 1 master_program

## Establishing connection between two independent applications
Master process spawns server and client applications with a single process for each application. Server opens a port and client connects to that port. Then client sends data to server with `MPI_Send` to verify that the connection is established.

**master.c**

    #include "mpi.h"
    
    int main(int argc, char *argv[])
    {   
       MPI_Init(&argc, &argv);
    
       MPI_Comm intercomm;
       // Spawn two applications with a single process for each application.
       // Server must be spawned before client otherwise the client will complain at MPI_Lookup_name().
       MPI_Comm_spawn("server", MPI_ARGV_NULL, 1, MPI_INFO_NULL, 0, MPI_COMM_SELF, &intercomm, MPI_ERRCODES_IGNORE);
       MPI_Comm_spawn("client", MPI_ARGV_NULL, 1, MPI_INFO_NULL, 0, MPI_COMM_SELF, &intercomm, MPI_ERRCODES_IGNORE);
       
       MPI_Finalize();
       return 0;
    }

**server.c**

    #include "mpi.h"
    #include <stdio.h>
    
    int main(int argc, char *argv[])
    {  
       MPI_Init(&argc, &argv);
    
       // Open port.
       char port_name[MPI_MAX_PORT_NAME];
       MPI_Open_port(MPI_INFO_NULL, port_name);
    
       // Publish port name and accept client.
       MPI_Comm client;
       MPI_Publish_name("name", MPI_INFO_NULL, port_name);
       MPI_Comm_accept(port_name, MPI_INFO_NULL, 0, MPI_COMM_WORLD, &client);
       
       // Receive data from client.
       int recvbuf;
       MPI_Recv(&recvbuf, 1, MPI_INT, 0, 0, client, MPI_STATUS_IGNORE);
       printf("recvbuf = %d\n", recvbuf);
    
       MPI_Unpublish_name("name", MPI_INFO_NULL, port_name);
    
       MPI_Finalize();
       return 0;
    }

**client.c**

    #include "mpi.h"
    
    int main(int argc, char *argv[])
    {   
       MPI_Init(&argc, &argv);
    
       // Look up for server's port name.
       char port_name[MPI_MAX_PORT_NAME];
       MPI_Lookup_name("name", MPI_INFO_NULL, port_name);
       
       // Connect to server.
       MPI_Comm server;
       MPI_Comm_connect(port_name, MPI_INFO_NULL, 0, MPI_COMM_SELF, &server);
    
       // Send data to server.
       int sendbuf = 3;
       MPI_Send(&sendbuf, 1, MPI_INT, 0, 0, server);
       
       MPI_Finalize();
       return 0;
    }

**Command line**

    mpicc master.c -o master_program
    mpicc server.c -o server
    mpicc client.c -o client
    mpirun -n 1 master_program

