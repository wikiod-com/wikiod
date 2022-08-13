---
title: "Getting started with mpi"
slug: "getting-started-with-mpi"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World!
Three things are usually important when starting to learn to use MPI. First, you must initialize the library when you are ready to use it (you also need to finalize it when you are done). Second, you will want to know the size of your communicator (the thing you use to send messages to other processes). Third, you will want to know your rank within that communicator (which process number are you within that communicator).

    #include <mpi.h>
    #include <stdio.h>
    
    int main(int argc, char **argv) {
        int size, rank;
        int res;
    
        res = MPI_Init(&argc, &argv);
        if (res != MPI_SUCCESS)
        {
            fprintf (stderr, "MPI_Init failed\n");
            exit (0);
        }
    
        res = MPI_Comm_size(MPI_COMM_WORLD, &size);
        if (res != MPI_SUCCESS)
        {
            fprintf (stderr, "MPI_Comm_size failed\n");
            exit (0);
        }
        res = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        if (res != MPI_SUCCESS)
        {
            fprintf (stderr, "MPI_Comm_rank failed\n");
            exit (0);
        }
    
        fprintf(stdout, "Hello World from rank %d of %d~\n", rank, size);
    
        res = MPI_Finalize();
        if (res != MPI_SUCCESS)
        {
            fprintf (stderr, "MPI_Finalize failed\n");
            exit (0);
        }
    }

If you run this program like this:

    mpiexec -n 2 ./hello

You would expect to get output like this:

    Hello World from rank 0 of 2!
    Hello World from rank 1 of 2!

You could also get that output backward (see http://stackoverflow.com/a/17571699/491687\) for more discussion of this:
    
    Hello World from rank 1 of 2!
    Hello World from rank 0 of 2!


## Rank and size
To get the size of a communicator (e.g. `MPI_COMM_WORLD`) and the local process' rank inside it:
 
    int rank, size;
    int res;
    MPI_Comm communicator = MPI_COMM_WORLD;

    res = MPI_Comm_rank (communicator, &rank);
    if (res != MPI_SUCCESS)
    {
      fprintf (stderr, "MPI_Comm_rank failed\n");
      exit (0);
    }
    res = MPI_Comm_size (communicator, &size);
    if (res != MPI_SUCCESS)
    {
      fprintf (stderr, "MPI_Comm_size failed\n");
      exit (0);
    }


## Init/Finalize
Before any MPI commands can be run, the environment needs to be initialized, and finalized in the end:

    int main(int argc, char** argv)
    {
        int res;
        res = MPI_Init(&argc,&argv);
        if (res != MPI_SUCCESS)
        {
          fprintf (stderr, "MPI_Init failed\n");
          exit (0);
        }
        ...
        res = MPI_Finalize();
        if (res != MPI_SUCCESS)
        {
          fprintf (stderr, "MPI_Finalize failed\n");
          exit (0);
        }
    }

## Return values of MPI calls
Almost any MPI call returns an integer error code, which signifies the success of the operation. If no error occurs, the return code is `MPI_SUCCESS`:

<!-- language-all: lang-c -->

    if (MPI_Some_op(...) != MPI_SUCCESS)
    {
       // Process error
    }

If an error occurs, MPI calls an error handler associated with the communicator, window or file object before returning to the user code. There are two predefined error handlers (the user can define additional error handlers):

* `MPI_ERRORS_ARE_FATAL` - errors result in termination of the MPI program
* `MPI_ERRORS_RETURN` - errors result in the error code being passed back to the user

The default error handler for communicators and windows is `MPI_ERRORS_ARE_FATAL`; for file objects it is `MPI_ERRORS_RETURN`. The error handler for `MPI_COMM_WORLD` also applies to all operations that are not specifically related to an object (e.g., `MPI_Get_count`). Thus, checking the return value of non-I/O operations without setting the error handler to `MPI_ERRORS_RETURN` is redundant as erroneous MPI calls will not return.

    // The execution will not reach past the following line in case of error
    int res = MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (res != MPI_SUCCESS)
    {
        // The following code will never get executed
        fprintf(stderr, "MPI_Comm_size failed: %d\n", res);
        exit(EXIT_FAILURE);
    }

To enable user error processing, one must first change the error handler of `MPI_COMM_WORLD`:

    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

    int res = MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (res != MPI_SUCCESS)
    {
        fprintf(stderr, "MPI_Comm_size failed: %d\n", res);
        exit(EXIT_FAILURE);
    }

The MPI standard does not require that MPI implementations are able to recover from errors and continue the program execution.

