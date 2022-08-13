---
title: "Pseudo-Random Number Generator Kernel Example"
slug: "pseudo-random-number-generator-kernel-example"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ------ | ------ |
| __global unsigned int * rnd_buffer   | unsigned int is standardised by the OpenCL standard as being 32-bit |
|  *  | __global means device's main memory for read/write access | 
|  *  | rnd_buffer is just a name in scope of "opencl program"(not host but device) |

## Using Thomas Wang's integer hash function
Auxilliary function that takes a seed and evaluates:

    uint wang_hash(uint seed)
    {
            seed = (seed ^ 61) ^ (seed >> 16);
            seed *= 9;
            seed = seed ^ (seed >> 4);
            seed *= 0x27d4eb2d;
            seed = seed ^ (seed >> 15);
            return seed;
     }

another auxilliary function that uses it to initialize a buffer location shown by "id":

     void wang_rnd_0(__global unsigned int * rnd_buffer,int id)                
     {
         uint maxint=0;
         maxint--;
         uint rndint=wang_hash(id);
         rnd_buffer[id]=rndint;
     }

and another doing extra float output between 0 and 1

     float wang_rnd(__global unsigned int * rnd_buffer,int id)                
     {
         uint maxint=0;
         maxint--; // not ok but works
         uint rndint=wang_hash(rnd_buffer[id]);
         rnd_buffer[id]=rndint;
         return ((float)rndint)/(float)maxint;
     }

Initializer kernel:

     __kernel void rnd_init(__global unsigned int * rnd_buffer)
     {
           int id=get_global_id(0);
           wang_rnd_0(rnd_buffer,id);  // each (id) thread has its own random seed now           
     }

Single iteration kernel:

     __kernel void rnd_1(__global unsigned int * rnd_buffer)
     {
          int id=get_global_id(0);

          // can use this to populate a buffer with random numbers 
          // concurrently on all cores of a gpu
          float thread_private_random_number=wang_rnd(rnd_buffer,id);
     }

