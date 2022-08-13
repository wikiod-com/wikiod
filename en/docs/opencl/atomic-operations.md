---
title: "Atomic Operations"
slug: "atomic-operations"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Syntax
 - int atomic_add (    volatile __global int *p , int val)

 - unsigned int atomic_add (    volatile __global unsigned int *p , unsigned int val)

 - int atomic_add (    volatile __local int *p , int val)

 - unsigned int atomic_add (    volatile __local unsigned int *p ,unsigned int val)

## Parameters
|p|val|
|-----|-----|
|pointer to cell|added to cell|


Performance depends on atomic operations number and memory space. Doing serial work almost always slows kernel execution because of gpu being a SIMD array and each unit in an array waits other units if they don't do same type of work.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/ORB4s.gif

## Atomic Add Function
        int fakeMalloc(__local int * addrCounter,int size)
        {
            // lock addrCounter
            // adds size to addrCounter's pointed cell value
            // unlock
            // return old value of addrCounter's pointed cell
  
            // serial between all threads visiting -> slow
            return atomic_add(addrCounter,size);
        }

        __kernel void vecAdd(__global float* results )
        {
           int id = get_global_id(0);
           int lid=get_local_id(0);
           __local float stack[1024];
           __local int ctr;
           if(lid==0)
              ctr=0;
           barrier(CLK_LOCAL_MEM_FENCE);
           stack[lid]=0.0f;                    // parallel operation
           barrier(CLK_LOCAL_MEM_FENCE);
           int fakePointer=fakeMalloc(&ctr,1); // serial operation
           barrier(CLK_LOCAL_MEM_FENCE);
           stack[fakePointer]=lid;             // parallel operation
           barrier(CLK_GLOBAL_MEM_FENCE);
           results[id]=stack[lid];
        }

Output of first elements:

sometimes

192
193
194
195
196
197
198

sometimes

0
1
2
3
4
5
6

sometimes

128
129
130
131
132
133
134

for a setting with local range=256.

Whatever thread visits fakeMalloc first, it puts its own local thread id in first result cell. Internal SIMD and wavefront structure of the example gpu lets neighboring 64 threads to put their values to neighboring result cells. Other devices may put values more randomly or totally in order depending on the opencl implementation of that devices.




