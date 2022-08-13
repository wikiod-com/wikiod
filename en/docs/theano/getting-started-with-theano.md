---
title: "Getting started with theano"
slug: "getting-started-with-theano"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing Theano and configuring the GPU on Ubuntu 14.04
You can use the following instructions to install Theano and configure the GPU (assume a freshly installed Ubuntu 14.04):

    # Install Theano
    sudo apt-get install python-numpy python-scipy python-dev python-pip python-nose g++ libopenblas-dev git
    sudo pip install Theano
    
    # Install Nvidia drivers, CUDA and CUDA toolkit, following some instructions from http://docs.nvidia.com/cuda/cuda-installation-guide-linux/index.html
    wget http://developer.download.nvidia.com/compute/cuda/7.5/Prod/local_installers/cuda-repo-ubuntu1404-7-5-local_7.5-18_amd64.deb # Got the link at https://developer.nvidia.com/cuda-downloads
    sudo dpkg -i cuda-repo-ubuntu1404-7-5-local_7.5-18_amd64.deb
    sudo apt-get update
    sudo apt-get install cuda
    
    sudo reboot

At that point, running `nvidia-smi` should work, but running `nvcc` won't work.

    # Execute in console, or (add in ~/.bash_profile then run "source ~/.bash_profile"):
    export PATH=/usr/local/cuda-7.5/bin:$PATH
    export LD_LIBRARY_PATH=/usr/local/cuda-7.5/lib64:$LD_LIBRARY_PATH

At that point, both `nvidia-smi` and `nvcc` should work.

To test whether Theano is able to use the GPU:

Copy-paste the following in `gpu_test.py`:

    # Start gpu_test.py
    # From http://deeplearning.net/software/theano/tutorial/using_gpu.html#using-gpu
    from theano import function, config, shared, sandbox
    import theano.tensor as T
    import numpy
    import time
    
    vlen = 10 * 30 * 768  # 10 x #cores x # threads per core
    iters = 1000
    
    rng = numpy.random.RandomState(22)
    x = shared(numpy.asarray(rng.rand(vlen), config.floatX))
    f = function([], T.exp(x))
    print(f.maker.fgraph.toposort())
    t0 = time.time()
    for i in xrange(iters):
        r = f()
    t1 = time.time()
    print("Looping %d times took %f seconds" % (iters, t1 - t0))
    print("Result is %s" % (r,))
    if numpy.any([isinstance(x.op, T.Elemwise) for x in f.maker.fgraph.toposort()]):
        print('Used the cpu')
    else:
        print('Used the gpu')
    # End gpu_test.py

and run it:

    THEANO_FLAGS='mode=FAST_RUN,device=gpu,floatX=float32' python gpu_test.py

which should return:

    f@f-Aurora-R4:~$ THEANO_FLAGS='mode=FAST_RUN,device=gpu,floatX=float32' python gpu_test.py
    Using gpu device 0: GeForce GTX 690
    [GpuElemwise{exp,no_inplace}(<CudaNdarrayType(float32, vector)>), HostFromGpu(GpuElemwise{exp,no_inplace}.0)]
    Looping 1000 times took 0.658292 seconds
    Result is [ 1.23178029  1.61879349  1.52278066 ...,  2.20771813  2.29967761
      1.62323296]
    Used the gpu


To know your CUDA version:

    ​nvcc -V

Example:


    username@server:~$ nvcc -V
    nvcc: NVIDIA (R) Cuda compiler driver
    Copyright (c) 2005-2015 NVIDIA Corporation
    Built on Tue_Aug_11_14:27:32_CDT_2015
    Cuda compilation tools, release 7.5, V7.5.17



----------

## Adding cuDNN ##

To add cuDNN (instructions from http://deeplearning.net/software/theano/library/sandbox/cuda/dnn.html):

1. Download cuDNN from https://developer.nvidia.com/rdp/cudnn-download (need registration, which is free)
2. `tar -xvf cudnn-7.0-linux-x64-v3.0-prod.tgz`
3. Do one of the following

Option 1: Copy the `*.h` files to `CUDA_ROOT/include` and the `*.so*` files to `CUDA_ROOT/lib64` (by default, `CUDA_ROOT` is `/usr/local/cuda` on Linux).

    sudo cp cuda/lib64/* /usr/local/cuda/lib64/
    sudo cp cuda/include/cudnn.h /usr/local/cuda/include/


Option 2:

    export LD_LIBRARY_PATH=/home/user/path_to_CUDNN_folder/lib64:$LD_LIBRARY_PATH
    export CPATH=/home/user/path_to_CUDNN_folder/include:$CPATH
    export LIBRARY_PATH=/home/user/path_to_CUDNN_folder/lib64:$LD_LIBRARY_PATH

By default, Theano will detect if it can use cuDNN. If so, it will use it. If not, Theano optimizations will not introduce cuDNN ops. So Theano will still work if the user did not introduce them manually.

To get an error if Theano can not use cuDNN, use this Theano flag: `optimizer_including=cudnn`.

Example:

    THEANO_FLAGS='mode=FAST_RUN,device=gpu,floatX=float32,optimizer_including=cudnn' python gpu_test.py

To know your cuDNN version:

    cat /usr/local/cuda/include/cudnn.h | grep CUDNN_MAJOR -A 2


----------


## Adding CNMeM ##

The [CNMeM library](https://github.com/NVIDIA/cnmem) is a "Simple library to help the Deep Learning frameworks manage CUDA memory.". 

    # Build CNMeM without the unit tests
    git clone https://github.com/NVIDIA/cnmem.git cnmem
    cd cnmem
    mkdir build
    cd build
    sudo apt-get install -y cmake
    cmake ..
    make
    
    # Copy files to proper location
    sudo cp ../include/cnmem.h /usr/local/cuda/include
    sudo cp *.so /usr/local/cuda/lib64/
    cd ../..

To use with Theano, you need to add the `lib.cnmem` flag. Example:

    THEANO_FLAGS='mode=FAST_RUN,device=gpu,floatX=float32,lib.cnmem=0.8,optimizer_including=cudnn' python gpu_test.py

The first output of the script should be:

    Using gpu device 0: GeForce GTX TITAN X (CNMeM is enabled with initial size: 80.0% of memory, cuDNN 5005)


`lib.cnmem=0.8` means that it can use up to 80% of the GPU.

CNMeM has been reported to give some interesting speed improvements, and is supported by Theano, Torch, and Caffee.

[Theano - source 1](https://github.com/fchollet/keras/issues/987):

> The speed up depend of many factor, like the shapes and the model itself. The speed up go from 0 to 2x faster.

[Theano - source 2](https://groups.google.com/d/msg/theano-users/nvfrmlJ96TA/Y9NhxP7k6qgJ):

> If you don't change the Theano flag allow_gc, you can expect 20% speed up on the GPU. In some case (small models), we saw a 50% speed up.





----------

Common issues:

-  [Importing theano: AttributeError: 'module' object has no attribute 'find_graphviz'](http://stackoverflow.com/q/38446771/395857)






## Installation or Setup
Detailed instructions on getting theano set up or installed.

## Running Theano on multiple CPU cores
You can run Theano on multiple CPU cores with the [`OMP_NUM_THREADS=[number_of_cpu_cores]` flag](http://deeplearning.net/software/theano/tutorial/multi_cores.html). 

Example:

    OMP_NUM_THREADS=4 python gpu_test.py 

The script [`theano/misc/check_blas.py`](https://github.com/Theano/Theano/blob/master/theano/misc/check_blas.py) outputs information regarding which BLAS is used:

    cd [theano_git_directory]
    OMP_NUM_THREADS=4 python theano/misc/check_blas.py


## Your first theano program
In this example, we will compile functions that computes sum and difference given two real number.

    from __future__ import print_function
    import theano
    import theano.tensor as T
    
    #define two symbolic scalar
    s_x = T.fscalar()
    s_y = T.fscalar()

    #compute something
    s_sum = s_x + s_y
    s_diff = s_x - s_y

    #compile a function that adds two number
    #theano will call system compiler at here
    fn_add = theano.function(inputs=[s_x, s_y], outputs=s_sum)
    fn_diff = theano.function(inputs=[s_x, s_y], outputs=s_diff)

    #call the compiled functions
    print(fn_add(2., 2.)) #4.
    print(fn_diff(2., 2.)) #0.

