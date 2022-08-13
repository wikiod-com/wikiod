---
title: "TensorFlow GPU setup"
slug: "tensorflow-gpu-setup"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

This topic is about setting up and managing GPUs in TensorFlow.

It assumes that the GPU version of TensorFlow has been installed (see https://www.tensorflow.org/install/ for more information on the GPU installation).

You also might want to have a look to the official documentation: https://www.tensorflow.org/tutorials/using_gpu



**Main sources:**


 - https://www.tensorflow.org
 - https://github.com/tensorflow/tensorflow/blob/master/tensorflow/core/protobuf/config.proto
 - https://stackoverflow.com/a/37901914
 - https://github.com/tensorflow/tensorflow/issues/152
 - https://github.com/tensorflow/tensorflow/issue/9201

## Control the GPU memory allocation
By default, TensorFlow pre-allocate the whole memory of the GPU card (which can causes `CUDA_OUT_OF_MEMORY` warning).

To change this, it is possible to

 - change the percentage of memory pre-allocated, using `per_process_gpu_memory_fraction` config option,

      > A value between 0 and 1 that indicates what fraction of the  
      available GPU memory to pre-allocate for each process.  1 means  
      to pre-allocate all of the GPU memory, 0.5 means the process  
      allocates ~50% of the available GPU memory.  

- disable the pre-allocation, using `allow_growth` config option. Memory allocation will grow as usage grows.

    >  If true, the allocator does not pre-allocate the entire specified  
      GPU memory region, instead starting small and growing as needed.

For example:

    config = tf.ConfigProto()
    config.gpu_options.per_process_gpu_memory_fraction = 0.4
    sess = tf.Session(config=config) as sess:


or

    config = tf.ConfigProto()
    config.gpu_options.allow_growth = True
    sess= tf.Session(config=config):


More information on the config options [here][1].


  [1]: https://github.com/tensorflow/tensorflow/blob/master/tensorflow/core/protobuf/config.proto

## Run TensorFlow on CPU only - using the `CUDA_VISIBLE_DEVICES` environment variable.
To ensure that a GPU version TensorFlow process only runs on CPU:

 
    import os
    os.environ["CUDA_VISIBLE_DEVICES"]="-1"    
    import tensorflow as tf

For more information on the `CUDA_VISIBLE_DEVICES`, have a look to this [answer][1] or to the [CUDA documentation][2].


  [1]: https://stackoverflow.com/a/44518219/4282745
  [2]: http://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#env-vars

## Run TensorFlow Graph on CPU only - using `tf.config`
    import tensorflow as tf
    sess = tf.Session(config=tf.ConfigProto(device_count={'GPU': 0}))


Bear in mind that this method prevents the TensorFlow Graph from using the GPU but TensorFlow still lock the GPU device as described in this an [issue][1] opened on this method. Using the `CUDA_VISIBLE_DEVICES` seems to be the best way to ensure that TensorFlow is kept away from the GPU card (see this [answer][2]).


  [1]: https://github.com/tensorflow/tensorflow/issues/9201#issue-221681884
  [2]: https://github.com/tensorflow/tensorflow/issues/9201#issuecomment-294092781

## Use a particular set of GPU devices
To use a particular set of GPU devices, the `CUDA_VISIBLE_DEVICES` environment variable can be used:

    import os
    os.environ["CUDA_DEVICE_ORDER"]="PCI_BUS_ID" # see issue #152
    os.environ["CUDA_VISIBLE_DEVICES"]="0" # Will use only the first GPU device

    
    os.environ["CUDA_VISIBLE_DEVICES"]="0,3" # Will use only the first and the fourth GPU devices

(Quoted from this [answer][1]; more information on the CUDA environment variables [here][2].)


  [1]: https://stackoverflow.com/a/37901914
  [2]: http://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#env-vars

## List the available devices available by TensorFlow in the local process.
    from tensorflow.python.client import device_lib
    print(device_lib.list_local_devices())

