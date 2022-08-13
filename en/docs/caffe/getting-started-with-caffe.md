---
title: "Getting started with caffe"
slug: "getting-started-with-caffe"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and setup
Ubuntu
------

Below are detailed instructions to install Caffe, pycaffe as well as its dependencies, on Ubuntu 14.04 x64 or 14.10 x64. 

Execute the following script, e.g. "bash compile_caffe_ubuntu_14.sh" (~30 to 60 minutes on a new Ubuntu). 


    # This script installs Caffe and pycaffe. 
    # CPU only, multi-threaded Caffe.
 
    # Usage: 
    # 0. Set up here how many cores you want to use during the installation:
    # By default Caffe will use all these cores.
    NUMBER_OF_CORES=4
    
    sudo apt-get install -y libprotobuf-dev libleveldb-dev libsnappy-dev 
    sudo apt-get install -y libopencv-dev libhdf5-serial-dev
    sudo apt-get install -y --no-install-recommends libboost-all-dev
    sudo apt-get install -y libatlas-base-dev 
    sudo apt-get install -y python-dev 
    sudo apt-get install -y python-pip git
    
    # For Ubuntu 14.04
    sudo apt-get install -y libgflags-dev libgoogle-glog-dev liblmdb-dev protobuf-compiler 
    
    # Install LMDB
    git clone https://github.com/LMDB/lmdb.git 
    cd lmdb/libraries/liblmdb
    sudo make 
    sudo make install
    
    # More pre-requisites 
    sudo apt-get install -y cmake unzip doxygen
    sudo apt-get install -y protobuf-compiler
    sudo apt-get install -y libffi-dev python-pip python-dev build-essential
    sudo pip install lmdb
    sudo pip install numpy
    sudo apt-get install -y python-numpy
    sudo apt-get install -y gfortran # required by scipy
    sudo pip install scipy # required by scikit-image
    sudo apt-get install -y python-scipy # in case pip failed
    sudo apt-get install -y python-nose
    sudo pip install scikit-image # to fix https://github.com/BVLC/caffe/issues/50
    
    
    # Get caffe (http://caffe.berkeleyvision.org/installation.html#compilation)
    cd
    mkdir caffe
    cd caffe
    wget https://github.com/BVLC/caffe/archive/master.zip
    unzip -o master.zip
    cd caffe-master
    
    # Prepare Python binding (pycaffe)
    cd python
    for req in $(cat requirements.txt); do sudo pip install $req; done

    # to be able to call "import caffe" from Python after reboot:
    echo "export PYTHONPATH=$(pwd):$PYTHONPATH " >> ~/.bash_profile 
    source ~/.bash_profile # Update shell 
    cd ..
    
    # Compile caffe and pycaffe
    cp Makefile.config.example Makefile.config
    sed -i '8s/.*/CPU_ONLY := 1/' Makefile.config # Line 8: CPU only
    sudo apt-get install -y libopenblas-dev
    sed -i '33s/.*/BLAS := open/' Makefile.config # Line 33: to use OpenBLAS
    # Note that if one day the Makefile.config changes and these line numbers may change
    echo "export OPENBLAS_NUM_THREADS=($NUMBER_OF_CORES)" >> ~/.bash_profile 
    mkdir build
    cd build
    cmake ..
    cd ..
    make all -j$NUMBER_OF_CORES # 4 is the number of parallel threads for compilation: typically equal to number of physical cores
    make pycaffe -j$NUMBER_OF_CORES
    make test
    make runtest
    #make matcaffe
    make distribute
    
    # Afew few more dependencies for pycaffe
    sudo pip install pydot
    sudo apt-get install -y graphviz
    sudo pip install scikit-learn
    
At the end, you need to run "source ~/.bash_profile" manually or start a new shell to be able to do 'python import caffe'.

## Enable multithreading with Caffe
Caffe can run on multiple cores. One way is to enable multithreading with Caffe to use OpenBLAS instead of the default ATLAS. To do so, you can follow these three steps:

1. `sudo apt-get install -y libopenblas-dev`
2. Before compiling Caffe, edit [`Makefile.config`](https://github.com/BVLC/caffe/blob/master/Makefile.config.example), replace `BLAS := atlas` by `BLAS := open`
3. After compiling Caffe, running `export OPENBLAS_NUM_THREADS=4` will cause Caffe to use 4 cores.


## Regularization loss (weight decay) in Caffe
In the [solver][1] file, we can set a global regularization loss using the `weight_decay` and `regularization_type` options.

In many cases we want different weight decay rates for different layers. This can be done by setting the `decay_mult` option for each layer in the network definition file, where  `decay_mult` is the multiplier on the global weight decay rate, so the actual weight decay rate applied for one layer is `decay_mult*weight_decay`.

For example, the following defines a convolutional layer with NO weight decay regardless of the options in the solver file.

    layer {
      name: "Convolution1"
      type: "Convolution"
      bottom: "data"
      top: "Convolution1"
      param {
        decay_mult: 0
      }
      convolution_param {
        num_output: 32
        pad: 0
        kernel_size: 3
        stride: 1
        weight_filler {
          type: "xavier"
        }
      }
    }

See [this thread][2] for more information.

  [1]: http://caffe.berkeleyvision.org/tutorial/solver.html
  [2]: http://stackoverflow.com/q/32177764/1714410

