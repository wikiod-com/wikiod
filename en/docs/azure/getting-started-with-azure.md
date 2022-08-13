---
title: "Getting started with azure"
slug: "getting-started-with-azure"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Azure N-series(GPU) : install CUDA, cudnn, Tensorflow on UBUNTU 16.04 LTS
After spending more than 5 hours, i found this easy solution:

-To verify that the system has a CUDA-capable GPU, run the following command:

    lspci | grep -i NVIDIA
You will see output similar to the following example (showing an NVIDIA Tesla K80/M60 card):

[![enter image description here][1]][1]


-Disabling the nouveau driver:

    sudo -i
    rmmod nouveau
-After a *reboot*: `sudo reboot`, verify the driver is installed properly by issuing:

    lsmod | grep -i nvidia

-Next, download the **CUDA** package from Nvidia, ...

    wget https://developer.nvidia.com/compute/cuda/8.0/prod/local_installers/cuda-repo-ubuntu1604-8-0-local_8.0.44-1_amd64-deb

-... make it known to apt-get and install the CUDA Toolkit:

    sudo dpkg -i cuda-repo-ubuntu1604-8-0-local_8.0.44-1_amd64-deb
    sudo apt-get update
    sudo apt-get install -y cuda

-Now we can check the status of the GPU(s) by running:

    nvidia-smi

Next, we download **cuDNN**...

    wget http://developer.download.nvidia.com/compute/redist/cudnn/v5.1/cudnn-8.0-linux-x64-v5.1.tgz

-... unzip, copy the lib64 and include folders:

    tar -zxf cudnn-8.0-linux-x64-v5.1.tgz
    sudo cp cuda/lib64/* /usr/local/cuda/lib64/
    sudo cp cuda/include/* /usr/local/cuda/include/
    sudo rm -R cuda

-Time to do some clean up and remove the downloaded archives:

    rm cuda-repo-ubuntu1604-8-0-local_8.0.44-1_amd64-deb
    rm cudnn-8.0-linux-x64-v5.1.tgz

To install **Tensorflow** with **CPU/GPU** , go here :

https://www.tensorflow.org/install/install_linux#installing_with_anaconda


Reference:

1.https://www.lutzroeder.com/blog/2016-12-27-tensorflow-azure
2.https://www.tensorflow.org/install/install_linux#installing_with_anaconda

  [1]: https://i.stack.imgur.com/12dFh.png
  [2]: https://www.tensorflow.org/install/install_linux#installing_with_anaconda

