---
title: "GPU-accelerated computing"
slug: "gpu-accelerated-computing"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

GPU computing requires a 'platform' which can connect to and utilize the hardware.  The two primary low-level languages that accomplish this are CUDA and OpenCL.  The former requires installation of the proprietary NVIDIA CUDA Toolkit and is only applicable on NVIDIA GPUs.  The latter is both company (e.g. NVIDIA, AMD, Intel) and hardware independent (CPU or GPU) but requires the installation of an SDK (software development kit).  In order to use a GPU via R you will need to install one of these pieces of software first.

Once either the CUDA Toolkit or a OpenCL SDK is installed, you can install an appropriate R package.  Almost all the R GPU packages are dependent upon CUDA and limited to NVIDIA GPUs.  These include:

1. [gputools](https://cran.r-project.org/web/packages/gputools/index.html)
2. [cudaBayesreg](https://cran.r-project.org/web/packages/cudaBayesreg/index.html)
3. [HiPLARM](https://cran.r-project.org/web/packages/HiPLARM/index.html)
4. [gmatrix](https://cran.r-project.org/web/packages/gmatrix/index.html)

There are currently only two OpenCL enabled packages

1. [OpenCL](https://cran.r-project.org/web/packages/OpenCL/index.html) - interface from R to OpenCL
2. [gpuR](https://cran.r-project.org/web/packages/gpuR/index.html) - general purpose library

**Warning** - installation can be difficult for different operating systems with different environmental variables and GPU platforms.  

## gpuR gpuMatrix objects
```r
library(gpuR)

# gpuMatrix objects
X <- gpuMatrix(rnorm(100), 10, 10)
Y <- gpuMatrix(rnorm(100), 10, 10)

# transfer data to GPU when operation called
# automatically copied back to CPU
Z <- X %*% Y

```

## gpuR vclMatrix objects
```r
library(gpuR)

# vclMatrix objects
X <- vclMatrix(rnorm(100), 10, 10)
Y <- vclMatrix(rnorm(100), 10, 10)

# data always on GPU
# no data transfer
Z <- X %*% Y
```

