---
title: "Wrapping C++"
slug: "wrapping-c++"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

## Wrapping a DLL: C++ to Cython to Python
This demonstrates a non-trivial example of wrapping a C++ dll with Cython. It will cover the following main steps:

 - Create an example DLL with C++ using Visual Studio.
 - Wrap the DLL with Cython so that it may be called in Python.

**It is assumed that you have Cython installed and can successfully import it in Python.**

For the DLL step, it is also assumed that you are familiar with creating a DLL in Visual Studio.

The full example includes the creation of the following files:

 1. `complexFunLib.h`: Header file for the C++ DLL source
 2. `complexFunLib.cpp`: CPP file for the C++ DLL source
 3. `ccomplexFunLib.pxd`: Cython "header" file
 4. `complexFunLib.pyx`: Cython "wrapper" file
 5. `setup.py`: Python setup file for creating `complexFunLib.pyd` with Cython 
 6. `run.py`: Example Python file that imports the compiled, Cython wrapped DLL

**C++ DLL Source: `complexFunLib.h` and `complexFunLib.cpp`**
-----------------------------------------
**Skip this if you already have a DLL and header source file.** First, we create the C++ source from which the DLL will be compiled using Visual Studio. In this case, we want to do fast array calculations with the complex exponential function. The following two functions perform the calculation `k*exp(ee)` on arrays `k` and `ee`, where the results are stored in `res`. There are two functions to accommodate both single and double precision. Note that these example functions use OpenMP, so make sure that OpenMP is enabled in the Visual Studio options for the project.

**H File**
<!-- language: lang-c++ -->
    // Avoids C++ name mangling with extern "C"
    #define EXTERN_DLL_EXPORT extern "C" __declspec(dllexport)  
    #include <complex>
    #include <stdlib.h>
    
    // Handles 64 bit complex numbers, i.e. two 32 bit (4 byte) floating point numbers
    EXTERN_DLL_EXPORT void mp_mlt_exp_c4(std::complex<float>* k, 
                                         std::complex<float>* ee,
                                         int sz, 
                                         std::complex<float>* res, 
                                         int threads);

    // Handles 128 bit complex numbers, i.e. two 64 bit (8 byte) floating point numbers
    EXTERN_DLL_EXPORT void mp_mlt_exp_c8(std::complex<double>* k,                                       std::complex<double>* ee,
                                         int sz, 
                                         std::complex<double>* res, 
                                         int threads);

**CPP File**
<!-- language: lang-c++ -->
    #include "stdafx.h"
    #include <stdio.h>
    #include <omp.h>
    #include "complexFunLib.h"

    void mp_mlt_exp_c4(std::complex<float>* k,
                       std::complex<float>* ee,
                       int sz,
                       std::complex<float>* res,
                       int threads)
    {
        // Use Open MP parallel directive for multiprocessing
        #pragma omp parallel num_threads(threads)
        {
            #pragma omp for
            for (int i = 0; i < sz; i++) res[i] = k[i] * exp(ee[i]);
        }
    }

    void mp_mlt_exp_c8(std::complex<double>* k,
                       std::complex<double>* ee,
                       int sz, std::complex<double>* res,
                       int threads)
    {
        // Use Open MP parallel directive for multiprocessing
        #pragma omp parallel num_threads(threads)
        {
            #pragma omp for
            for (int i = 0; i < sz; i++) res[i] = k[i] * exp(ee[i]);
        }
    }

**Cython Source: `ccomplexFunLib.pxd` and `complexFunLib.pyx`**
-----------------------------------------
Next, we create the Cython source files necessary to wrap the C++ DLL. In this step, we make the following assumptions:

 - You have installed Cython
 - You possess a working DLL, e.g. the one described above

The ultimate goal is to create use these Cython source files in conjunction with the original DLL to compile a `.pyd` file which may be imported as a Python module and exposes the functions written in C++.

**PXD File**

This file corresponds the C++ header file. In most cases, you may copy-paste the header over to this file with minor Cython specific changes. In this case, the specific Cython complex types were used. Note the addition of `c` at the beginning of `ccomplexFunLib.pxd`. This is not necessary, but we have found that such a naming convention helps maintain organization.

<!-- language: lang-cython -->
    cdef extern from "complexFunLib.h":
        void mp_mlt_exp_c4(float complex* k, float complex* ee, int sz,
                           float complex* res, int threads);
        void mp_mlt_exp_c8(double complex* k, double complex* ee, int sz,
                           double complex* res, int threads);

**PYX File**

This file corresponds to the C++ `cpp` source file. In this example, we will be passing pointers to Numpy `ndarray` objects to the import DLL functions. It is also possible to use the built in Cython `memoryview` object for arrays, but its performance may not be as good as `ndarray` objects (however the syntax is significantly cleaner).

<!-- language: lang-cython -->
    cimport ccomplexFunLib  # Import the pxd "header"
    # Note for Numpy imports, the C import most come AFTER the Python import
    import numpy as np  # Import the Python Numpy
    cimport numpy as np  # Import the C Numpy

    # Import some functionality from Python and the C stdlib
    from cpython.pycapsule cimport *

    # Python wrapper functions.
    # Note that types can be delcared in the signature

    def mp_exp_c4(np.ndarray[np.complex64_t, ndim=1] k,
                  np.ndarray[np.complex64_t, ndim=1] ee,
                  int sz,
                  np.ndarray[np.complex64_t, ndim=1] res,
                  int threads):
        '''
        TODO: Python docstring
        '''
        # Call the imported DLL functions on the parameters.
        # Notice that we are passing a pointer to the first element in each array
        ccomplexFunLib.mp_mlt_exp_c4(&k[0], &ee[0], sz, &res[0], threads)
        
    def mp_exp_c8(np.ndarray[np.complex128_t, ndim=1] k,
                  np.ndarray[np.complex128_t, ndim=1] ee,
                  int sz,
                  np.ndarray[np.complex128_t, ndim=1] res,
                  int threads):
        '''
        TODO: Python docstring
        '''
        ccomplexFunLib.mp_mlt_exp_c8(&k[0], &ee[0], sz, &res[0], threads)


**Python Source: `setup.py` and `run.py`**
-----------------------------------------
**setup.py**

This file is a Python file that executes the Cython compilation. Its purpose is to generate the compiled `.pyd` file that may then be imported by Python modules. In this example, we have kept all the required files (i.e. `complexFunLib.h`, `complexFunLib.dll`, `ccomplexFunLib.pxd`, and `complexFunLib.pyx`) in the same directory as `setup.py`.

Once this file is created, it should be run from the command line with parameters: `build_ext --inplace`

Once this file is executed, it should produce a `.pyd` file without raising any errors. Note that in some cases if there is a mistake the `.pyd` may be created but is invalid. Make sure that no errors were thrown in the execution of `setup.py` before using the generated `.pyd`.

<!-- language: lang-python -->
    from distutils.core import setup
    from distutils.extension import Extension
    from Cython.Distutils import build_ext
    import numpy as np
    
    ext_modules = [
        Extension('complexFunLib',
                  ['complexFunLib.pyx'],
                  # Note here that the C++ language was specified
                  # The default language is C
                  language="c++",  
                  libraries=['complexFunLib'],
                  library_dirs=['.'])
        ]

    setup(
        name = 'complexFunLib',
        cmdclass = {'build_ext': build_ext},
        ext_modules = ext_modules,
        include_dirs=[np.get_include()]  # This gets all the required Numpy core files
    )

**run.py**

Now `complexFunLib` may be imported directly into a Python module and the wrapped DLL functions called.
<!-- language: lang-python -->
    import complexFunLib
    import numpy as np
    
    # Create arrays of non-trivial complex numbers to be exponentiated,
    # i.e. res = k*exp(ee)
    k = np.ones(int(2.5e5), dtype='complex64')*1.1234 + np.complex64(1.1234j)
    ee = np.ones(int(2.5e5), dtype='complex64')*1.1234 + np.complex64(1.1234j) 
    sz = k.size  # Get size integer
    res = np.zeros(int(2.5e5), dtype='complex64')  # Create array for results

    # Call function
    complexFunLib.mp_exp_c4(k, ee, sz, res, 8)  

    # Print results
    print(res)

