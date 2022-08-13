---
title: "Cython bundling"
slug: "cython-bundling"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Bundling a Cython program using pyinstaller
Start from a Cython program with a entrypoint:

    def do_stuff():
        cdef int a,b,c
        a = 1
        b = 2
        c = 3
        print("Hello World!")
        print([a,b,c])
        input("Press Enter to continue.")


Create a `setup.py` file in the same folder:

    from distutils.core import setup
    from Cython.Build import cythonize
    setup(
        name = "Hello World",
        ext_modules = cythonize('program.pyx'), 
    )

Running it with `python setup.py build_ext --inplace` will produce a `.pyd` library in a subfolder.

After that, create a vanilla Python script using the library (e.g., `main.py`) and put the `.pyd` file beside it:

    import program
    program.do_stuff()

Use PyInstaller to bundle it `pyinstaller --onefile "main.py"`. This will create a subfolder containing the executable of a 4 MB+ size containing the library plus the python runtime.

## Automating build (Windows)
For automation of the above procedure in Windows use a `.bat` of the similar contents:

    del "main.exe"
    python setup.py build_ext --inplace
    del "*.c"
    rmdir /s /q ".\build"
    pyinstaller --onefile "main.py"
    copy /y ".\dist\main.exe" ".\main.exe"
    rmdir /s /q ".\dist"
    rmdir /s /q ".\build"
    del "*.spec"
    del "*.pyd"


## Adding Numpy to the bundle
To add Numpy to the bundle, modify the `setup.py` with `include_dirs` keyword and necessary import the numpy in the wrapper Python script to notify Pyinstaller.

`program.pyx`:

    import numpy as np
    cimport numpy as np


    def do_stuff():
        print("Hello World!")
        cdef int n
        n = 2
        r = np.random.randint(1,5)
        print("A random number: "+str(r))
        print("A random number multiplied by 2 (made by cdef):"+str(r*n))
        input("Press Enter to continue.")

`setup.py`:

    from distutils.core import setup, Extension
    from Cython.Build import cythonize
    import numpy
    
    setup(
        ext_modules=cythonize("hello.pyx"),
        include_dirs=[numpy.get_include()]
    )

`main.py`:

    import program
    import numpy
    program.do_stuff()

