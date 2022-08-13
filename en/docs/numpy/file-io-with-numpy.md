---
title: "File IO with numpy"
slug: "file-io-with-numpy"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Loading numerical data from text files with consistent structure
The function `np.loadtxt` can be used to read csv-like files:

    # File:
    #    # Col_1 Col_2
    #    1, 1
    #    2, 4
    #    3, 9
    np.loadtxt('/path/to/dir/csvlike.txt', delimiter=',', comments='#')
    # Output:
    # array([[ 1.,  1.],
    #        [ 2.,  4.],
    #        [ 3.,  9.]])

The same file could be read using a regular expression with `np.fromregex`:

    np.fromregex('/path/to/dir/csvlike.txt', r'(\d+),\s(\d+)', np.int64)
    # Output:
    # array([[1, 1],
    #        [2, 4],
    #        [3, 9]])



## Saving and loading numpy arrays using binary files
    x = np.random.random([100,100])
    x.tofile('/path/to/dir/saved_binary.npy')
    y = fromfile('/path/to/dir/saved_binary.npy')
    z = y.reshape(100,100)
    all(x==z)
    # Output:
    #     True

## Saving data as CSV style ASCII file
Analog to `np.loadtxt`, `np.savetxt` can be used to save data in an ASCII file

    import numpy as np
    x = np.random.random([100,100])
    np.savetxt("filename.txt", x)

To control formatting:

    np.savetxt("filename.txt", x, delimiter=", " , 
        newline="\n", comments="$ ", fmt="%1.2f",
        header="commented example text")

Output:

    $ commented example text
    0.30, 0.61, 0.34, 0.13, 0.52, 0.62, 0.35, 0.87, 0.48, [...]

## Reading CSV files
Three main functions available (description from man pages):

>[`fromfile`][1] - A highly efficient way of reading binary data with a known data-type, as well as parsing simply formatted text files.
> Data written using the tofile method can be read using this function.

>[`genfromtxt`][1] - Load data from a text file, with missing values handled as specified. Each line past the first skip_header
> lines is split at the delimiter character, and characters following
> the comments character are discarded.

> [`loadtxt`][2] - Load data from a text file. Each row in the text file must
> have the same number of values.

`genfromtxt` is a wrapper function for `loadtxt`. `genfromtxt` is the most straight-forward to use as it has many parameters for dealing with the input file.

[**Consistent number of columns, consistent data type (numerical or string):**][3]

Given an input file, `myfile.csv` with the contents:
 
    #descriptive text line to skip
    1.0, 2, 3
    4, 5.5, 6

    import numpy as np
    np.genfromtxt('path/to/myfile.csv',delimiter=',',skiprows=1)

gives an array:

    array([[ 1. ,  2. ,  3. ],
           [ 4. ,  5.5,  6. ]])


[**Consistent number of columns, mixed data type (across columns):**][4]

    1   2.0000  buckle_my_shoe
    3   4.0000  margery_door

    import numpy as np
    np.genfromtxt('filename', dtype= None)
    

    array([(1, 2.0, 'buckle_my_shoe'), (3, 4.0, 'margery_door')], 
    dtype=[('f0', '<i4'), ('f1', '<f8'), ('f2', '|S14')])

Note the use of `dtype=None` results in a recarray.

**Inconsistent number of columns:**

file:
     1 2 3 4 5
     6 7 8 9 10
     11 22
     13 14 15 16 17
     18 19 20 21 22
     23 24

[Into single row array:][5]

    result=np.fromfile(path_to_file,dtype=float,sep="\t",count=-1)


  [1]: http://docs.scipy.org/doc/numpy/reference/generated/numpy.genfromtxt.html#numpy.genfromtxt
  [2]: http://docs.scipy.org/doc/numpy/reference/generated/numpy.loadtxt.html#numpy.loadtxt
  [3]: http://stackoverflow.com/a/26296194/1461850
  [4]: http://stackoverflow.com/a/15481761/1461850
  [5]: http://stackoverflow.com/a/12902173/1461850



