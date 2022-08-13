---
title: "Prepare Data for Training"
slug: "prepare-data-for-training"
draft: false
images: []
weight: 9686
type: docs
toc: true
---

## Prepare image dataset for image classification task
Caffe has a build-in input layer tailored for image classification tasks (i.e., single integer label per input image). This input `"Data"` layer is built upon an [tag:lmdb] or [tag:leveldb] data structure. In order to use `"Data"` layer one has to construct the data structure with all training data.

<h2>A quick guide to Caffe's `convert_imageset`</h2>

<h3>Build</h3>
First thing you must do is build caffe and caffe's tools (`convert_imageset` is one of these tools).  
After installing caffe and `make`ing it make sure you ran `make tools` as well.  
Verify that a binary file `convert_imageset` is created in `$CAFFE_ROOT/build/tools`.

<h3>Prepare your data</h3>
*Images:* put all images in a folder (I'll call it here `/path/to/jpegs/`).  
*Labels:* create a text file (e.g., `/path/to/labels/train.txt`) with a line per input image <path/to/file> <label>. For example:  
> img_0000.jpeg 1  
img_0001.jpeg 0  
img_0002.jpeg 0  

In this example the first image is labeled `1` while the other two are labeled `0`. 

<h3>Convert the dataset</h3>
Run the binary in shell

    ~$ GLOG_logtostderr=1 $CAFFE_ROOT/build/tools/convert_imageset \
        --resize_height=200 --resize_width=200 --shuffle  \
        /path/to/jpegs/ \
        /path/to/labels/train.txt \
        /path/to/lmdb/train_lmdb

Command line explained:  

- `GLOG_logtostderr` flag is set to 1 *before* calling `convert_imageset` indicates the logging mechanism to redirect log messages to stderr.  
- `--resize_height` and `--resize_width` resize **all** input images to same size `200x200`.  
- `--shuffle` randomly change the order of images and does not preserve the order in the `/path/to/labels/train.txt` file.  
- Following are the path to the images folder, the labels text file and the output name. Note that the output name should not exist prior to calling `convert_imageset` otherwise you'll get a scary error message.

Other flags that might be useful:

- `--backend` - allows you to choose between an `lmdb` dataset or `levelDB`.
- `--gray`    - convert all images to gray scale.
- `--encoded` and `--encoded_type`  - keep image data in encoded (jpg/png) compressed form in the database.
- `--help`    - shows some help, see all relevant flags under *Flags from tools/convert_imageset.cpp*  


You can check out [`$CAFFE_ROOT/examples/imagenet/convert_imagenet.sh`][1]
for an example how to use `convert_imageset`.

 [1]: https://github.com/BVLC/caffe/blob/master/examples/imagenet/create_imagenet.sh 


see [this thread][2] for more information.

 [2]: http://stackoverflow.com/a/31431716/1714410

## Prepare arbitrary data in HDF5 format
In addition to [image classification datasets][1], Caffe also have `"HDF5Data"` layer for arbitrary inputs. This layer requires all training/validation data to be stored in [tag:hdf5] format files.  
This example shows how to use python `h5py` module to construct such hdf5 file and how to setup caffe `"HDF5Data"` layer to read that file.


<h3>Build the hdf5 binary file</h3>
Assuming you have a text file `'train.txt'` with each line with an image file name and a single floating point number to be used as regression target.

    import h5py, os
    import caffe
    import numpy as np

    SIZE = 224 # fixed size to all images
    with open( 'train.txt', 'r' ) as T :
        lines = T.readlines()
    # If you do not have enough memory split data into
    # multiple batches and generate multiple separate h5 files
    X = np.zeros( (len(lines), 3, SIZE, SIZE), dtype='f4' ) 
    y = np.zeros( (1,len(lines)), dtype='f4' )
    for i,l in enumerate(lines):
        sp = l.split(' ')
        img = caffe.io.load_image( sp[0] )
        img = caffe.io.resize( img, (SIZE, SIZE, 3) ) # resize to fixed size
        # you may apply other input transformations here...
        # Note that the transformation should take img from size-by-size-by-3 and transpose it to 3-by-size-by-size
        X[i] = img
        y[i] = float(sp[1])
    with h5py.File('train.h5','w') as H:
        H.create_dataset( 'X', data=X ) # note the name X given to the dataset!
        H.create_dataset( 'y', data=y ) # note the name y given to the dataset!
    with open('train_h5_list.txt','w') as L:
        L.write( 'train.h5' ) # list all h5 files you are going to use

<h3>Configuring `"HDF5Data"` layer</h3>
Once you have all `h5` files and the corresponding test files listing them you can add an HDF5 input layer to your `train_val.prototxt`:

     layer {
       type: "HDF5Data"
       top: "X" # same name as given in create_dataset!
       top: "y"
       hdf5_data_param {
         source: "train_h5_list.txt" # do not give the h5 files directly, but the list.
         batch_size: 32
       }
       include { phase:TRAIN }
     }


You can find more information [here][2] and [here][3].
___
As shown in above, we pass into Caffe a list of HDF5 files. That is because in the current version there's a size limit of 2GB for a single HDF5 data file. So if the training data exceeds 2GB, we'll need to split it into separate files.

If a single HDF5 data file exceeds 2GB we'll get an error message like 

    Check failed: shape[i] <= 2147483647 / count_ (100 vs. 71) blob size exceeds INT_MAX
---
If the total amount of data is less than 2GB, shall we split the data into separate files or not?

According to a piece of comment in [Caffe's source code][4], a single file would be better,

> If shuffle == true, the ordering of the HDF5 files is shuffled,
> and the ordering of data within any given HDF5 file is shuffled,
> but data between different files are not interleaved.


  [4]: https://github.com/BVLC/caffe/blob/master/src/caffe/proto/caffe.proto#L743-L755

 [1]: https://www.wikiod.com/caffe/prepare-data-for-training#Prepare image dataset for image classification task
 [2]: http://stackoverflow.com/a/31808324/1714410
 [3]: http://stackoverflow.com/a/33166461/1714410

