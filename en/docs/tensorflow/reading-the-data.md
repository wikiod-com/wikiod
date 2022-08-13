---
title: "Reading the data"
slug: "reading-the-data"
draft: false
images: []
weight: 9703
type: docs
toc: true
---

## How to load images and labels from a TXT file
It has not been explained in the Tensorflow documentation how to load images and labels directly from a TXT file. The code below illustrates how I achieved it. However, it does not mean that is the best way to do it and that this way will help in further steps. 

For instance, I'm loading the labels in one single integer value {0,1} while the documentation uses a one-hot vector [0,1].

    # Learning how to import images and labels from a TXT file
    #
    # TXT file format
    #
    # path/to/imagefile_1 label_1
    # path/to/imagefile_2 label_2
    # ...                 ...
    #
    # where label_X is either {0,1}
    
    #Importing Libraries
    import os
    import tensorflow as tf
    import matplotlib.pyplot as plt
    from tensorflow.python.framework import ops
    from tensorflow.python.framework import dtypes
    
    #File containing the path to images and the labels [path/to/images label]
    filename = '/path/to/List.txt'
    
    #Lists where to store the paths and labels
    filenames = []
    labels = []
    
    #Reading file and extracting paths and labels
    with open(filename, 'r') as File:
        infoFile = File.readlines() #Reading all the lines from File
        for line in infoFile: #Reading line-by-line
            words = line.split() #Splitting lines in words using space character as separator
            filenames.append(words[0])
            labels.append(int(words[1]))
    
    NumFiles = len(filenames)
    
    #Converting filenames and labels into tensors
    tfilenames = ops.convert_to_tensor(filenames, dtype=dtypes.string)
    tlabels = ops.convert_to_tensor(labels, dtype=dtypes.int32)
    
    #Creating a queue which contains the list of files to read and the value of the labels
    filename_queue = tf.train.slice_input_producer([tfilenames, tlabels], num_epochs=10, shuffle=True, capacity=NumFiles)
    
    #Reading the image files and decoding them
    rawIm= tf.read_file(filename_queue[0])
    decodedIm = tf.image.decode_png(rawIm) # png or jpg decoder
    
    #Extracting the labels queue
    label_queue = filename_queue[1]
    
    #Initializing Global and Local Variables so we avoid warnings and errors
    init_op = tf.group(tf.local_variables_initializer() ,tf.global_variables_initializer())
    
    #Creating an InteractiveSession so we can run in iPython
    sess = tf.InteractiveSession()
    
    with sess.as_default():
        sess.run(init_op)
        
        # Start populating the filename queue.
        coord = tf.train.Coordinator()
        threads = tf.train.start_queue_runners(coord=coord)
    
        for i in range(NumFiles): #length of your filenames list
            nm, image, lb = sess.run([filename_queue[0], decodedIm, label_queue])
            
            print image.shape
            print nm
            print lb
            
            #Showing the current image
            plt.imshow(image)
            plt.show()
    
        coord.request_stop()
        coord.join(threads)

## Random shuffling the examples
To randomly shuffle the examples, you can use `tf.train.shuffle_batch` function instead of `tf.train.batch`, as follows:
```
parsed_batch = tf.train.shuffle_batch([serialized_example],
    batch_size=100, capacity=1000,
    min_after_dequeue=200)
```
`tf.train.shuffle_batch` (as well as `tf.train.batch`) creates a `tf.Queue` and keeps adding `serialized_examples` to it.

`capacity` measures how many elements can be stored in Queue in one time. Bigger capacity leads to bigger memory usage, but lower latency caused by threads waiting to fill it up.

`min_after_dequeue` is the minimum number of elements present in the queue after getting elements from it. The `shuffle_batch` queue is not shuffling elements perfectly uniformly - it is designed with huge data, not-fitting-memory one, in mind. Instead, it reads between `min_after_dequeue` and `capacity` elements, store them in memory and randomly chooses a batch of them. After that it enqueues some more elements, to keep its number between `min_after_dequeue` and `capacity`. Thus, the bigger value of `min_after_dequeue`, the more random elements are - the choice of `batch_size` elements is guaranteed to be taken from at least `min_after_dequeue` consecutive elements, but the bigger `capacity` has to be and the longer it takes to fill the queue initially.

## Reading data for n epochs with batching
Assume your data examples are already read to a python's variable and you would like to read it n times, in batches of given size:
```
import numpy as np
import tensorflow as tf
data = np.array([1, 2, 3, 4, 5])
n = 4
```
To merge data in batches, possibly with random shuffling, you can use `tf.train.batch` or `tf.train.batch_shuffle`, but you need to pass to it the tensor that would produce whole data n times:
```
limited_tensor = tf.train.limit_epochs(data, n)
batch = tf.train.shuffle_batch([limited_tensor], batch_size=3, enqueue_many=True, capacity=4)
```
The `limit_epochs` converts the numpy array to tensor under the hood and returns a tensor producing it n times and throwing an OutOfRangeError afterwards. 
The `enqueue_many=True` argument passed to `shuffle_batch` denotes that each tensor in the tensor list `[limited_tensor]` should be interpreted as containing a number of examples. Note that capacity of the batching queue can be smaller than the number of examples in the tensor.

One can process the data as usual:
```
with tf.Session() as sess:
  sess.run(tf.initialize_local_variables())
  tf.train.start_queue_runners()
  try:
    while True:
      data_batch = sess.run(batch)
      # process data
  except tf.errors.OutOfRangeError:
    pass
```

## Count examples in CSV file
```
import tensorflow as tf
filename_queue = tf.train.string_input_producer(["file.csv"], num_epochs=1)
reader = tf.TextLineReader()
key, value = reader.read(filename_queue)
col1, col2 = tf.decode_csv(value, record_defaults=[[0], [0]])

with tf.Session() as sess:
  sess.run(tf.initialize_local_variables())
  tf.train.start_queue_runners()
  num_examples = 0
  try:
    while True:
      c1, c2 = sess.run([col1, col2])
      num_examples += 1
  except tf.errors.OutOfRangeError:
    print "There are", num_examples, "examples"
```
`num_epochs=1` makes `string_input_producer` queue to close after processing each file on the list once. It leads to raising `OutOfRangeError` which is caught in `try:`. By default, `string_input_producer` produces the filenames infinitely.

`tf.initialize_local_variables()` is a tensorflow Op, which, when executed, initializes `num_epoch` _local_ variable inside `string_input_producer`.

`tf.train.start_queue_runners()` start extra treads that handle adding data to the queues asynchronically.

## Read & Parse TFRecord file
[TFRecord files](https://www.tensorflow.org/versions/r0.10/api_docs/python/python_io.html#tfrecords-format-details) is the native tensorflow binary format for storing data (tensors). To read the file you can use a code similar to the CSV example:
```
import tensorflow as tf
filename_queue = tf.train.string_input_producer(["file.tfrecord"], num_epochs=1)
reader = tf.TFRecordReader()
key, serialized_example = reader.read(filename_queue)
```
Then, you need to parse the examples from `serialized_example` Queue. You can do it either using `tf.parse_example`, which requires previous batching, but is [faster](https://github.com/tensorflow/tensorflow/issues/390) or `tf.parse_single_example`:
```
batch = tf.train.batch([serialized_example], batch_size=100)
parsed_batch = tf.parse_example(batch, features={
  "feature_name_1": tf.FixedLenFeature(shape=[1], tf.int64),
  "feature_name_2": tf.FixedLenFeature(shape=[1], tf.float32)
})
```
`tf.train.batch` joins consecutive values of given tensors of shape `[x, y, z]` to tensors of shape `[batch_size, x, y, z]`.
`features` dict maps names of the features to tensorflow's definitions of [features](https://www.tensorflow.org/versions/r0.10/api_docs/python/io_ops.html#FixedLenFeature). You use `parse_single_example` in a similar way:
```
parsed_example = tf.parse_single_example(serialized_example, {
  "feature_name_1": tf.FixedLenFeature(shape=[1], tf.int64),
  "feature_name_2": tf.FixedLenFeature(shape=[1], tf.float32)
})
```
`tf.parse_example` and `tf.parse_single_example` return a dictionary mapping feature names to the tensor with the values.

To batch examples coming from `parse_single_example` you should extract the tensors from the dict and use `tf.train.batch` as before:
```
parsed_batch = dict(zip(parsed_example.keys(),
    tf.train.batch(parsed_example.values(), batch_size=100)
```

You read the data as before, passing the list of all the tensors to evaluate to `sess.run`:
```
with tf.Session() as sess:
  sess.run(tf.initialize_local_variables())
  tf.train.start_queue_runners()
  try:
    while True:
      data_batch = sess.run(parsed_batch.values())
      # process data
  except tf.errors.OutOfRangeError:
    pass
```

