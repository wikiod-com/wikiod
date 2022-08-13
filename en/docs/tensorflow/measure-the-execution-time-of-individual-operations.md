---
title: "Measure the execution time of individual operations"
slug: "measure-the-execution-time-of-individual-operations"
draft: false
images: []
weight: 9649
type: docs
toc: true
---

## Basic example with TensorFlow's Timeline object
The [**`Timeline`** object][2] allows you to get the execution time for each node in the graph:

  - you use a classic `sess.run()` but also specify the optional arguments `options` and `run_metadata`
  - you then create a `Timeline` object with the `run_metadata.step_stats` data

---

Here is an example program that measures the performance of a matrix multiplication:

<!-- language: lang-py -->

    import tensorflow as tf
    from tensorflow.python.client import timeline

    x = tf.random_normal([1000, 1000])
    y = tf.random_normal([1000, 1000])
    res = tf.matmul(x, y)

    # Run the graph with full trace option
    with tf.Session() as sess:
        run_options = tf.RunOptions(trace_level=tf.RunOptions.FULL_TRACE)
        run_metadata = tf.RunMetadata()
        sess.run(res, options=run_options, run_metadata=run_metadata)

        # Create the Timeline object, and write it to a json
        tl = timeline.Timeline(run_metadata.step_stats)
        ctf = tl.generate_chrome_trace_format()
        with open('timeline.json', 'w') as f:
            f.write(ctf)

---

You can then open Google Chrome, go to the page `chrome://tracing` and load the `timeline.json` file.
You should see something like:

[![timeline][1]][1]


  [1]: http://i.stack.imgur.com/qrPyb.png
  [2]: https://github.com/tensorflow/tensorflow/blob/master/tensorflow/python/client/timeline.py

