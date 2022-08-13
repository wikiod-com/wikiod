---
title: "Save Tensorflow model in Python and load with Java"
slug: "save-tensorflow-model-in-python-and-load-with-java"
draft: false
images: []
weight: 9936
type: docs
toc: true
---

Building and especially training a model may be easiest done in Python so how to you load and use the trained model in Java?

The model can accept any number of inputs, so change the NUM_PREDICTIONS if you want to run more predictions than one. Realize that the Java is using JNI to call into the C++ tensorflow model, so you will see some info messages coming from the model when you run this.

## Create and save a model with Python
    import tensorflow as tf
    # good idea
    tf.reset_default_graph()
    
    # DO MODEL STUFF
    # Pretrained weighting of 2.0
    W = tf.get_variable('w', shape=[], initializer=tf.constant(2.0), dtype=tf.float32)
    # Model input x
    x = tf.placeholder(tf.float32, name='x')
    # Model output y = W*x
    y = tf.multiply(W, x, name='y')
    
    # DO SESSION STUFF
    sess = tf.Session()
    sess.run(tf.global_variables_initializer()) 
    
    # SAVE THE MODEL
    builder = tf.saved_model.builder.SavedModelBuilder("/tmp/model" )
    builder.add_meta_graph_and_variables(
      sess, 
      [tf.saved_model.tag_constants.SERVING]
    )
    builder.save()



## Load and use the model in Java.
    public static void main( String[] args ) throws IOException
    {
        // good idea to print the version number, 1.2.0 as of this writing
        System.out.println(TensorFlow.version());        
        final int NUM_PREDICTIONS = 1;

        // load the model Bundle
        try (SavedModelBundle b = SavedModelBundle.load("/tmp/model", "serve")) {

            // create the session from the Bundle
            Session sess = b.session();
            // create an input Tensor, value = 2.0f
            Tensor x = Tensor.create(
                new long[] {NUM_PREDICTIONS}, 
                FloatBuffer.wrap( new float[] {2.0f} ) 
            );
            
            // run the model and get the result, 4.0f.
            float[] y = sess.runner()
                .feed("x", x)
                .fetch("y")
                .run()
                .get(0)
                .copyTo(new float[NUM_PREDICTIONS]);

            // print out the result.
            System.out.println(y[0]);
        }                
    }


