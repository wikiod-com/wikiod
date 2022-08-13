---
title: "Save and Restore a Model in TensorFlow"
slug: "save-and-restore-a-model-in-tensorflow"
draft: false
images: []
weight: 9050
type: docs
toc: true
---

Tensorflow distinguishes between saving/restoring the current values of all the variables in a graph and saving/restoring the actual graph structure. To restore the graph, you are free to use either Tensorflow's functions or just call your piece of code again, that built the graph in the first place. When defining the graph, you should also think about which and how variables/ops should be retrievable once the graph has been saved and restored.

In the restoring model section above if I understand correctly you build the model and then restore the variables. I believe rebuilding the model is not necessary so long as you add the relevant tensors/placeholders when saving using `tf.add_to_collection()`. For example:

    tf.add_to_collection('cost_op', cost_op)

Then later you can restore the saved graph and get access to `cost_op` using

    with tf.Session() as sess:
        new_saver = tf.train.import_meta_graph('model.meta')` 
        new_saver.restore(sess, 'model')
        cost_op = tf.get_collection('cost_op')[0]

Even if you don't run `tf.add_to_collection()`, you can retrieve your tensors, but the process is a bit more cumbersome, and you may have to do some digging to find the right names for things. For example:

in a script that builds a tensorflow graph, we define some set of tensors `lab_squeeze`:
```
...
with tf.variable_scope("inputs"):
    y=tf.convert_to_tensor([[0,1],[1,0]])
    split_labels=tf.split(1,0,x,name='lab_split')
    split_labels=[tf.squeeze(i,name='lab_squeeze') for i in split_labels]
...
with tf.Session().as_default() as sess:
    saver=tf.train.Saver(sess,split_labels)
    saver.save("./checkpoint.chk")
    
```

we can recall them later on as follows: 

```
with tf.Session() as sess:
    g=tf.get_default_graph()
    new_saver = tf.train.import_meta_graph('./checkpoint.chk.meta')` 
    new_saver.restore(sess, './checkpoint.chk')
    split_labels=['inputs/lab_squeeze:0','inputs/lab_squeeze_1:0','inputs/lab_squeeze_2:0']

    split_label_0=g.get_tensor_by_name('inputs/lab_squeeze:0') 
    split_label_1=g.get_tensor_by_name("inputs/lab_squeeze_1:0")

```

There are a number of ways to find the name of a tensor -- you can find it in your graph on tensor board, or you can search through for it with something like: 

```
sess=tf.Session()
g=tf.get_default_graph()
...
x=g.get_collection_keys()
[i.name for j in x for i in g.get_collection(j)] # will list out most, if not all, tensors on the graph
```


## Saving the model
Saving a model in tensorflow is pretty easy.

Let's say you have a linear model with input `x` and want to predict an output `y`. The loss here is the mean square error (MSE). The batch size is 16.


<!-- language: lang-py -->

    # Define the model
    x = tf.placeholder(tf.float32, [16, 10])  # input
    y = tf.placeholder(tf.float32, [16, 1])   # output

    w = tf.Variable(tf.zeros([10, 1]), dtype=tf.float32)

    res = tf.matmul(x, w)
    loss = tf.reduce_sum(tf.square(res - y))

    train_op = tf.train.GradientDescentOptimizer(0.01).minimize(loss)

---

Here comes the Saver object, which can have multiple parameters (cf. [doc][1]).

<!-- language: lang-py -->

    # Define the tf.train.Saver object
    # (cf. params section for all the parameters)    
    saver = tf.train.Saver(max_to_keep=5, keep_checkpoint_every_n_hours=1)

---

Finally we train the model in a `tf.Session()`, for `1000` iterations. We only save the model every `100` iterations here.

<!-- language: lang-py -->

    # Start a session
    max_steps = 1000
    with tf.Session() as sess:
        # initialize the variables
        sess.run(tf.initialize_all_variables())

        for step in range(max_steps):
            feed_dict = {x: np.random.randn(16, 10), y: np.random.randn(16, 1)}  # dummy input
            _, loss_value = sess.run([train_op, loss], feed_dict=feed_dict)

            # Save the model every 100 iterations
            if step % 100 == 0:
                saver.save(sess, "./model", global_step=step)

---
After running this code, you should see the last 5 checkpoints in your directory:
- `model-500` and `model-500.meta`
- `model-600` and `model-600.meta`
- `model-700` and `model-700.meta`
- `model-800` and `model-800.meta`
- `model-900` and `model-900.meta`

Note that in this example, while the `saver` actually saves both the current values of the variables as a checkpoint and the structure of the graph (`*.meta`), no specific care was taken w.r.t how to retrieve e.g. the placeholders `x` and `y` once the model was restored. E.g. if the restoring is done anywhere else than this training script, it can be cumbersome to retrieve `x` and `y` from the restored graph (especially in more  complicated models). To avoid that, always give names to your variables / placeholders / ops or think about using `tf.collections` as shown in one of the remarks.


[1]: https://www.tensorflow.org/versions/r0.10/api_docs/python/state_ops.html#Saver

## Restoring the model
Restoring is also quite nice and easy. 

Here's a handy helper function:
<!-- language: python -->
    def restore_vars(saver, sess, chkpt_dir):
        """ Restore saved net, global score and step, and epsilons OR
        create checkpoint directory for later storage. """
        sess.run(tf.initialize_all_variables())

        checkpoint_dir = chkpt_dir 

        if not os.path.exists(checkpoint_dir):
            try:
                print("making checkpoint_dir")
                os.makedirs(checkpoint_dir)
                return False
            except OSError:
                raise

        path = tf.train.get_checkpoint_state(checkpoint_dir)
        print("path = ",path)
        if path is None:
            return False
        else:
            saver.restore(sess, path.model_checkpoint_path)
            return True


Main code:
<!-- language: python -->
    path_to_saved_model = './'
    max_steps = 1

    # Start a session
    with tf.Session() as sess:
    
        ... define the model here ...

        print("define the param saver")
        saver = tf.train.Saver(max_to_keep=5, keep_checkpoint_every_n_hours=1)

        # restore session if there is a saved checkpoint
        print("restoring model")
        restored = restore_vars(saver, sess, path_to_saved_model)
        print("model restored ",restored)

        # Now continue training if you so choose

        for step in range(max_steps):

            # do an update on the model (not needed)
            loss_value = sess.run([loss])
            # Now save the model
            saver.save(sess, "./model", global_step=step)

