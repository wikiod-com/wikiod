---
title: "Minimalist example code for distributed Tensorflow."
slug: "minimalist-example-code-for-distributed-tensorflow"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

This document shows how to create a cluster of TensorFlow servers, and how to distribute a computation graph across that cluster.

## Distributed training example
 
    import tensorflow as tf

    FLAGS = None

    def main(_):
        ps_hosts = FLAGS.ps_hosts.split(",")
        worker_hosts = FLAGS.worker_hosts.split(",")

        # Create a cluster from the parameter server and worker hosts.
        cluster = tf.train.ClusterSpec({"ps": ps_hosts, "worker": worker_hosts})

        # Create and start a server for the local task.
        server = tf.train.Server(cluster, job_name=FLAGS.job_name, task_index=FLAGS.task_index)

        if FLAGS.job_name == "ps":
            server.join()
        elif FLAGS.job_name == "worker":

            # Assigns ops to the local worker by default.
            with tf.device(tf.train.replica_device_setter(worker_device="/job:worker/task:%d" % FLAGS.task_index, cluster=cluster)):

                # Build model...
                loss = ...
                global_step = tf.contrib.framework.get_or_create_global_step()

                train_op = tf.train.AdagradOptimizer(0.01).minimize(loss, global_step=global_step)

            # The StopAtStepHook handles stopping after running given steps.
            hooks=[tf.train.StopAtStepHook(last_step=1000000)]

            # The MonitoredTrainingSession takes care of session initialization,
            # restoring from a checkpoint, saving to a checkpoint, and closing when done
            # or an error occurs.
            with tf.train.MonitoredTrainingSession(master=server.target,
                                           is_chief=(FLAGS.task_index == 0),
                                           checkpoint_dir="/tmp/train_logs",
                                           hooks=hooks) as mon_sess:
                while not mon_sess.should_stop():
                    # Run a training step asynchronously.
                    # See `tf.train.SyncReplicasOptimizer` for additional details on how to perform *synchronous* training.
                   # mon_sess.run handles AbortedError in case of preempted PS.
                   mon_sess.run(train_op)

