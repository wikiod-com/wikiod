---
title: "Savepoints and externalized checkpoints"
slug: "savepoints-and-externalized-checkpoints"
draft: false
images: []
weight: 9899
type: docs
toc: true
---

Savepoints are _"fat", externally stored checkpoints_ that allow us to resume a stateful flink program after a permanent failure, a cancelation or a code update. Before Flink 1.2 and the introduction of _externalized checkpoints_, savepoints needed to be triggered explicitly. 

## Externalized checkpoints (Flink 1.2+)

Before 1.2, the only way to persist state/retain a checkpoint after a job termination/cancellation/persistant failure was through a savepoint, which is triggered manually. Version 1.2 introduced persistent checkpoints.

Persistent checkpoints behave very much like regular periodic checkpoints except the following differences:

1. They persist their meta data into a persistant storage (like savepoints).
2. They are not discarded when the owning job fails permanently. Furthermore, they can be configured to not be discarded when the job is cancelled.

It is thus very similar to savepoints; in fact, savepoints are just externalized checkpoints with a bit more information.

_Important note_: At the moment, Flink's checkpoint coordinator only retains the last successfully completed checkpoint. This means that whenever a new checkpoint completes then the last completed checkpoint will be discarded. This also applies to externalized checkpoints.

## Configuration

Where the metadata about [externalized] checkpoints are stored is configured in `flink-conf.yaml` (and cannot be overriden through code):

<!-- language: lang-bash -->
    # path to the externalized checkpoints
    state.checkpoints.dir: file:///tmp/flink-backend/ext-checkpoints

Note that this directory _only contains the checkpoint metadata_ required to restore the checkpoint. The actual checkpoint files are still stored in their configured directory (i.e. `state.bachend.fs.checkpointdir` property).

## Usage

You need to explicitly enable external checkpoints in the code using the `getCheckpointConfig()` method of the streaming environment:

<!-- language: lang-java -->
    StreamExecutionEnvironment env = StreamExecutionEnvironment.getExecutionEnvironment();   
    // enable regular checkpoints
    env.enableCheckpointing(5000); // every 5 sec.
    // enable externalized checkpoints
    env.getCheckpointConfig()
        .enableExternalizedCheckpoints(CheckpointConfig.ExternalizedCheckpointCleanup.RETAIN_ON_CANCELLATION);

The available `ExternalizedCheckpointCleanup` modes are:

- `RETAIN_ON_CANCELLATION`: the last checkpoint and its metadata are kept on job cancellation; it is your responsibility to clean up afterwards.
- `DELETE_ON_CANCELLATION`: the last checkpoint is deleted upon cancellation, meaning it is only available if the application fails.

To resume from an externalized checkpoint, use the savepoint syntax. For example:

    flink run -s /tmp/flink-backend/ext-checkpoints/savepoint-02d0cf7e02ea app.jar


## Savepoints: requirements and preliminary notes
A savepoint stores two things: (a) the positions of all datasources, (b) the states of operators. Savepoints are useful in many circonstances:

* slight application code updates
* Flink update
* changes in parallelism 
* ...



As of __version 1.3__ (also valid for earlier version):

-  checkpoint __must be enabled__ for the savepoints to be possible. If you forget to explicitly enable checkpoint using:

    <!-- language: lang-java -->
       StreamExecutionEnvironment env = StreamExecutionEnvironment.getExecutionEnvironment();
       env.enableCheckpointing(checkpointInterval);

    you will get:

       java.lang.IllegalStateException: Checkpointing disabled. You can enable it via the execution environment of your job
 
-  when using window operations, it is crucial to use event-time (vs ingestion or processing time) to yield proper results;       
-  to be able to upgrade a program and reuse savepoints, __manual uid must be set__. This is because, by default, Flink changes the operator's UID after any change in their code;
-  Chained operators are identified by the ID of the first task. It’s not possible to manually assign an ID to an intermediate chained task, e.g. in the chain [ a -> b -> c ] only a can have its ID assigned manually, but not b or c. To work around this, you can manually define the task chains. If you rely on the automatic ID assignment, a change in the chaining behaviour will also change the IDs (see point above).

More info is available [in the FAQ](https://ci.apache.org/projects/flink/flink-docs-release-1.3/setup/savepoints.html#faq).


## Savepoints
## Configuration

The configuration is in the file `flink/conf/flink-conf.yaml` (under Mac OSX via homebrew, it is `/usr/local/Cellar/apache-flink/1.1.3/libexec/conf/flink-conf.yaml`).

__Flink < 1.2__: 
The configuration is very similar to the checkpoints configuration (topic available). The only difference is that it makes no sense to define an in-memory savepoint backend, since we need the savepoints to persist after Flink's shutdown.


<!-- language: lang-bash -->
    # Supported backends: filesystem, <class-name-of-factory>
    savepoints.state.backend: filesystem

<!-- language: lang-bash -->
    # Use "hdfs://" for HDFS setups, "file://" for UNIX/POSIX-compliant file systems,
    # (or any local file system under Windows), or "S3://" for S3 file system.
    # Note: must be accessible from the JobManager and all TaskManagers !
    savepoints.state.backend.fs.checkpointdir: file:///tmp/flink-backend/savepoints

_Note_: If you don't specify a backend, the default backend is _jobmanager_, meaning that your savepoints will disappear once the cluster is shutdown. This is useful for debug only.

__Flink 1.2+__: as explained in [this jira ticket](https://issues.apache.org/jira/browse/FLINK-4507), allowing a savepoint to be saved in the jobmanager's memory makes little sense. Since Flink 1.2, savepoints are necessarily stored into files. The above configuration has been replaced by:

<!-- language: lang-bash -->
    # Default savepoint target directory
    state.savepoints.dir: hdfs:///flink/savepoints


## Usage

__Getting the job ID__

To trigger a savepoint, all you need is the job ID of the application. The job ID is printed in the command line when you launch the job or can be retrieved later using `flink list`:

    flink list
    Retrieving JobManager.
    Using address localhost/127.0.0.1:6123 to connect to JobManager.
    ------------------ Running/Restarting Jobs -------------------
    17.03.2017 11:44:03 : 196b8ce6788d0554f524ba747c4ea54f : CheckpointExample (RUNNING)
    --------------------------------------------------------------
    No scheduled jobs.

__Triggering a savepoint__

To trigger a savepoint, use `flink savepoint <jobID>`:

    flink savepoint 196b8ce6788d0554f524ba747c4ea54f
    Retrieving JobManager.
    Using address /127.0.0.1:6123 to connect to JobManager.
    Triggering savepoint for job 196b8ce6788d0554f524ba747c4ea54f.
    Waiting for response...
    Savepoint completed. Path: file:/tmp/flink-backend/savepoints/savepoint-a40111f915fc
    You can resume your program from this savepoint with the run command.

Note that you can also provide a target directory as a second argument, it will override the default one defined in `flink/bin/flink-conf.yaml`.

In Flink 1.2+, it is also possible to cancel a job AND do a savepoint at the same time, using the `-s` option: 

<!-- language: lang-bash -->
    flink cancel -s 196b8ce6788d0554f524ba747c4ea54f # use default savepoints dir
    flink cancel -s hdfs:///savepoints 196b8ce6788d0554f524ba747c4ea54f # specify target dir

_Note_: it is possible to move a savepoint, but do not rename it !

__Resuming from a savepoint__

To resume from a specific savepoint, use the `-s [savepoint-dir]` option of the `flink run` command:

    flink run -s /tmp/flink-backend/savepoints/savepoint-a40111f915fc app.jar

## Specifying operator UID

To be able to resume from a savepoint after a code change, you must ensure that the new code uses the same UID for operator. To manually assign a UID, call the `.uid(<name>)` fonction right after the operator:

<!-- language: lang-java -->
    env
        .addSource(source)
        .uid(className + "-KafkaSource01")
        .rebalance()
        .keyBy((node) -> node.get("key").asInt())
        .flatMap(new StatefulMapper())
        .uid(className + "-StatefulMapper01")
        .print();

