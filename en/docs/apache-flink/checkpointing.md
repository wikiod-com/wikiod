---
title: "Checkpointing"
slug: "checkpointing"
draft: false
images: []
weight: 9926
type: docs
toc: true
---

(tested on Flink 1.2 and below) 

Every function, source or operator in Flink can be stateful. 
Checkpoints allow Flink to recover state and positions in the streams to give the application the same semantics as a failure-free execution. It is the mecanism behind the guarantees of _fault tolerance_ and _exactly-once_ processing.

Read [this article](https://ci.apache.org/projects/flink/flink-docs-release-1.2/internals/stream_checkpointing.html) to understand the internals.

Checkpoints are only useful when a failure happens in the cluster, for example when a taskmanager fails. They do not persist after the job itself failed or was canceled. 

To be able to resume a stateful job after failure/cancellation, have a look at __savepoints__ or __externalized checkpoints (flink 1.2+)__.

## Configuration and setup
Checkpointing configuration is done in two steps. First, you need to choose a _backend_. Then, you can specify the interval and mode of the checkpoints in a per-application basis.

--------

# Backends

__Available backends__ 

Where the checkpoints are stored depends on the configured backend:

- `MemoryStateBackend`: in-memory state, backup to JobManager’s/ZooKeeper’s memory. Should be used only for minimal state (default to max. 5 MB, for storing Kafka offsets for example) or testing and local debugging.
- `FsStateBackend`: the state is kept in-memory on the TaskManagers, and state snapshots (i.e. checkpoints) are stored in a file system (HDFS, DS3, local filesystem, ...). This setup is encouraged for large states or long windows and for high availability setups.
- `RocksDBStateBackend`: holds in-flight data in a RocksDB database that is (per default) stored in the TaskManager data directories. Upon checkpointing, the whole RocksDB database is written to a file (like above). Compared to the FsStateBackend, it allows for larger states (limited only by the disk space vs the size of the taskmanager memory), but the throughput will be lower (data not always in memory, must be loaded from disc).  

Note that whatever the backend, metadata (number of checkpoints, localisation, etc.) are always stored in the jobmanager memory and checkpoints __won't persist after the application termination/cancellation__.

__Specifying the backend__

You specify the backend in your program's `main` method using:

<!-- language: lang-java --> 

    StreamExecutionEnvironment env = StreamExecutionEnvironment.getExecutionEnvironment();
    env.setStateBackend(new FsStateBackend("hdfs://namenode:40010/flink/checkpoints"));

Or set the default backend in `flink/conf/flink-conf.yaml`:

<!-- language: lang-bash -->
    # Supported backends: 
    #  - jobmanager (MemoryStateBackend), 
    #  - filesystem (FsStateBackend), 
    #  - rocksdb (RocksDBStateBackend), 
    #  - <class-name-of-factory>
    state.backend: filesystem
        
    # Directory for storing checkpoints in a Flink-supported filesystem
    # Note: State backend must be accessible from the JobManager and all TaskManagers.
    # Use "hdfs://" for HDFS setups, "file://" for UNIX/POSIX-compliant file systems, 
    # "S3://" for S3 file system.
    state.backend.fs.checkpointdir: file:///tmp/flink-backend/checkpoints

----------

# Enabling checkpoints

Every application need to explicitly enable checkpoints:

 <!-- language: lang-java --> 

    long checkpointInterval = 5000; // every 5 seconds

    StreamExecutionEnvironment env = StreamExecutionEnvironment.getExecutionEnvironment();
    env.enableCheckpointing(checkpointInterval);

You can optionally specify a _checkpoint mode_. If not, it default to _exactly once_:

 <!-- language: lang-java --> 
    env.enableCheckpointing(checkpointInterval, CheckpointingMode.AT_LEAST_ONCE);

The checkpointing mode defines what consistency guarantees the system gives in the presence of failures. When checkpointing is activated, the data streams are replayed such that lost parts of the processing are repeated. With `EXACTLY_ONCE`, the system draws checkpoints such that a recovery behaves as if the operators/functions see each record "exactly once". With `AT_LEAST_ONCE`, the checkpoints are drawn in a simpler fashion that typically encounters some duplicates upon recovery.

## Testing checkpoints
## The code

Here is a simple flink application using a stateful mapper with an `Integer` managed state. You can play with the `checkpointEnable`, `checkpointInterval` and `checkpointMode` variables to see their effect:

<!-- language: lang-java -->

    public class CheckpointExample {
    
        private static Logger LOG = LoggerFactory.getLogger(CheckpointExample.class);
        private static final String KAFKA_BROKER = "localhost:9092";
        private static final String KAFKA_INPUT_TOPIC = "input-topic";
        private static final String KAFKA_GROUP_ID = "flink-stackoverflow-checkpointer";
        private static final String CLASS_NAME = CheckpointExample.class.getSimpleName();
    
    
        public static void main(String[] args) throws Exception {
    
            // play with them
            boolean checkpointEnable = false;
            long checkpointInterval = 1000;
            CheckpointingMode checkpointMode = CheckpointingMode.EXACTLY_ONCE;
    
            // ----------------------------------------------------
    
            LOG.info(CLASS_NAME + ": starting...");
            final StreamExecutionEnvironment env = StreamExecutionEnvironment.getExecutionEnvironment();
    
            // kafka source
            // https://ci.apache.org/projects/flink/flink-docs-release-1.2/dev/connectors/kafka.html#kafka-consumer
            Properties prop = new Properties();
            prop.put("bootstrap.servers", KAFKA_BROKER);
            prop.put("group.id", KAFKA_GROUP_ID);
            prop.put("auto.offset.reset", "latest");
            prop.put("enable.auto.commit", "false");
    
            FlinkKafkaConsumer09<String> source = new FlinkKafkaConsumer09<>(
                    KAFKA_INPUT_TOPIC, new SimpleStringSchema(), prop);
    
            // checkpoints
            // internals: https://ci.apache.org/projects/flink/flink-docs-master/internals/stream_checkpointing.html#checkpointing
            // config: https://ci.apache.org/projects/flink/flink-docs-release-1.3/dev/stream/checkpointing.html
            if (checkpointEnable) env.enableCheckpointing(checkpointInterval, checkpointMode);
    
            env
                    .addSource(source)
                    .keyBy((any) -> 1)
                    .flatMap(new StatefulMapper())
                    .print();
    
            env.execute(CLASS_NAME);
        }
    
            /* *****************************************************************
             * Stateful mapper
             * (cf. https://ci.apache.org/projects/flink/flink-docs-release-1.3/dev/stream/state.html)
             * ****************************************************************/
    
        public static class StatefulMapper extends RichFlatMapFunction<String, String> {
            private transient ValueState<Integer> state;
    
            @Override
            public void flatMap(String record, Collector<String> collector) throws Exception {
                // access the state value
                Integer currentState = state.value();
    
                // update the counts
                currentState += 1;
                collector.collect(String.format("%s: (%s,%d)",
                        LocalDateTime.now().format(ISO_LOCAL_DATE_TIME), record, currentState));
                // update the state
                state.update(currentState);
            }
    
            @Override
            public void open(Configuration parameters) throws Exception {
                ValueStateDescriptor<Integer> descriptor =
                        new ValueStateDescriptor<>("CheckpointExample", TypeInformation.of(Integer.class), 0);
                state = getRuntimeContext().getState(descriptor);
            }
        }
    }

## Running the example and simulating failure

To be able to check the checkpoints, you need to start a `cluster`. The easier way is to use the `start-cluster.sh` script in the `flink/bin` directory:

    start-cluster.sh
    Starting cluster.
    [INFO] 1 instance(s) of jobmanager are already running on virusnest.
    Starting jobmanager daemon on host virusnest.
    Password:
    Starting taskmanager daemon on host virusnest.

Now, package your app and submit it to flink:

    mvn clean package
    flink run target/flink-checkpoints-test.jar -c CheckpointExample

Create some data:

    kafka-console-producer --broker-list localhost:9092 --topic input-topic 
    a
    b
    c
    ^D

The output should be available in `flink/logs/flink-<user>-jobmanager-0-<host>.out`. For example:

    tail -f flink/logs/flink-Derlin-jobmanager-0-virusnest.out
    2017-03-17T08:21:51.249: (a,1)
    2017-03-17T08:21:51.545: (b,2)
    2017-03-17T08:21:52.363: (c,3)


To test the checkpoints, simply kill the taskmanager (this will emulate a failure), produce some data and start a new one:

    # killing the taskmanager
    ps -ef | grep -i taskmanager
    kill <taskmanager PID>

    # starting a new taskmanager
    flink/bin/taskmanager.sh start


__Note:__ when starting a new taskmanager, it will use another log file, namely  `flink/logs/flink-<user>-jobmanager-1-<host>.out` (notice the integer increment).

## What to expect

* *checkpoints disabled*: if you produce data during the failure, they will be definitely lost. But surprisingly enough, the counters will be right !
* *checkpoints enabled*: no data loss anymore (and correct counters). 
* *checkpoints with at-least-once mode*: you may see duplicates, especially if you set a checkpoint interval to a high number and kill the taskmanager multiple times 

