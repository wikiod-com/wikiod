---
title: "Consume data from Kafka"
slug: "consume-data-from-kafka"
draft: false
images: []
weight: 9884
type: docs
toc: true
---

## Built-in deserialization schemas
__SimpleStringSchema__: `SimpleStringSchema` deserializes the message as a string. In case your messages have keys, the latter will be ignored.

<!-- language: lang-java -->

    new FlinkKafkaConsumer09<>(kafkaInputTopic, new SimpleStringSchema(), prop);

__JSONDeserializationSchema__

`JSONDeserializationSchema` deserializes json-formatted messages using _jackson_ and returns a stream of `com.fasterxml.jackson.databind.node.ObjectNode` objects. You can then use the `.get("property")` method to access fields. Once again, keys are ignored.

<!-- language: lang-java -->

    new FlinkKafkaConsumer09<>(kafkaInputTopic, new JSONDeserializationSchema(), prop);


__JSONKeyValueDeserializationSchema__

`JSONKeyValueDeserializationSchema` is very similar to the previous one, but deals with messages with json-encoded keys AND values. 

<!-- language: lang-java -->

    boolean fetchMetadata = true;
    new FlinkKafkaConsumer09<>(kafkaInputTopic, new JSONKeyValueDeserializationSchema(fetchMetadata), properties);

The `ObjectNode` returned contains the following fields:
- `key`: all the fields present in the key
- `value`: all the message fields
- (optional) `metadata`:  exposes the `offset`, `partition` and `topic` of the message (pass `true` to the constructor in order to fetch metadata as well).

For example:

<!-- language: lang-bash -->

    kafka-console-producer --broker-list localhost:9092 --topic json-topic \
        --property parse.key=true \
        --property key.separator=|
    {"keyField1": 1, "keyField2": 2} | {"valueField1": 1, "valueField2" : {"foo": "bar"}}
    ^C

Will be decoded as:

<!-- language: lang-json -->

    {
        "key":{"keyField1":1,"keyField2":2},
        "value":{"valueField1":1,"valueField2":{"foo":"bar"}},
        "metadata":{
            "offset":43,
            "topic":"json-topic",
            "partition":0
        }
    }      



## Kafka partitions and Flink parallelism
In kafka, each consumer from the same consumer group gets assigned one or more partitions. Note that it is not possible for two consumers to consume from the same partition. The number of flink consumers depends on the flink parallelism (defaults to 1).

There are three possible cases:

1) __kafka partitions == flink parallelism__: this case is ideal, since each consumer takes care of one partition. If your messages are balanced between partitions, the work will be evenly spread across flink operators;
2) __kafka partitions < flink parallelism__: some flink instances won't receive any messages. To avoid that, you need to call `rebalance` on your input stream _before any operation_, which causes data to be re-partitioned:

    <!-- language: lang-java -->

       inputStream = env.addSource(new FlinkKafkaConsumer10("topic", new SimpleStringSchema(), properties));
    
       inputStream
           .rebalance()
           .map(s -> "message" + s)
           .print();
    
3. __kafka partitions > flink parallelism__: in this case, some instances will handle multiple partitions. Once again, you can use `rebalance` to spread messages evenly accross workers.

## KafkaConsumer example
`FlinkKafkaConsumer` let's you consume data from one or more kafka topics. 

## versions

The consumer to use depends on your kafka distribution. 

- `FlinkKafkaConsumer08`: uses the old `SimpleConsumer` API of Kafka. Offsets are handled by Flink and committed to zookeeper.
- `FlinkKafkaConsumer09`: uses the new Consumer API of Kafka, which handles offsets and rebalance automatically.
- `FlinkKafkaProducer010`: this connector supports Kafka messages with timestamps both for producing and consuming (useful for window operations).

## usage

The binaries are not part of flink core, so you need to import them:

    <dependency>
      <groupId>org.apache.flink</groupId>
      <artifactId>flink-connector-kafka-0.${kafka.version}_2.10</artifactId>
      <version>RELEASE</version>
    </dependency>

The constructor takes three arguments:
- one or more topic to read from
- a deserialization schema telling Flink how to interpret/decode the messages
- kafka consumer configuration properties. Those are the same as a "regular" kafka consumer. The minimum required are:
    + `bootstrap.servers`: a comma separated list of Kafka brokers in the form ip:port. For version 8, use `zookeeper.connect` (list of zookeeper servers) instead
    + `group.id`: the id of the consumer group (see kafka documentation for more details)

In Java:

<!-- language: lang-java -->

    Properties properties = new Properties();
    properties.put("group.id", "flink-kafka-example");
    properties.put("bootstrap.servers", "localhost:9092");
    
    DataStream<String> inputStream = env.addSource( 
            new FlinkKafkaConsumer09<>(
                kafkaInputTopic, new SimpleStringSchema(), properties));

In scala:

<!-- language: lang-scala -->

    val properties = new Properties();
    properties.setProperty("bootstrap.servers", "localhost:9092");
    properties.setProperty("group.id", "test");

    inputStream = env.addSource(
            new FlinkKafkaConsumer08[String](
                "topic", new SimpleStringSchema(), properties))

During development, you can use the kafka properties `enable.auto.commit=false` and `auto.offset.reset=earliest` to reconsume the same data everytime you launch your pogram.

## Fault tolerance
As explained in [the docs](https://ci.apache.org/projects/flink/flink-docs-release-1.3/dev/connectors/kafka.html),

> With Flink’s checkpointing enabled, the Flink Kafka Consumer will consume records from a topic and periodically checkpoint all its Kafka offsets, together with the state of other operations, in a consistent manner. In case of a job failure, Flink will restore the streaming program to the state of the latest checkpoint and re-consume the records from Kafka, starting from the offsets that where stored in the checkpoint.
>
> The interval of drawing checkpoints therefore defines how much the program may have to go back at most, in case of a failure.

To use fault tolerant Kafka Consumers, you need to enable checkpointing at the execution environment using the `enableCheckpointing` method:

<!-- language: lang-java -->

    final StreamExecutionEnvironment env = StreamExecutionEnvironment.getExecutionEnvironment();
    env.enableCheckpointing(5000); // checkpoint every 5 seconds

