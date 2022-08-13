---
title: "ProducerConsumer in Java"
slug: "producerconsumer-in-java"
draft: false
images: []
weight: 9888
type: docs
toc: true
---

This topic shows how to produce and consume records in Java.

## SimpleConsumer (Kafka >= 0.9.0)
The 0.9 release of Kafka introduced a complete redesign of the kafka consumer. 
If you are interested in the old `SimpleConsumer` (0.8.X), have a look at [this page](https://cwiki.apache.org/confluence/display/KAFKA/0.8.0+SimpleConsumer+Example).  If your Kafka installation is newer than 0.8.X, the following codes should work out of the box.

# Configuration and initialization

> Kafka 0.9 no longer supports Java 6 or Scala 2.9. If you are still on Java 6, consider upgrading to a supported version.

First, create a maven project and add the following dependency in your pom:

<!-- language: lang-xml -->
    <dependencies>
        <dependency>
            <groupId>org.apache.kafka</groupId>
            <artifactId>kafka-clients</artifactId>
            <version>0.9.0.1</version>
        </dependency>
    </dependencies>

__Note__ : don't forget to update the version field for the latest releases (now > 0.10).

The consumer is initialised using a `Properties` object. There are lots of properties allowing you to fine-tune the consumer behaviour. Below is the minimal configuration needed:

<!-- language: lang-java -->

    Properties props = new Properties();
    props.put("bootstrap.servers", "localhost:9092");
    props.put("group.id", "consumer-tutorial");
    props.put("key.deserializer", StringDeserializer.class.getName());
    props.put("value.deserializer", StringDeserializer.class.getName()); 

The `bootstrap-servers` is an initial list of brokers for the consumer to be able discover the rest of the cluster. This doesn’t need to be all the servers in the cluster: the client will determine the full set of alive brokers from the brokers in this list.  

The `deserializer` tells the consumer how to interpret/deserialize the message keys and values. Here, we use the built-in `StringDeserializer`. 

Finally, the `group.id` corresponds to the consumer group of this client. Remember: all consumers of a consumer group will split messages between them (kafka acting like a message queue), while consumers from different consumer groups will get the same messages (kafka acting like a publish-subscribe system).

Other useful properties are:

 - `auto.offset.reset`: controls what to do if the offset stored in Zookeeper is either missing or out-of-range. Possible values are `latest` and `earliest`. Anything else will throw an exception;
 - `enable.auto.commit`:  if `true` (default), the consumer offset is periodically (see `auto.commit.interval.ms`) saved in the background. Setting it to `false` and using `auto.offset.reset=earliest` - is to determine where should the consumer start from in case no committed offset information is found. `earliest` means from the start of the assigned topic partition. `latest` means from the highest number of available committed offset for the partition. However, Kafka consumer will always resume from the last committed offset as long as a valid offset record is found (i.e. ignoring `auto.offset.reset`. The best example is when a brand new consumer group subscribes to a topic. This is when it uses `auto.offset.reset` to determine whether to start from the beginning (earliest) or the end (latest) of the topic.
 - `session.timeout.ms`: a session timeout ensures that the lock will be released if the consumer crashes or if a network partition isolates the consumer from the coordinator. Indeed:
 
    > When part of a consumer group, each consumer is assigned a subset of the partitions from topics it has subscribed to. This is basically a group lock on those partitions. As long as the lock is held, no other members in the group will be able to read from them. When your consumer is healthy, this is exactly what you want. It’s the only way that you can avoid duplicate consumption. But if the consumer dies due to a machine or application failure, you need that lock to be released so that the partitions can be assigned to a healthy member. [source](https://www.confluent.io/blog/tutorial-getting-started-with-the-new-apache-kafka-0-9-consumer-client/)
      
The full list of properties is available [here]()http://kafka.apache.org/090/documentation.html#newconsumerconfigs.


# Consumer creation and topic subscription

Once we have the properties, creating a consumer is easy:

<!-- language: lang-java -->

    KafkaConsumer<String, String> consumer = new KafkaConsumer<>( props );
    consumer.subscribe( Collections.singletonList( "topic-example" ) );

After you have subscribed, the consumer can coordinate with the rest of the group to get its partition assignment. This is all handled automatically when you begin consuming data. 

# Basic poll

The consumer needs to be able to fetch data in parallel, potentially from many partitions for many topics likely spread across many brokers. Fortunately, this is all handled automatically when you begin consuming data. To do that, all you need to do is call `poll` in a loop and the consumer handles the rest. 

`poll` returns a (possibly empty) set of messages from the partitions that were assigned. 

<!-- language: lang-java -->

    while( true ){
        ConsumerRecords<String, String> records = consumer.poll( 100 );
        if( !records.isEmpty() ){
            StreamSupport.stream( records.spliterator(), false ).forEach( System.out::println );
        }
    }


# The code

## Basic example 

This is the most basic code you can use to fetch messages from a kafka topic.

<!-- language: lang-java -->

    public class ConsumerExample09{
    
        public static void main( String[] args ){
    
            Properties props = new Properties();
            props.put( "bootstrap.servers", "localhost:9092" );
            props.put( "key.deserializer", "org.apache.kafka.common.serialization.StringDeserializer" );
            props.put( "value.deserializer", "org.apache.kafka.common.serialization.StringDeserializer" );
            props.put( "auto.offset.reset", "earliest" );
            props.put( "enable.auto.commit", "false" );
            props.put( "group.id", "octopus" );
    
            try( KafkaConsumer<String, String> consumer = new KafkaConsumer<>( props ) ){
                consumer.subscribe( Collections.singletonList( "test-topic" ) );
    
                while( true ){
                    // poll with a 100 ms timeout
                    ConsumerRecords<String, String> records = consumer.poll( 100 );
                    if( records.isEmpty() ) continue;
                    StreamSupport.stream( records.spliterator(), false ).forEach( System.out::println );
                }
            }
        }
    }


## Runnable example

> The consumer is designed to be run in its own thread. It is not safe for multithreaded use without external synchronization and it is probably not a good idea to try. 

Below is a simple Runnable task which initializes the consumer, subscribes to a list of topics, and executes the poll loop indefinitely until shutdown externally.

<!-- language: lang-java -->

    public class ConsumerLoop implements Runnable{
        private final KafkaConsumer<String, String> consumer;
        private final List<String> topics;
        private final int id;
    
        public ConsumerLoop( int id, String groupId, List<String> topics ){
            this.id = id;
            this.topics = topics;
            Properties props = new Properties();
            props.put( "bootstrap.servers", "localhost:9092");
            props.put( "group.id", groupId );
            props.put( "auto.offset.reset", "earliest" );
            props.put( "key.deserializer", StringDeserializer.class.getName() );
            props.put( "value.deserializer", StringDeserializer.class.getName() );
            this.consumer = new KafkaConsumer<>( props );
        }
        
        @Override
        public void run(){
            try{
                consumer.subscribe( topics );
    
                while( true ){
                    ConsumerRecords<String, String> records = consumer.poll( Long.MAX_VALUE );
                    StreamSupport.stream( records.spliterator(), false ).forEach( System.out::println );
                }
            }catch( WakeupException e ){
                // ignore for shutdown 
            }finally{
                consumer.close();
            }
        }
    
    
        public void shutdown(){
            consumer.wakeup();
        }
    }
 
Note that we use a timeout of `Long.MAX_VALUE` during poll, so it will wait indefinitely for a new message.
To properly close the consumer, it is important to call its `shutdown()` method before ending the application.

A driver could use it like this: 

<!-- language: lang-java -->
    
    public static void main( String[] args ){

        int numConsumers = 3;
        String groupId = "octopus";
        List<String> topics = Arrays.asList( "test-topic" );

        ExecutorService executor = Executors.newFixedThreadPool( numConsumers );
        final List<ConsumerLoop> consumers = new ArrayList<>();

        for( int i = 0; i < numConsumers; i++ ){
            ConsumerLoop consumer = new ConsumerLoop( i, groupId, topics );
            consumers.add( consumer );
            executor.submit( consumer );
        }

        Runtime.getRuntime().addShutdownHook( new Thread(){
            @Override
            public void run(){
                for( ConsumerLoop consumer : consumers ){
                    consumer.shutdown();
                }
                executor.shutdown();
                try{
                    executor.awaitTermination( 5000, TimeUnit.MILLISECONDS );
                }catch( InterruptedException e ){
                    e.printStackTrace();
                }
            }
        } );
    }
    


## SimpleProducer (kafka >= 0.9)
 ## Configuration and initialization
 
 First, create a maven project and add the following dependency in your pom:
 
 <!-- language: lang-xml -->
     <dependencies>
         <dependency>
             <groupId>org.apache.kafka</groupId>
             <artifactId>kafka-clients</artifactId>
             <version>0.9.0.1</version>
         </dependency>
     </dependencies>


The producer is initialized using a `Properties` object. 
There are lots of properties allowing you to fine-tune the producer behavior. Below is the minimal configuration needed:

<!-- language: lang-java -->

    Properties props = new Properties();
    props.put("bootstrap.servers", "localhost:9092");
    props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer");
    props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer");
    props.put("client.id", "simple-producer-XX");
The `bootstrap-servers` is an initial list of one or more brokers for the producer to be able discover the rest of the cluster. The `serializer` properties tell Kafka how the message key and value should be encoded. Here, we will send string messages. 
 Although not required, setting a `client.id` since is always recommended: this allows you to easily correlate requests on the broker with the client instance which made it.

Other interesting properties are:

<!-- language: lang-java -->
    props.put("acks", "all"); 
    props.put("retries", 0);  
    props.put("batch.size", 16384);
    props.put("linger.ms", 1);
    props.put("buffer.memory", 33554432);

You can control the _durability of messages_ written to Kafka through the `acks` setting. The default value of “1” requires an explicit acknowledgement from the partition leader that the write succeeded. The strongest guarantee that Kafka provides is with `acks=all`, which guarantees that not only did the partition leader accept the write, but it was successfully replicated to all of the in-sync replicas. You can also use a value of “0” to maximize throughput, but you will have no guarantee that the message was successfully written to the broker’s log since the broker does not even send a response in this case.

`retries` (default to >0) determines if the producer try to resend message after a failure. Note that with retries > 0, message reordering may occur since the retry may occur after a following write succeeded. 

Kafka producers attempt to collect sent messages into batches to improve throughput. With the Java client, you can use `batch.size` to control the maximum size in bytes of each message batch. To give more time for batches to fill, you can use `linger.ms` to have the producer delay sending. Finally, compression can be enabled with the `compression.type` setting. 

Use `buffer.memory` to limit the total memory that is available to the Java client for collecting unsent messages. When this limit is hit, the producer will block on additional sends for as long as `max.block.ms` before raising an exception. Additionally, to avoid keeping records queued indefinitely, you can set a timeout using `request.timeout.ms`. 

The complete list of properties is available [here](http://kafka.apache.org/documentation/#producerconfigs). I suggest to read [this article](http://docs.confluent.io/3.1.2/clients/producer.html) from Confluent for more details.

# Sending messages

The `send()` method is asynchronous. When called it adds the record to a buffer of pending record sends and immediately returns. This allows the producer to batch together individual records for efficiency.

The result of send is a `RecordMetadata` specifying the partition the record was sent to and the offset it was assigned.  Since the send call is asynchronous it returns a `Future` for the RecordMetadata that will be assigned to this record. To consult the metadata, you can either call `get()`, which will block until the request completes or use a callback.

 <!-- language: lang-java -->
 
    // synchronous call with get()
    RecordMetadata recordMetadata = producer.send( message ).get();
    // callback with a lambda
    producer.send( message, ( recordMetadata, error ) -> System.out.println(recordMetadata) );
    
# The code 

 <!-- language: lang-java -->
    public class SimpleProducer{
        
        public static void main( String[] args ) throws ExecutionException, InterruptedException{
            Properties props = new Properties();
    
            props.put("bootstrap.servers", "localhost:9092");
            props.put("acks", "all");
            props.put("retries", 0);
            props.put("batch.size", 16384);
            props.put("linger.ms", 1);
            props.put("buffer.memory", 33554432);
            props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer");
            props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer");
            props.put( "client.id", "octopus" );
    
            String topic = "test-topic";
    
            Producer<String, String> producer = new KafkaProducer<>( props );
    
            for( int i = 0; i < 10; i++ ){
                ProducerRecord<String, String> message = new ProducerRecord<>( topic, "this is message " + i );
                producer.send( message );
                System.out.println("message sent.");
            }
    
            producer.close(); // don't forget this
        }
    }
    

