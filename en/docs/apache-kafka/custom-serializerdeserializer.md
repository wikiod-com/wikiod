---
title: "Custom SerializerDeserializer"
slug: "custom-serializerdeserializer"
draft: false
images: []
weight: 9832
type: docs
toc: true
---

Kafka stores and transports byte arrays in its queue. The (de)serializers are responsible for translating between the byte array provided by Kafka and POJOs. 



## Syntax
- public void configure(Map<String, ?> config, boolean isKey);
- public T deserialize(String topic, byte[] bytes); 
- public byte[] serialize(String topic, T obj);

## Parameters
|parameters|details|
|----------|-------|
| config | the configuration properties (`Properties`) passed to the `Producer` or `Consumer` upon creation, as a map. It contains regular kafka configs, but can also be augmented with user-defined configuration. It is the best way to pass arguments to the (de)serializer.   |
|isKey| custom (de)serializers can be used for keys and/or values. This parameter tells you which of the two this instance will deal with. |
|topic| the topic of the current message. This lets you define custom logic based on the source/destination topic. |
|bytes| The raw message to deserialize|
|obj| The message to serialize. Its actual class depends on your serializer.|


Before version 0.9.0.0 the Kafka Java API used `Encoders` and `Decoders`. They have been replaced by `Serializer` and `Deserializer` in the new API.

## Gson (de)serializer
This example uses the [gson](https://github.com/google/gson) library to map java objects to json strings. The (de)serializers are generic, but they don't always need to be !


# Serializer 

## Code
<!-- language: lang-java -->

    public class GsonSerializer<T> implements Serializer<T> {
    
        private Gson gson = new GsonBuilder().create();
        
        @Override
        public void configure(Map<String, ?> config, boolean isKey) {
            // this is called right after construction
            // use it for initialisation
        }
        
        @Override
        public byte[] serialize(String s, T t) {
            return gson.toJson(t).getBytes();
        }
        
        @Override
        public void close() {
            // this is called right before destruction
        }
    }

## Usage

Serializers are defined through the required `key.serializer` and `value.serializer` producer properties. 

Assume we have a POJO class named `SensorValue` and that we want to produce messages without any key (keys set to `null`):

<!-- language: lang-java -->

    Properties props = new Properties();
    props.put("bootstrap.servers", "localhost:9092");
    // ... other producer properties ... 
    props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer");
    props.put("value.serializer", GsonSerializer.class.getName());

    Producer<String, SensorValue> producer = new KafkaProducer<>(properties);
    // ... produce messages ... 
    producer.close();

(`key.serializer` is a required configuration. Since we don't specify message keys, we keep the `StringSerializer` shipped with kafka, which is able to handle `null`).

-----------

# deserializer

## Code

<!-- language: lang-java -->

    public class GsonDeserializer<T> implements Deserializer<T> {
    
        public static final String CONFIG_VALUE_CLASS = "value.deserializer.class";
        public static final String CONFIG_KEY_CLASS = "key.deserializer.class";
        private Class<T> cls;
    
        private Gson gson = new GsonBuilder().create();
    
    
        @Override
        public void configure(Map<String, ?> config, boolean isKey) {
            String configKey = isKey ? CONFIG_KEY_CLASS : CONFIG_VALUE_CLASS;
            String clsName = String.valueOf(config.get(configKey));
    
            try {
                cls = (Class<T>) Class.forName(clsName);
            } catch (ClassNotFoundException e) {
                System.err.printf("Failed to configure GsonDeserializer. " +
                        "Did you forget to specify the '%s' property ?%n",
                        configKey);
            }
        }
    
    
        @Override
        public T deserialize(String topic, byte[] bytes) {
            return (T) gson.fromJson(new String(bytes), cls);
        }
    
    
        @Override
        public void close() {}
    }

## Usage

Deserializers are defined through the required `key.deserializer` and `value.deserializer` consumer properties. 

Assume we have a POJO class named `SensorValue` and that we want to produce messages without any key (keys set to `null`):

<!-- language: lang-java -->

    Properties props = new Properties();
    props.put("bootstrap.servers", "localhost:9092");
    // ... other consumer properties ... 
    props.put("key.deserializer", "org.apache.kafka.common.serialization.StringDeserializer");
    props.put("value.deserializer", GsonDeserializer.class.getName());
    props.put(GsonDeserializer.CONFIG_VALUE_CLASS, SensorValue.class.getName());

    try (KafkaConsumer<String, SensorValue> consumer = new KafkaConsumer<>(props)) {
        // ... consume messages ... 
    }

Here, we add a custom property to the consumer configuration, namely `CONFIG_VALUE_CLASS`. The `GsonDeserializer` will use it in the `configure()` method to determine what POJO class it should handle (all properties added to `props` will be passed to the `configure` method in the form of a map).

