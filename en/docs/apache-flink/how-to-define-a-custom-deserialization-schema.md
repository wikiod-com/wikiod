---
title: "How to define a custom (de)serialization schema"
slug: "how-to-define-a-custom-deserialization-schema"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

Schemas are used by some connectors (Kafka, RabbitMQ) to turn messages into Java objects and vice-versa.

## Custom Schema Example
To use a custom schema, all you need to do is implement one of the `SerializationSchema` or `DeserializationSchema` interface.  

<!-- language: lang-java -->

    public class MyMessageSchema implements DeserializationSchema<MyMessage>, SerializationSchema<MyMessage> {

        @Override
        public MyMessage deserialize(byte[] bytes) throws IOException {
            return MyMessage.fromString(new String(bytes));
        }

        @Override
        public byte[] serialize(MyMessage myMessage) {
            return myMessage.toString().getBytes();
        }

        @Override
        public TypeInformation<MyMessage> getProducedType() {
            return TypeExtractor.getForClass(MyMessage.class);
        }

        // Method to decide whether the element signals the end of the stream.
        // If true is returned the element won't be emitted.
        @Override
        public boolean isEndOfStream(MyMessage myMessage) {
            return false;
        }
    }

The `MyMessage` class is defined as follow:

<!-- language: lang-java -->

    public class MyMessage{

        public int id;
        public String payload;
        public Date timestamp;
        
        public MyMessage(){}
        
        public static MyMessage fromString( String s ){
            String[] tokens = s.split( "," );
            if(tokens.length != 3) throw new RuntimeException( "Invalid record: " + s );
    
            try{
                MyMessage message = new MyMessage();
                message.id = Integer.parseInt(tokens[0]);
                message.payload = tokens[1];
                message.timestamp = new Date( Long.parseLong(tokens[0]));
                return message;
            }catch(NumberFormatException e){
                throw new RuntimeException("Invalid record: " + s);
            }
        }
    
        public String toString(){
            return String.format("%d,%s,%dl", id, payload, timestamp.getTime());
        }
    }

