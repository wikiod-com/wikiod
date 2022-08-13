---
title: "Getting started with spring-integration"
slug: "getting-started-with-spring-integration"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
The best way to get started using Spring-Integration in your project is with a dependency management system, like gradle.

    dependencies {
        compile 'org.springframework.integration:spring-integration-core:4.3.5.RELEASE'
    }

Below is a very simple example using the [gateway](http://docs.spring.io/spring-integration/docs/4.3.5.RELEASE/reference/html/messaging-endpoints-chapter.html#gateway), [service-activator](http://docs.spring.io/spring-integration/docs/4.3.5.RELEASE/reference/html/messaging-endpoints-chapter.html#service-activator) message endpoints.

    //these annotations will enable Spring integration and scan for components
    @Configuration
    @EnableIntegration
    @IntegrationComponentScan
    public class Application {
        //a channel has two ends, this Messaging Gateway is acting as input from one side of inChannel
        @MessagingGateway
        interface Greeting {
            @Gateway(requestChannel = "inChannel")
            String greet(String name);
        }
    
        @Component
        static class HelloMessageProvider {
            //a service activator act as a handler when message is received from inChannel, in this example, it is acting as the handler on the output side of inChannel
            @ServiceActivator(inputChannel = "inChannel")
            public String sayHello(String name) {
                return "Hi, " + name;
            }
        }
    
        @Bean
        MessageChannel inChannel() {
            return new DirectChannel();
        }
    
        public static void main(String[] args) {
            ApplicationContext context = new AnnotationConfigApplicationContext(Application.class);
            Greeting greeting = context.getBean(Greeting.class);
            //greeting.greet() send a message to the channel, which trigger service activitor to process the incoming message
            System.out.println(greeting.greet("Spring Integration!"));
        }
    }

It will display the string `Hi, Spring Integration!` in the console.

Of course, Spring Integration also provides xml-style configuration. For the above example, you can write such following xml configuration file.

    <?xml version="1.0" encoding="UTF-8"?>
    <beans xmlns="http://www.springframework.org/schema/beans"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xmlns:int="http://www.springframework.org/schema/integration"
           xsi:schemaLocation="http://www.springframework.org/schema/beans
            http://www.springframework.org/schema/beans/spring-beans.xsd
            http://www.springframework.org/schema/integration
            http://www.springframework.org/schema/integration/spring-integration.xsd">
        <int:gateway default-request-channel="inChannel"
                     service-interface="spring.integration.stackoverflow.getstarted.Application$Greeting"/>
        <int:channel id="inChannel"/>
        <int:service-activator input-channel="inChannel" method="sayHello">
            <bean class="spring.integration.stackoverflow.getstarted.Application$HelloMessageProvider"/>
        </int:service-activator>
    </beans>

To run the application using the xml config file, you should change the code `new AnnotationConfigApplicationContext(Application.class)` in `Application` class to `new ClassPathXmlApplicationContext("classpath:getstarted.xml")`. And run this application again, you can see the same output.

## Generic Inbound and Outbound Channel Adapter
Channel adapter is one of message endpoints in Spring Integration. It is used for unidirectional message flow. There are two types of channel adapter:

**Inbound Adapter**: input side of the channel. Listen or actively read message.

**Outbound Adapter**: output side of the channel. Send message to Java class or external system or protocol.

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/aTes7.png

* Source code.

      public class Application {
          static class MessageProducer {
              public String produce() {
                  String[] array = {"first line!", "second line!", "third line!"};
                  return array[new Random().nextInt(3)];
              }
          }

          static class MessageConsumer {
              public void consume(String message) {
                  System.out.println(message);
              }
          }

          public static void main(String[] args) {
              new ClassPathXmlApplicationContext("classpath:spring/integration/stackoverflow/ioadapter/ioadapter.xml");
          }
      }

* XML-style Configuration file:

      <?xml version="1.0" encoding="UTF-8"?>
      <beans xmlns="http://www.springframework.org/schema/beans"
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xmlns:int="http://www.springframework.org/schema/integration"
             xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
             http://www.springframework.org/schema/integration http://www.springframework.org/schema/integration/spring-integration.xsd">
          <int:channel id="channel"/>
          <int:inbound-channel-adapter id="inAdapter" channel="channel" method="produce">
              <bean class="spring.integration.stackoverflow.ioadapter.Application$MessageProducer"/>
              <int:poller fixed-rate="1000"/>
          </int:inbound-channel-adapter>
          <int:outbound-channel-adapter id="outAdapter" channel="channel" method="consume">
              <bean class="spring.integration.stackoverflow.ioadapter.Application$MessageConsumer"/>
          </int:outbound-channel-adapter>
      </beans>

* Message Flow

    * `inAdapter`: an inbound channel adapter. Invoke `Application$MessageProducer.produce` method every 1 second (`<int:poller fixed-rate="1000"/>`) and send the returned string as message to the channel `channel`.
    * `channel`: channel to transfer message. 
    * `outAdapter`: an outbound channel adapter. Once message reached on channel `channel`, this adapter will receive the message and then send it to `Application$MessageConsumer.consume` method which print the message on the console.
    * So you can observe that these random choose string will displayed on the console every 1 second.


## Simple Echo Excample with Spring-Integration-Stream
[![enter image description here][1]][1]


**Java code:**

    public class StdioApplication {
        public static void main(String[] args) {
            new ClassPathXmlApplicationContext("classpath:spring/integration/stackoverflow/stdio/stdio.xml");
        }
    }

**Xml config file**

    <?xml version="1.0" encoding="UTF-8"?>
    <beans xmlns="http://www.springframework.org/schema/beans"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xmlns:int="http://www.springframework.org/schema/integration"
           xmlns:int-stream="http://www.springframework.org/schema/integration/stream"
           xsi:schemaLocation="http://www.springframework.org/schema/beans
           http://www.springframework.org/schema/beans/spring-beans.xsd
           http://www.springframework.org/schema/integration/stream
           http://www.springframework.org/schema/integration/stream/spring-integration-stream.xsd
           http://www.springframework.org/schema/integration
           http://www.springframework.org/schema/integration/spring-integration.xsd">
        <int:channel id="channel"/>
        <int-stream:stdin-channel-adapter id="stdin" channel="channel">
            <int:poller fixed-rate="1000"/>
        </int-stream:stdin-channel-adapter>
        <int-stream:stdout-channel-adapter id="stdout" channel="channel"/>
    </beans>

This is a echo example. When you run this Java application, you can input some string and then it will be displayed on the console.

  [1]: https://i.stack.imgur.com/wkNYA.png

