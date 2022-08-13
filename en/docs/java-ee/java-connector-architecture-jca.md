---
title: "Java Connector Architecture (JCA)"
slug: "java-connector-architecture-jca"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Let's clarify some terminologies first:     

 - **Outbound Messaging** is where the message starts from the server (to be more accurate it's initiated from your app which you have on the server, `WebSphere Liberty` in this case) and end at the EIS.
 - **Inbound Messaging** is where message starts from the EIS and end at the server.  
 - **Message Endpoint** in general the place where the message end up sitting/getting received at a specific stage of its life cycle.

[![enter image description here][1]][1]

So with outbound connectivity, we are referring the situation where an application obtains a connection to an external EIS and reads or writes data to it. With inbound connectivity we are referring the situation where the Resource Adapter (RA) listens for events from the external EIS and calls into your application when such an event occurs.

*Illustration of an Outbound RA*

[![enter image description here][2]][2]

*Illustration of an Inbound RA*

[![enter image description here][3]][3]

**What is a MessageEndPoint mean in JCA?**

 The application server (ex: `WebSphere Liberty`) provides message endpoint MBeans to assist you in managing the delivery of a message to your message-driven beans that are acting as listeners on specific endpoints, which are destinations, and in managing the EIS resources that are utilized by these message-driven beans. Message-driven beans that are deployed as message endpoints are not the same as message-driven beans that are configured against a listener port. Message-driven beans that are used as message endpoints must be deployed using an `ActivationSpecification` that is defined within an RA configuration for JCA (Found in the `ra.xml` file) .

**What does it mean activating a MessageEndPoint?**

With message endpoint MBeans, you can activate and deactivate specific endpoints within your applications to ensure that messages are delivered only to listening message-driven beans that are interacting with healthy EIS resources. This capability allows you to optimize the performance of your JMS applications in situations where an EIS resource is not behaving as expected. Message delivery to an endpoint typically fails when the message driven bean that is listening invokes an operation against a resource that is not healthy. For example, a messaging provider, which is an inbound resource adapter that is JCA compliant, might fail to deliver messages to an endpoint when its underlying message-driven bean attempts to commit transactions against a database server that is not responding.

**Does MessageEndPoint need to be a bean?**

It should. Otherwise you will end up in a big mess by creating your own unconventional way of doing stuff which beat the purpose of following Java EE specification in the first place.
Design your message-driven beans to delegate business processing to other enterprise beans. Do not access the EIS resources directly in the message-driven bean, but do so indirectly through a delegate bean.

**Can you show some simple example on working/deploying a MessageEndPoint?**

Check the second resource I'm mentioning below for a helpful example.

**Useful learning resources:**

 - [Managing messages with message endpoints][4]
 - [Develop inbound connectors][5]


  [1]: http://i.stack.imgur.com/DHV1M.gif
  [2]: http://i.stack.imgur.com/306VT.png
  [3]: http://i.stack.imgur.com/J1PXA.png
  [4]: http://www-01.ibm.com/support/knowledgecenter/SSD28V_8.5.5/com.ibm.websphere.nd.doc/ae/tdat_msgendpoint.html
  [5]: http://www.javaworld.com/article/2071886/java-web-development/develop-inbound-connectors-with-jca-1-5.html

## Example Resource Adapter
    class MyResourceAdapter 
       implements javax.resource.spi.ResourceAdapter {
      
       public void start(BootstrapContext ctx){..}
       public void stop(){..}
    
       public void endpointActivation (MessageEndpoingFactory mf, ActivationSpec a){..}
       public void endpointDeactivation (MessageEndpoingFactory mf, ActivationSpec a){..}
       public void getXAResources(ActivationSpec[] activationSpecs){..}
    }

