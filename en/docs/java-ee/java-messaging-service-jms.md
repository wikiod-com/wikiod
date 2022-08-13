---
title: "Java Messaging Service (JMS)"
slug: "java-messaging-service-jms"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

The [Java Message Service][1] is a Java API that allows applications to create, send, receive, and read messages. The JMS API defines a common set of interfaces and associated semantics that allow programs written in the Java programming language to communicate with other messaging implementations. JMS enables communication that is not only loosely coupled but also asynchronous and reliable.

  [1]: http://docs.oracle.com/javaee/7/tutorial/jms-concepts001.htm#BNCDR

Java Message Service (JMS) is a standard Java API that allows applications to create, send, receive and read messages asynchronously.

JMS defines general set of interfaces and classes that allow applications to interact with other messages providers.

JMS is similar to JDBC: JDBC connects to different databases (Derby, MySQL, Oracle, DB2 etc.) and JMS connects with different providers (OpenMQ, MQSeries, SonicMQ and so on).

JMS reference implementation is Open Message Queue (OpenMQ). It is open source project and can be used in standalone applications or can be built in application server. It is the default JMS provider integrated into GlassFish.


## Using ActiveMQ library for messaging (activemq jms provider specific implementations)
**Setup ActiveMQ**

 - Download a ActiveMQ distribution from activemq.apache.org and unpack it somewhere
 - You can start the server immediately, running unsecured on localhost, using the script bin/activemq
 - When it is running, you can access your local server's console on http://localhost:8161/admin/
 - Configure it by modifying conf/activemq.xml
 - As the title suggests following examples user activemq jms provider specific implementations and hence activemq-all.jar needs to be added to the classpath.

**Sending a message through standalone client**


    import javax.jms.Connection;
    import javax.jms.ConnectionFactory;
    import javax.jms.JMSException;
    import javax.jms.Message;
    import javax.jms.MessageProducer;
    import javax.jms.Queue;
    import javax.jms.Session;
    import org.apache.activemq.ActiveMQConnectionFactory;
    
    public class JmsClientMessageSender {
    
        public static void main(String[] args) {
            ConnectionFactory factory = new ActiveMQConnectionFactory("tcp://localhost:61616"); // ActiveMQ-specific
            Connection con = null;
            try {
                con = factory.createConnection();
                Session session = con.createSession(false, Session.AUTO_ACKNOWLEDGE); // non-transacted session
    
                Queue queue = session.createQueue("test.queue"); // only specifies queue name
    
                MessageProducer producer = session.createProducer(queue);
                Message msg = session.createTextMessage("hello queue"); // text message
                producer.send(msg);
    
            } catch (JMSException e) {
                e.printStackTrace();
            } finally {
                if (con != null) {
                    try {
                        con.close(); // free all resources
                    } catch (JMSException e) { /* Ignore */ }
                }
            }
        }
    }

**Polling for messages**

    import javax.jms.Connection;
    import javax.jms.ConnectionFactory;
    import javax.jms.JMSException;
    import javax.jms.Message;
    import javax.jms.MessageConsumer;
    import javax.jms.Queue;
    import javax.jms.Session;
    import javax.jms.TextMessage;
    import org.apache.activemq.ActiveMQConnectionFactory;
    
    public class JmsClientMessagePoller {
    
        public static void main(String[] args) {
            ConnectionFactory factory = new ActiveMQConnectionFactory("tcp://localhost:61616"); // ActiveMQ-specific
            Connection con = null;
    
            try {
                con = factory.createConnection();
                Session session = con.createSession(false, Session.AUTO_ACKNOWLEDGE); // non-transacted session
    
                Queue queue = session.createQueue("test.queue"); // only specifies queue name
    
                MessageConsumer consumer = session.createConsumer(queue);
    
                con.start(); // start the connection
                while (true) { // run forever
                    Message msg = consumer.receive(); // blocking!
                    if (!(msg instanceof TextMessage))
                        throw new RuntimeException("Expected a TextMessage");
                    TextMessage tm = (TextMessage) msg;
                    System.out.println(tm.getText()); // print message content
                }
            } catch (JMSException e) {
                e.printStackTrace();
            } finally {
                try {
                    con.close();
                } catch (JMSException e) {/* Ignore */ }
            }
        }
    }

**Using MessageListener**

    import javax.jms.Connection;
    import javax.jms.ConnectionFactory;
    import javax.jms.JMSException;
    import javax.jms.Message;
    import javax.jms.MessageConsumer;
    import javax.jms.MessageListener;
    import javax.jms.Queue;
    import javax.jms.Session;
    import javax.jms.TextMessage;
    import org.apache.activemq.ActiveMQConnectionFactory;
    
    public class JmsClientMessageListener {
    
        public static void main(String[] args) {
            ConnectionFactory factory = new ActiveMQConnectionFactory("tcp://localhost:61616"); // ActiveMQ-specific
            Connection con = null;
    
            try {
                con = factory.createConnection();
                Session session = con.createSession(false, Session.AUTO_ACKNOWLEDGE); // non-transacted session
                Queue queue = session.createQueue("test.queue"); // only specifies queue name
    
                MessageConsumer consumer = session.createConsumer(queue);
    
                consumer.setMessageListener(new MessageListener() {
                    public void onMessage(Message msg) {
                        try {
                            if (!(msg instanceof TextMessage))
                                throw new RuntimeException("no text message");
                            TextMessage tm = (TextMessage) msg;
                            System.out.println(tm.getText()); // print message
                        } catch (JMSException e) {
                            System.err.println("Error reading message");
                        }
                    }
                });
                con.start(); // start the connection
                Thread.sleep(60 * 1000); // receive messages for 60s
            } catch (JMSException e1) {
                e1.printStackTrace();
            } catch (InterruptedException e) {
                e.printStackTrace();
            } finally {
                try {
                    con.close();        // free all resources
                } catch (JMSException e) {
                    e.printStackTrace();
                } 
            }
        }
    }

## Creating ConnectionFactory


## Using jndi based lookup for messaging (Non-implementation-specific example)
This method allows non-implementation-specific code to be written and deployed across multiple jms platforms. Below basic example connects to activemq jms server and sends a message.

    import java.util.Properties;
    
    import javax.jms.JMSException;
    import javax.jms.Queue;
    import javax.jms.QueueConnection;
    import javax.jms.QueueConnectionFactory;
    import javax.jms.QueueSender;
    import javax.jms.QueueSession;
    import javax.jms.Session;
    import javax.jms.TextMessage;
    import javax.naming.Context;
    import javax.naming.InitialContext;
    import javax.naming.NamingException;
    
    public class JmsClientJndi {
    
        public static void main(String[] args) {
    
            Properties jndiProps = new Properties();
            // Following two could be set via a system property for flexibility in the code.
            jndiProps.setProperty(Context.INITIAL_CONTEXT_FACTORY, "org.apache.activemq.jndi.ActiveMQInitialContextFactory");
            jndiProps.setProperty(Context.PROVIDER_URL, "tcp://localhost:61616");
            
            QueueConnection conn = null;
            QueueSession session = null;
            QueueSender sender = null;
            InitialContext jndi = null;
            try {
                jndi = new InitialContext(jndiProps);
                QueueConnectionFactory factory = (QueueConnectionFactory) jndi.lookup("ConnectionFactory");
                conn = factory.createQueueConnection();
                conn.start();
    
                session = conn.createQueueSession(false, Session.AUTO_ACKNOWLEDGE);
                Queue queue = (Queue) jndi.lookup("dynamicQueues/test.queue");
                sender = session.createSender(queue);
                
                TextMessage msg = session.createTextMessage();
                msg.setText("Hello worlds !!!!! ");
                sender.send(msg);
                
                
            } catch (NamingException e) {
                e.printStackTrace();
            } catch (JMSException e) {
                e.printStackTrace();
            } finally {
                try {
                    if (sender != null)
                        sender.close();
                    if (session != null)
                        session.close();
                    if (conn != null)
                        conn.close();
                } catch (JMSException e) {
                    e.printStackTrace();
                }
            }
        }
    }

