---
title: "Hello world"
slug: "hello-world"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Akka hello world (Scala)
 1. Add akka-actor dependency (SBT example)

<!-- language: scala -->
    libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.4.8"

 2. Create actor classes:

  Actor for string output:
<!-- language: scala -->
    class OutputActor extends Actor {
      override def receive: Receive = {
        case message => println(message)
      }
    }

Actor for string modifying:
<!-- language: scala -->
    class AppendActor(outputActor: ActorRef) extends Actor {
      override def receive: Receive = {
        case message: String =>
          val changed = s"Hello, $message!"
          outputActor ! changed

        case unknown =>
          println(s"unknown message: $unknown")
      }
    }

 3. Create actor systems and send message

<!-- language: scala -->
    object HelloWorld extends App {
      val system = ActorSystem("HelloWorld")
      val outputActor = system.actorOf(Props[OutputActor], name = "output")
      val appendActor = system.actorOf(Props(classOf[AppendActor], outputActor), name = "appender")

      appendActor ! "Akka" // send test message
      Thread.sleep(500) // wait for async evaluation
      system.terminate() // terminate actors system
    }

Program output:

> Hello, Akka!

## Akka Hello World (Java 8)
Add this dependency to your project POM:

<!-- language: lang-xml -->
    <dependency>
            <groupId>com.typesafe.akka</groupId>
            <artifactId>akka-actor_2.11</artifactId>
            <version>2.4.4</version>
    </dependency>

Create an Actor

<!-- language: lang-java -->
    public class HelloWorldActor extends AbstractActor {

        public HelloActor() {
            receive(ReceiveBuilder
                .match(SayHello.class, this::sayHello)
                .match(SayBye.class, this::sayBye)
                .build());
        }

        private void sayHello(final SayHello message) {
            System.out.println("Hello World");
        }

        private void sayHello(final SayBye message) {
            System.out.println("Bye World");
        }

        public static Props props() {
            return Props.create(HelloWorldActor.class);
        }
    }

Create a Junit test for the actor

<!-- language: lang-java -->
    public class HelloActorTest {
    
        private ActorSystem actorSystem;
    
        @org.junit.Before
        public void setUp() throws Exception {
            actorSystem = ActorSystem.create();
        }
    
        @After
        public void tearDown() throws Exception {
            JavaTestKit.shutdownActorSystem(actorSystem);
        }
    
        @Test
        public void testSayHello() throws Exception {
            new JavaTestKit(actorSystem) {
                {
                    ActorRef helloActorRef = actorSystem.actorOf(HelloWorldActor.props());
                    helloActorRef.tell(new SayHello(), ActorRef.noSender());
                    helloActorRef.tell(new SayBye(), ActorRef.noSender());
                }
            };
         }
     }



## Simple Actor Implementation
Consider a communication happening between a Employee and its HR Department.   
    
[![Actor System Message Flow][1]][1]


Broadly these are explained in the following six steps when a message is passed to the actor:

 1. Employee creates something called an `ActorSystem`.

 2. It uses the ActorSystem to create something called as `ActorRef`. The message(MSG) is sent to the `ActorRef` (a proxy to HR Actor).

 3. Actor ref passes the message along to a `Message Dispatcher`.

 4. The Dispatcher enqueues the message in the target Actor’s `MailBox`.

 5. The Dispatcher then puts the `Mailbox` on a Thread (more on that in the next section).

 6. The `MailBox` dequeues a message and eventually delegates that to the actual HR Actor’s `receive` method.



        /** The Main Program consider it as a Employee Actor that is sending the requests  **/

        object EmployeeActorApp extends App{
         //Initialize the ActorSystem
          val actorSystem=ActorSystem("HrMessageingSystem")

         //construct the HR Actor Ref
          val hrActorRef=actorSystem.actorOf(Props[HrActor])

         //send a message to the HR Actor
          hrActorRef!Message

         //Let's wait for a couple of seconds before we shut down the    system
          Thread.sleep (2000) 

         //Shut down the ActorSystem.
          actorSystem.shutdown()
    
        }  


        /** The HRActor reads the message sent to it and performs action based on the message Type **/
        class HRActor extends Actor {
           def receive  = {
                case s: String if(s.equalsIgnoreCase(“SICK”)) => println("Sick Leave applied”)
                case s: String if(s.equalsIgnoreCase(“PTO”)) => println("PTO applied “)
    }
 }


  [1]: http://i.stack.imgur.com/7xZNq.jpg

