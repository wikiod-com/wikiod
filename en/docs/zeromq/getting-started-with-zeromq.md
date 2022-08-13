---
title: "Getting started with zeromq"
slug: "getting-started-with-zeromq"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Zeromq has a huge number of bindings for different languages.

To find the right binding and instructions how to use [follow this][1].

Also you can use: a pure [C# implementation][2] or [Java implementation][3]


  [1]: http://zeromq.org/bindings:_start
  [2]: https://github.com/zeromq/netmq
  [3]: https://github.com/zeromq/jeromq

## ZeroMQ Hello World example.
In this example we will make a simple client and server with REQ-REP (request-reply) sockets. The client sends "Hello" to the server, which replies with "World".
Server opens a ZeroMQ REP-socket on port 5555, reads requests on it, and replies with "World" to each request.

**Hello World server in C:**
    
    #include <zmq.h>
    #include <stdio.h>
    #include <unistd.h>
    #include <string.h>
    #include <assert.h>
    
    int main (void)
    {
        //  Socket to talk to clients
        void *context = zmq_ctx_new ();
        void *responder = zmq_socket (context, ZMQ_REP);
        int rc = zmq_bind (responder, "tcp://*:5555");
        assert (rc == 0);
    
        while (1) {
            char buffer [10];
            zmq_recv (responder, buffer, 10, 0);
            printf ("Received Hello\n");
            sleep (1);          //  Do some 'work'
            zmq_send (responder, "World", 5, 0);
        }
        return 0;
    }

**Hello World server in Java:**

    import org.zeromq.ZMQ;
    
    public class hwserver {
    
        public static void main(String[] args) throws Exception {
            ZMQ.Context context = ZMQ.context(1);
    
            //  Socket to talk to clients
            ZMQ.Socket responder = context.socket(ZMQ.REP);
            responder.bind("tcp://*:5555");
    
            while (!Thread.currentThread().isInterrupted()) {
                // Wait for next request from the client
                byte[] request = responder.recv(0);
                System.out.println("Received Hello");
    
                // Do some 'work'
                Thread.sleep(1000);
    
                // Send reply back to client
                String reply = "World";
                responder.send(reply.getBytes(), 0);
            }
            responder.close();
            context.term();
        }
    }

**Hello World client in C:**

    #include <zmq.h>
    #include <string.h>
    #include <stdio.h>
    #include <unistd.h>
    
    int main (void)
    {
        printf ("Connecting to hello world server…\n");
        void *context = zmq_ctx_new ();
        void *requester = zmq_socket (context, ZMQ_REQ);
        zmq_connect (requester, "tcp://localhost:5555");
    
        int request_nbr;
        for (request_nbr = 0; request_nbr != 10; request_nbr++) {
            char buffer [10];
            printf ("Sending Hello %d…\n", request_nbr);
            zmq_send (requester, "Hello", 5, 0);
            zmq_recv (requester, buffer, 10, 0);
            printf ("Received World %d\n", request_nbr);
        }
        zmq_close (requester);
        zmq_ctx_destroy (context);
        return 0;
    }

**Hello World client in Java:**

    import org.zeromq.ZMQ;
    
    public class hwclient {
    
        public static void main(String[] args) {
            ZMQ.Context context = ZMQ.context(1);
    
            //  Socket to talk to server
            System.out.println("Connecting to hello world server…");
    
            ZMQ.Socket requester = context.socket(ZMQ.REQ);
            requester.connect("tcp://localhost:5555");
    
            for (int requestNbr = 0; requestNbr != 10; requestNbr++) {
                String request = "Hello";
                System.out.println("Sending Hello " + requestNbr);
                requester.send(request.getBytes(), 0);
    
                byte[] reply = requester.recv(0);
                System.out.println("Received " + new String(reply) + " " + requestNbr);
            }
            requester.close();
            context.term();
        }
    }

**Note:** REQ-REP zeromq sockets are blocking, so when a client (REQ) sends a message, he can't do anything else until he receives a response from the server and vice versa - until the server (REP) receives a message from the client, he can't send him anything.

