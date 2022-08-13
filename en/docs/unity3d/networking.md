---
title: "Networking"
slug: "networking"
draft: false
images: []
weight: 9874
type: docs
toc: true
---

## Headless mode in Unity ##
If you are building a Server to deploy in Linux, the Build settings have a "Headless mode" option. An application build with this option doesn't display anything and doesn't read user input, which is usually what we want for a Server.

[![Headless mode in Build settings][1]][1]


  [1]: http://i.stack.imgur.com/iDxCm.png

## Creating a server, a client, and sending a message.
<!-- language-all: lang-cs -->
Unity networking provides the High Level API (HLA) to handle network communications abstracting from low level implementations.

In this example we will see how to create a Server that can communicate with one or multiple clients.

The HLA allows us to easily serialize a class and send objects of this class over the network.


---


## The Class we are using to serialize ##
This class have to inherance from MessageBase, in this example we will just send a string inside this class.

    using System;
    using UnityEngine.Networking;
    
    public class MyNetworkMessage : MessageBase
    {
        public string message;
    }
---
## Creating a Server ##
We create a server that listen to the port 9999, allows a maximum of 10 connections, and read objects from the network of our custom class.

The HLA associates different types of message to an id. There are default messages type defined in the MsgType class from Unity Networking. For example the connect type have id 32 and it is called in the server when a client connects to it, or in the client when it connects to a server. You can register handlers to manage the different types of message.

When you are sending a custom class, like our case, we define a handlers with a new id associated to the class we are sending over the network.


    using UnityEngine;
    using System.Collections;
    using UnityEngine.Networking;
    
    public class Server : MonoBehaviour {
    
        int port = 9999;
        int maxConnections = 10;
    
        // The id we use to identify our messages and register the handler
        short messageID = 1000;
    
        // Use this for initialization
        void Start () {
            // Usually the server doesn't need to draw anything on the screen
            Application.runInBackground = true;
            CreateServer();
        }    
    
        void CreateServer() {
            // Register handlers for the types of messages we can receive
            RegisterHandlers ();
    
            var config = new ConnectionConfig ();
            // There are different types of channels you can use, check the official documentation
            config.AddChannel (QosType.ReliableFragmented);
            config.AddChannel (QosType.UnreliableFragmented);
    
            var ht = new HostTopology (config, maxConnections);
    
            if (!NetworkServer.Configure (ht)) {
                Debug.Log ("No server created, error on the configuration definition");
                return;
            } else {
                // Start listening on the defined port
                if(NetworkServer.Listen (port))
                    Debug.Log ("Server created, listening on port: " + port);   
                else
                    Debug.Log ("No server created, could not listen to the port: " + port);    
            }
        }
    
        void OnApplicationQuit() {
            NetworkServer.Shutdown ();
        }
    
        private void RegisterHandlers () {
            // Unity have different Messages types defined in MsgType
            NetworkServer.RegisterHandler (MsgType.Connect, OnClientConnected);
            NetworkServer.RegisterHandler (MsgType.Disconnect, OnClientDisconnected);
    
            // Our message use his own message type.
            NetworkServer.RegisterHandler (messageID, OnMessageReceived);
        }
    
        private void RegisterHandler(short t, NetworkMessageDelegate handler) {
            NetworkServer.RegisterHandler (t, handler);
        }
    
        void OnClientConnected(NetworkMessage netMessage)
        {
            // Do stuff when a client connects to this server
    
            // Send a thank you message to the client that just connected
            MyNetworkMessage messageContainer = new MyNetworkMessage();
            messageContainer.message = "Thanks for joining!";
    
            // This sends a message to a specific client, using the connectionId
            NetworkServer.SendToClient(netMessage.conn.connectionId,messageID,messageContainer);
    
            // Send a message to all the clients connected
            messageContainer = new MyNetworkMessage();
            messageContainer.message = "A new player has conencted to the server";
    
            // Broadcast a message a to everyone connected
            NetworkServer.SendToAll(messageID,messageContainer);
        }
    
        void OnClientDisconnected(NetworkMessage netMessage)
        {
            // Do stuff when a client dissconnects
        }
    
        void OnMessageReceived(NetworkMessage netMessage)
        {
            // You can send any object that inherence from MessageBase
            // The client and server can be on different projects, as long as the MyNetworkMessage or the class you are using have the same implementation on both projects
            // The first thing we do is deserialize the message to our custom type
            var objectMessage = netMessage.ReadMessage<MyNetworkMessage>();
            Debug.Log("Message received: " + objectMessage.message);
    
        }
    }
---
## The Client ##
Now we create a Client 

    using System;
    using UnityEngine;
    using UnityEngine.Networking;
    
    
    public class Client : MonoBehaviour
    {
        int port = 9999;
        string ip = "localhost";
    
        // The id we use to identify our messages and register the handler
        short messageID = 1000;
    
        // The network client
        NetworkClient client;
    
        public Client ()
        {
            CreateClient();
        }
    
        void CreateClient()
        {
            var config = new ConnectionConfig ();
    
            // Config the Channels we will use
            config.AddChannel (QosType.ReliableFragmented);
            config.AddChannel (QosType.UnreliableFragmented);
    
            // Create the client ant attach the configuration
            client = new NetworkClient ();
            client.Configure (config,1);
    
            // Register the handlers for the different network messages
            RegisterHandlers();

            // Connect to the server
            client.Connect (ip, port);
        }

        // Register the handlers for the different message types
        void RegisterHandlers () {
        
            // Unity have different Messages types defined in MsgType
            client.RegisterHandler (messageID, OnMessageReceived);
            client.RegisterHandler(MsgType.Connect, OnConnected);
            client.RegisterHandler(MsgType.Disconnect, OnDisconnected);
        }

        void OnConnected(NetworkMessage message) {        
            // Do stuff when connected to the server
    
            MyNetworkMessage messageContainer = new MyNetworkMessage();
            messageContainer.message = "Hello server!";
    
            // Say hi to the server when connected
            client.Send(messageID,messageContainer);
        }
    
        void OnDisconnected(NetworkMessage message) {
            // Do stuff when disconnected to the server
        }
    
        // Message received from the server
        void OnMessageReceived(NetworkMessage netMessage)
        {
            // You can send any object that inherence from MessageBase
            // The client and server can be on different projects, as long as the MyNetworkMessage or the class you are using have the same implementation on both projects
            // The first thing we do is deserialize the message to our custom type
            var objectMessage = netMessage.ReadMessage<MyNetworkMessage>();
    
            Debug.Log("Message received: " + objectMessage.message);
        }
    }






