---
title: "Sockets"
slug: "sockets"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

A socket is one end-point of a two-way communication link between two programs running on the network.

## Read from socket
    String hostName = args[0];
    int portNumber = Integer.parseInt(args[1]);
    
    try (
        Socket echoSocket = new Socket(hostName, portNumber);
        PrintWriter out =
            new PrintWriter(echoSocket.getOutputStream(), true);
        BufferedReader in =
            new BufferedReader(
                new InputStreamReader(echoSocket.getInputStream()));
        BufferedReader stdIn =
            new BufferedReader(
                new InputStreamReader(System.in))
    ) {
        //Use the socket
    }

