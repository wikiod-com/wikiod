---
title: "Differences between WebSockets and AJAX"
slug: "differences-between-websockets-and-ajax"
draft: false
images: []
weight: 9916
type: docs
toc: true
---

Ajax uses the HTTP Protocol and can send requests using POST/GET methods from Client to Server.

WebSocket is itself a protocol to communicate between Client and Server, distinct from HTTP.

In Ajax when you send a request , server sends response for that request and connection ends.

Using WebSockets when you establish a connection with server , then you can communicate between client and server as much you want and it keeps connection alive.


## example getting notification
Ajax :

 1. New request :
     - Client sends request : Server , Do you have new notification ?
     - Server sends response : Client , NO
     - connection ends
 2. New request :
     - Client sends request : Server , Do you have new notification ?
     - Server sends response : Client , NO
     - connection ends
 3. New request :
     - Client sends request : Server , Do you have new notification ?
     - Server sends response : Client , NO
     - connection ends
 4. New request :
     - Client sends request : Server , Do you have new notification ?
     - Server sends response : Client , Yes here you are
     - connection ends

you see that there is number of useless requests !

web-socket

 - connection established :
 - client says : Server , Do you have new notification ?

> after xx time passed

 - Server says : Yes I have 

> after xx time passed

 - Server says : Yes I have 
 - Client says : Server , send new email if I have

> after xx time passed

 - Server says : now you got new email

now the it's clear that with web-socket we won't have useless requests
 



