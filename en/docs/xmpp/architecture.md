---
title: "Architecture"
slug: "architecture"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

XMPP allows for the full-duplex exchange of structured data and concurrent processing of requests between globally addressable clients and servers on the network. Unlike HTTP and the "Representational State Transfer" (REST) architecture widely deployed on the web, XMPP connections are stateful and concurrent, and an unlimited number of transactions may occur in the context of a single session. This architecture is sometimes refered too as "Availability for Concurrent Transactions" (ACT).

## Addressability ##

To faciliate routing across the network, all XMPP addresses are globally addressable. Like email, this is acomplished with DNS and a federated client/server architecture. Addresses are of the form `localpart@domainpart/resourcepart` where the localpart is optional and corresponds to a user of the network, the domainpar is required and corresponds to a server, and resourcepart is optional and refers to a specific connected client for that user (in XMPP users may be signed in from many different locations, eg. a phone and a laptop in the case of instant messaging, or many sensors using one account in the case of internet-of-things enabled devices). XMPP also provides facilities for discovering the presence (availability) of other addresses on the network.

## Stateful Streams ##

XMPP connections are long lived TCP connections that transport XML streams from a client to a server (c2s) or from a server to a server (s2s). Having these sessions be long lived and stateful allow nodes in the network to transmit data at any time and have it routed or delivered immediately.

## Routing ##

Streams form a direct link on the network between a client and a server or a server and a server. If a client wishes to communicate with a remote client on the network, they first send the information to their server which forms a server-to-server connection with the remote server which then delivers the information to its client.

## Servers ##

Servers in the XMPP network route data, but also have a number of other responsibilities including maintaining session state, storing client data (chat history, files, messages sent when no client was online to receive them, contact lists, etc.). They are where most of the business logic of handling an XMPP connection lives. This allows clients to remain as "dumb" as possible (containing very little logic).

## Visualizing the XMPP Network as a Graph
The XMPP network can be thought of as a bidirected graph with servers (S) operating in a mesh, clients (C) clustered about their local server, and streams represented by extraverted edges:

[![Federated server architecture][1]][1]

When a client wants to send data (eg. a message or presence information) across the network to another client, the message is always routed along the shorted possible path (from a client to its server, then to the remote client if they are on the same server or to the remote clients server and then to the client if the remote client is on a different server).

  [1]: http://i.stack.imgur.com/rKQIi.png

