---
title : google-cloud-messaging Tutorial
slug : google-cloud-messaging-tutorial
weight : 9994
draft : false
images : []
type : docs
---

**Google Cloud Messaging: Overview**

Google Cloud Messaging (GCM) is a free service that enables developers to send messages between servers and client apps. This includes downstream messages from servers to client apps, and upstream messages from client apps to servers.

For example, a lightweight downstream message could inform a client app that there is new data to be fetched from the server, as in the case of a "new email" notification. For use cases such as instant messaging, a GCM message can transfer up to 4kb of payload to the client app. The GCM service handles all aspects of queueing of messages and delivery to and from the target client app.

---

**Architectural Overview**

A GCM implementation includes a Google connection server, an app server in your environment that interacts with the connection server via HTTP or XMPP protocol, and a client app.

![Architectural Overview](https://developers.google.com/cloud-messaging/images/notifications-overview.svg)

Here's how these components interact:

- Google **GCM Connection Servers** accept downstream messages from your app server and send them to a client app. The [XMPP](https://developers.google.com/cloud-messaging/ccs.html) connection server can also accept messages sent upstream from the client app and forward them to your app server. For more information, see [About GCM Connection Server](https://developers.google.com/cloud-messaging/server).
- On your **App Server**, you implement the [HTTP](https://developers.google.com/cloud-messaging/http.html) and/or [XMPP](https://developers.google.com/cloud-messaging/ccs) protocol to communicate with the GCM connection server(s). App servers send downstream messages to a GCM connection server; the connection server enqueues and stores the message, and then sends it to the client app. If you implement XMPP, your app server can receive messages sent from the client app.
- The **Client App** is a GCM-enabled client app. To receive and send GCM messages, this app must register with GCM and get a unique identifier called a registration token. For more information on how to implement the client app, see the documentation for your platform.

---

**Key Concepts**

Below summarizes the key terms and concepts involved in GCM. It is divided into these categories:

- **Components** — The entities that play a primary role in GCM.
- **Credentials** — The IDs and tokens that are used in GCM to ensure that all parties have been authenticated, and that the message is going to the correct place.

GCM components and credentials.

**Components**

- **GCM Connection Servers** - Google servers involved in sending messages between the app server and the client app.
- **Client App** - A GCM-enabled client app that communicates with your app server.
- **App Server** - An app server that you write as part of implementing GCM. The app server sends data to a client app via the GCM connection server. If your app server implements the XMPP protocol, it can also receive messages sent upstream from client apps.

**Credentials**

- **Sender ID**<br>
A unique numerical value created when you configure your API project. The sender ID is used in the [registration process](https://developers.google.com/cloud-messaging/registration) to identify an app server that is permitted to send messages to the client app.
- **Server key**<br>
A key saved on the app server that gives the app server authorized access to Google services. In HTTP, the server key is included in the header of POST requests that send messages. In XMPP, the server key is used in the SASL PLAIN authentication request as a password to authenticate the connection. Do not include the server key anywhere in your client code. You obtain the server key when you create your API project.
- **Application ID**    
The client app that is registering to receive messages. How this is implemented is platform-dependent:

  - **Android**: use the package name from the app manifest.
  - **iOS**: use the app's bundle identifier.
  - **Chrome**: use the Chrome extension name.

- **Registration Token**<br>
An ID issued by the GCM connection servers to the client app that allows it to receive messages. Note that registration tokens must be kept secret.

---

**Lifecycle Flow**

- **Register to enable GCM**. An instance of a client app registers to receive messages. For more discussion, see [Registering Client Apps](https://developers.google.com/cloud-messaging/registration).
- **Send and receive downstream messages**.
  - Send a message. The app server sends messages to the client app:
    1. The app server [sends a message](https://developers.google.com/cloud-messaging/server.html#send-msg) to GCM connection servers.
    2. The GCM connection server enqueues and stores the message if the device is offline.
    3. When the device is online, the GCM connection server sends the message to the device.
    4. On the device, the client app receives the message according to the platform-specific implementation. See your platform-specific documentation for details.
  - Receive a message. A client app receives a message from a GCM connection server. See your platform-specific documentation for details on how a client app in that environment processes the messages it receives.
- **Send and receive upstream messages**. This feature is only available if you're using the [XMPP connection server](https://developers.google.com/cloud-messaging/ccs.html).
  - Send a message. A client app sends messages to the app server:
    1. On the device, the client app sends messages to the XMPP connection server. See your platform-specific documentation for details on how a client app can send a message via XMPP.
    2. The XMPP connection server enqueues and stores the message if the server is disconnected.
    3. When the app server is re-connected, the XMPP connection server sends the message to the app server.
  - Receive a message. An app server receives a message from the XMPP connection server and then does the following:
    1. Parses the message header to verify client app sender information.
    2. Sends "ack" to the XMPP connection server to acknowledge receiving the message.
    3. Optionally parses the message payload, as defined by the client app.

---

Official Documentation Reference can be found [here](https://developers.google.com/cloud-messaging/gcm).

