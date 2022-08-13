---
title: "WCF Security"
slug: "wcf-security"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## WCF Security
Security is a critical piece of any programming technology or framework for implementing
service - oriented applications

WCF has been built from the ground up for
providing the necessary security infrastructure at the message and service level.

In the following sections, you see how to use
many of the available security settings in WCF, and some common deployment scenarios.

For message protection, WCF supports the two traditional security models, transport security
and message security.

The bindings, in addition to specifying the communication protocol and encoding for the services,
will also allow you to confi gure the message protection settings and the authentication schema.


**Default Security Settings in WCF:**

| BINDING | SETTINGS |
| ------ | ------ |
| WsHttpBinding   | Message Security with Windows Authentication   |
| BasicHttpBinding| No Security|
|WsFederationHttpBinding |Message Security with Federated Authentication|
|NetTcpBinding|Transport Security with Windows Authenticatio|
|NetNamedPipeBinding|Transport Security with Windows Authentication|
|NetMsmqBinding|Transport Security with Windows Authentication|

consider following example:

  

     <wsHttpBinding >
       <binding name=”UsernameBinding” >
        <security mode=”Message” >
          <message clientCredentialType=”UserName”/ >
        </security >
       </binding >
     </wsHttpBinding >

In this example, the service has been confi gured with message security and the username security
token profi le. The rest of the security settings for the binding take the default values.

**Security Mode**

The security mode setting determines two fundamental security aspects for any service: the security
model for message protection and the supported client authentication schema.

| Security MODE | Description |
| ------ | ------ |
| None   | The service is available for anyone, and the messages are not protected as they go through the transport. When this mode is used, the service is vulnerable to any kind of attack.   |
| Transport   | Uses the transport security model for authenticating clients and protecting the messages. This mode provides the advantages and disadvantages discussed in transport security.   |
| Message     | Uses the message security model for authenticating clients and protecting the messages. This mode provides the advantages and disadvantages discussed in message security.   |
| Both   | Uses the transport security and message security models at the same time for authenticating the service consumers and protecting the messages. This mode is only supported by the MSMQ bindings and requires the same credentials at both levels.   |
| TransportWithMessageCredentials   | The message protection is provided by transport, and the credentials for authenticating the service consumers travel as part of the message. This mode provides the flexibility of using any of the credentials or token types supported in message authentication while the service authentication and message protection is performed at transport level.   |
| TransportCredentialOnly   | Uses transport security for authenticating clients The service is not authenticated, and the messages,including the client credentials, go as plain text through the transport. This security mode can be useful forscenarios where the kind of information transmitted between the client and the service is not sensitive, although the credentials also get exposed to anyone.   |





## Configure the WsHttpBinding to use transport security with Basic Authentication

    <bindings >
      <wsHttpBinding >
       <binding name="mybinding" >
        <security mode="Transport" >
         <transport clientCredentialType="Basic"/ >
        </security >
       </binding >
      </wsHttpBinding >
    </bindings >

