---
title: "Authentication Mechanisms in MongoDB"
slug: "authentication-mechanisms-in-mongodb"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

Authentication is the process of verifying the identity of a client. When access control, i.e. authorization, is enabled, MongoDB requires all clients to authenticate themselves in order to determine their access.

MongoDB supports a number of authentication mechanisms that clients can use to verify their identity. These mechanisms allow MongoDB to integrate into your existing authentication system.





## Authentication Mechanisms
MongoDB supports multiple authentication mechanisms.

**Client and User Authentication Mechanisms**

 - SCRAM-SHA-1 

 - X.509 Certificate Authentication

 - MongoDB Challenge and Response (MONGODB-CR)

 - LDAP proxy authentication, and

 - Kerberos authentication

 
 **Internal Authentication Mechanisms**

 - Keyfile 
 - X.509

