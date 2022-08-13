---
title : wsdl Tutorial
slug : wsdl-tutorial
weight : 9996
draft : false
images : []
type : docs
---

A WSDL(Web Services Definition Language) document describes networking services by defining endpoints contained in a service in XML format. These endpoints are made up out of operations and messages, combined with the networking protocol used to access these endpoints

The WSDL does not contain any implementation details around these message and operations so it is in some ways analogous a interface in programming languages. However the WSDL does go a step further by specifying the concrete network protocol used by the network service to communicate. The endpoints are formed by combining the operations and messages with the network protocol.

A WSDL is extensible to allow descriptions of endpoints, messages and operations regardless of what message format or networking protocols are used however typically the most common bindings seen are:

 1. SOAP
 2. HTTP GET/POST
 3. MIME

A WSDL document is essential when dealing with SOAP services as it allows clients of SOAP services to understand how they will talk to the service(network protocol) and what functionality service offers(end points). The document also specifies how the messages will be structured.





