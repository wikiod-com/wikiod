---
title: "WSO2 ESB - Local entries"
slug: "wso2-esb---local-entries"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

**In WSO2 registry is a content store and a metadata repository.** 

In WSO2 products and particularly WSO2 ESB uses registry to store metadata, artifacts (WSDL, XSD etc.,) and other service configurations like endpoints, sequences etc.,

Registry in WSO2 ESB has three flavours. 
 - Local registry
 - Configuration registry
 - Governance registry

**Local Registry:** 
    It can be used to store configurations and metadata that is specific to the ESB node. This cannot be shared across all ESB nodes in the cluster or peer ESB nodes. 
    
    The mount path of local registry is  /_system/local

The local registry entries or local entries are very useful to store Strings, URLs, XSD / XSLT which can be easily retrieved from a mediator. 
These local entries can be accessed from the mediators / sequences by the following entry. 

    <localEntry key="validate_schema">

The local entries can be accessed from the ESB management console by the following way and artifacts can be added easily. 

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/pya06.jpg



## Example to retrieve local entry from sequences
In WSO2 registry is a content store and a metadata repository. 

In WSO2 products and particularly WSO2 ESB uses registry to store metadata, artifacts (WSDL, XSD etc.,) and other service configurations like endpoints, sequences etc.,

Registry in WSO2 ESB has three flavours. 

 - Local registry 
 - Configuration registry 
 - Governance registry

**Local Registry:** 
    It can be used to store configurations and metadata that is specific to the ESB node. This cannot be shared across all ESB nodes in the cluster or peer ESB nodes. 
    
*The mount path of local registry is  /_system/local*

The local registry entries or local entries are very useful to store Strings, URLs, XSD / XSLT which can be easily retrieved from a mediator. 
These local entries can be accessed from the mediators / sequences by the following entry. 

    <localEntry key="validate_schema_XSLT">

The local entries can be accessed from the ESB management console by the following way and artifacts can be added easily. 

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/6SgyO.jpg

