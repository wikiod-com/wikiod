---
title: "Getting started with jaxb"
slug: "getting-started-with-jaxb"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Code generation from XSD
JAXB can be used to generate classes from an model defined in XSD.  It will then be possible to read XML document made against this XSD directly as java instances and inversly save these instances as XML document.  

Take the following XSD saved in a file named SimpleModel.xsd

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>
    <schema xmlns="http://www.w3.org/2001/XMLSchema"
     targetNamespace="http://myCorp/schemas/simpleModel"
     xmlns:simple="http://myCorp/schemas/simpleModel"
     elementFormDefault="qualified"
     attributeFormDefault="unqualified">
     
     <complexType name="Person">
         <sequence>
             <element name="FirstName" type="string"/>
             <element name="LastName" type="string"/>
             <element name="DateOfBirth" type="dateTime"/>
         </sequence>
     </complexType>
     
    </schema>

You can use JAXB to generate classes automatically to match this XSD using this command line (provided your JDK's bin folder is on your path)

<!-- language: lang-bat -->

    xjc SimpleModel.xsd

This will generate a package based on the namespace of your XSD (here mycorp.schemas.simplemodel) with the following classes :

* ObjectFactory.java
* package-info.java
* Person.java

The **ObjectFactory** is used to create instances of the class(es) that were generated.  In some cases this seems like a trivial wrapper around a 
<!-- language: lang-java -->
    new Person();

But in more complex cases it will create the proper [wrappers][1] around your instances providing the missing link to properly [marshall][2] and [unmarshall][3] the objects to and from XML.

The **package-info.java** contains information about the XSD in general.

All other files are classes derived from the model described in the XSD.  Here we only have **Person.java** since there is only one object in our model.

Using other command line [arguments][4] Jaxb and XJC will give you immense power over the generated code.  XJC also provide means to use or create plugins to go beyoond and do things like :

* Have the generated code implement an interface or extend a class.
* Automatically generate toString, hashcode, equals etc with the class.
* Automatically map between xml types (simple or complex) and JavaType.
* Inject custom code or annotations in the generated code.

And much much more

You can also use other tools to interact with xjc for you, Maven plugins (at least 4 that I know of), Ant task etc.  Often times these tools can perform things that would be difficult to get with just the Reference implementation thorugh command line.


  [1]: http://docs.oracle.com/javaee/5/api/javax/xml/bind/JAXBElement.html
  [2]: https://docs.oracle.com/javase/7/docs/api/javax/xml/bind/Marshaller.html
  [3]: http://docs.oracle.com/javaee/5/api/javax/xml/bind/Unmarshaller.html
  [4]: https://jaxb.java.net/2.2.4/docs/xjc.html

## Installation or Setup
The JAXB Reference Implementation (JAXB-RI) has been included with the Java Development Kit since JDK 6 update 3.

Refer to the [Unofficial JAXB Guide][1] for additional details on which JAXB-RI version is included with specific versions of the JDK.


  [1]: https://jaxb.java.net/guide/Which_JAXB_RI_is_included_in_which_JDK_.html

