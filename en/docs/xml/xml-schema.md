---
title: "XML Schema"
slug: "xml-schema"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

XML Schema is commonly known as XML Schema Definition (XSD). It is used to describe and validate the structure and the content of XML data. XML schema defines the elements, attributes and data types. 

## An Example of XSD Document
An XSD that describe a contact information about a company is given below. 

    <?xml version="1.0" encoding="UTF-8"?>
    <xs:schema targetNamespace="http://NamespaceTest.com/CommonTypes"
                      xmlns:xs="http://www.w3.org/2001/XMLSchema"
                      elementFormDefault="qualified">
    <xs:element name="contact">
        <xs:complexType>
            <xs:sequence>
                <xs:element name="name" type="xs:string" />
                <xs:element name="company" type="xs:string" />
                <xs:element name="phone" type="xs:int" />
            </xs:sequence>
        </xs:complexType>
    </xs:element>
    </xs:schema>
In the above example the attributes in second line<br> 
><xs:schema targetNamespace="http://NamespaceTest.com/CommonTypes"
                  xmlns:xs="http://www.w3.org/2001/XMLSchema"<br>
                  elementFormDefault="qualified">

Attributes 'targetnamespace' and elementFormDefault are optional.  


