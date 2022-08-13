---
title: "DTD"
slug: "dtd"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

XML Document Type Declaration commonly known as DTD is a way to describe precisely the XML language. DTDs check the validity of, structure and vocabulary of an XML document against the grammatical rules of the appropriate XML language. A DTD defines the structure and the legal elements and attributes of an XML document.


## XML document with an internal DTD
A DTD is referred to as an internal DTD if elements are declared within the XML files. To reference it as internal DTD, standalone attribute in XML declaration must be set to yes. 

An XML that describes a note that contains property to, from and message along with internal DTD will look like:

     <?xml version="1.0" encoding="utf-8" standalone="yes"?>
    <!DOCTYPE note [
    <!ELEMENT note (to,from,message>
    <!ELEMENT to (#PCDATA)>
    <!ELEMENT from (#PCDATA)>
    <!ELEMENT message (#PCDATA)>
    ]>
    <note>
    <to>Mr.X</to>
    <from>Mr.Y</from>
    <message>Stack Overflow is awesome </message>
    </note> 

## XML document with an external DTD
In external DTD elements are declared outside the XML file. They are accessed by specifying the system attributes which may be either the legal .dtd file or a valid URL. To reference it as external DTD, the standalone attribute in the XML declaration must be set as no.

An XML that describes a note that contains property to, from and the message is given below.

    <?xml version="1.0" encoding="UTF-8" standalone="no" ?>
    <!DOCTYPE note SYSTEM "note.dtd">
    <note>
      <to>Mr.X</to>
      <from>Mr.Y</from>
      <message>Stack Overflow is awesome</message>
    </note>
External DTD for the above XML, *note.dtd* is given below

    <!DOCTYPE note [
    <!ELEMENT note (to,from,message>
    <!ELEMENT to (#PCDATA)>
    <!ELEMENT from (#PCDATA)>
    <!ELEMENT message (#PCDATA)>
    ]>

 

## Document Type Declaration
An XML document can contain a DTD. DTD stands for *Document Type Declaration*. A DTD begins with `<!DOCTYPE root-element-name >` where `doc-element-name` must match the name of the so-called document element (the one element at the top-level).

    <?xml version="1.0"?>
    <!DOCTYPE document>
    <document>
      <!-- the rest of the document -->
    </document>


## Entities
A DTD can contain entity declarations.

    <?xml version="1.0"?>
    <!DOCTYPE document [
      <!ENTITY my-entity "This is the replacement text">
    ]>
    <document>
      <!-- the rest of the document -->
    </document>

Entities are described in details in [this topic](https://www.wikiod.com/xml/entities).

