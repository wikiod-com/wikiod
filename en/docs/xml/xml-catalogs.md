---
title: "XML Catalogs"
slug: "xml-catalogs"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

An XML catalog is made up of entries from one or more catalog entry files. A catalog entry file is an XML file whose document element is catalog and whose content follows the XML catalog DTD defined by OASIS at http://www.oasis-open.org/committees/entity/spec.html. Most of the elements are catalog entries, each of which serves to map an identifier or URL to another location.

## Catalog entry to resolve DTD location
<?xml version="1.0"?>
<!DOCTYPE catalog
   PUBLIC "-//OASIS/DTD Entity Resolution XML Catalog V1.0//EN"
   "http://www.oasis-open.org/committees/entity/release/1.0/catalog.dtd"> 1
<catalog  xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">  2
  <group  prefer="public"  xml:base="file:///usr/share/xml/" >  3

    <public 
       publicId="-//OASIS//DTD DocBook XML V4.5//EN"  4
       uri="docbook45/docbookx.dtd"/>

    <system
       systemId="http://www.oasis-open.org/docbook/xml/4.5/docbookx.dtd"  5
       uri="docbook45/docbookx.dtd"/>

    <system
       systemId="docbook4.5.dtd"    6
       uri="docbook45/docbookx.dtd"/>
  </group>
</catalog>

