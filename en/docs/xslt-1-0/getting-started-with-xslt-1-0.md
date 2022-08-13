---
title: "Getting started with xslt-1.0"
slug: "getting-started-with-xslt-10"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Identity transformation using the "identity rule"

This example shows the base of almost any XSLT transformation and the most fundamental XSLT design pattern. Producing as output an XML document that is identical to the source XML document.

**Source XML document**:

    <t>Hello, World!</t>

**XSLT transformation**:

    <xsl:stylesheet version="1.0"  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
     <xsl:output omit-xml-declaration="yes" indent="yes"/>
     
      <xsl:template match="@*|node()">
        <xsl:copy>
          <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
      </xsl:template>
    </xsl:stylesheet>

**Result**: When applying this transformation on *any* source XML document, the output is an XML document that is *identical* to the source XML document. In this case:

    <t>Hello, World!</t>

**Do note**:

 1. Using and overriding the *identity rule* is the most fundamental XSLT design pattern. This leads to simple, short and elegant solutions to fundamental tasks, such as deletion/insertion/renaming of elements, and a lot more.

 2. The *identity rule/template* is the one published in the **[W3C XSLT 1.0 Specification][1]**


  [1]: http://www.w3.org/TR/xslt#copying

## Installation or Setup
An XSLT processor is necessary in order to perform any XSLT transformation. It can usually be installed via system's package manager. E.g.  in Debian it can be installed with:

    sudo apt-get install xsltproc


## Minimal "Hellow World" transformation
This is a minimal possible XSLT transformation. It produces the string value of the source XML document. The output format is `text`.


**Source XML document**:

    <t>Hello, World!</t>

**XSLT transformation**:

    <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
      <xsl:output method="text"/>
    </xsl:stylesheet>

**Result** of applying the transformation on the source XML document specified above:

    Hello, World!

**Do note**:

 1. No `<xsl:template>` declaration is used.

 2. The wanted type of output is specified in the `<xsl:output>` declaration, as the value of its *method* attribute.

 2. When there are no matching templates the XSLT processor, following the rules of the XSLT processing model, applies the standard XSLT *built-in templates* and this results in copying to the output of the concatenation of all text nodes in document order. In this simple case the source XML document has just one text node with string value the string "`Hello, World!`".

