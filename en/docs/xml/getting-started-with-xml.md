---
title: "Getting started with xml"
slug: "getting-started-with-xml"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Well-formedness
An XML document is a text file that conforms to the XML specification's well-formedness rules. Such a conforming document is said to be *well-formed* (not to be confused with *valid*). XML is very strict with well-formedness in comparison to other languages such as HTML. A text file that is not well-formed is not considered XML and cannot be used by consuming applications at all.

Here are some rules that apply to XML documents:

 1. XML uses a much self-describing syntax. A prolog defines the XML version and the character encoding:

        <?xml version="1.0" encoding="UTF-8"?>

 2. There must be exactly one top-level element.

       <root>
           <!-- the rest of the XML document -->
       </root>

    However, comments, processing instructions, as well as the initial XML declaration, are allowed at the top-level as well. Text and attributes are not.

        <?xml version="1.0"?>
        <!-- some comments -->
        <?app a processing instruction?>
        <root/>
        <!-- some more comments -->

 3. Elements may nest, but must be "properly nested":

        <name>
          <first-name>John</first-name>
          <last-name>Doe</last-name>
        </name>
 
    The start and end tags of an embedded element have to be within the start and end tags of its container element. An overlapping of elements is illegal. 
In particular, this is  not well-formed XML: `<foo><bar></foo></bar>`

 4. Attributes may only appear in opening element tags or empty element tags, not in closing element tags. If attribute syntax appears between elements, it has no meaning and is parsed as text.

        <person first-name="John" last-name="Doe"/>

    This is not well-formed: <strike>`<person></person first-name="John"/>`</strike>

 5. Comments, processing instructions, text and further elements can appear anywhere inside an element (i.e., between its opening and closing tag) but not inside the tags.

        <element>
            This is some <b>bold</b> text.
            <!-- the b tag has no particular meaning in XML -->
        </element>

    This example is not well-formed: <strike>`<element <-- comment --> />`</strike>

 6. The `<` character may not appear in text, or in attribute values.

 7. The `"` character may not appear in attribute values that are quoted with `"`. The `'` character may not appear in attribute values that are quoted with `'`.

 8. The sequence of characters `--` may not appear in a comment.
 9. Literal `<` and `&` characters must be escaped by their respective entities `&lt;` and `&amp;`.

## Installation or Setup
XML is a syntax, which means a simple text editor is enough to get started.

However, having an XML-specific editor that shows you when and where your document is not well-formed is almost indispensable for productivity. Such editors may also allow you to validate XML documents against an XML Schema, or even generate XML Schemas from XML documents (and vice versa). 

Some examples of editors are oXygen, Atom, Eclipse and Altova XMLSpy. An alternate solution is to use a command-line XML parser such as Apache Xerces.


## The basic building blocks
XML is made of basic building blocks, which are:

- element
- text
- attributes
- comments
- processing instructions

An element has angle brackets:

    <element/>

    <element>some content</element>

An attribute appears in an opening element tag:

    <element
      attribute-name="attribute value"
      other-attribute='single-quoted value'>
      ...
    </element>

Text can appear anywhere within or between elements:

    <element>some more <b>bold</b> text</element>

Comments use the following syntax. It is important to know that XML comments, unlike in programming languages, are part of the model, and will be visible to the application above the parser.

    <!-- this is a comment -->

Processing instructions allow passing messages to the consuming application (e.g., how to display, or a stylesheet, etc). XML does not restrict the format of processing instructions.

    <?target-application these are some instructions?>

More details on building blocks can be found [in this topic](https://www.wikiod.com/xml/building-blocks)

## Hello World
    <?xml version="1.0"?>
    <?speech-generator voice="Siri"?>
    <root xmlns:vocabulary="http://www.example.com/vocabulary">
      <!-- These are the standard greetings -->
      <vocabulary:greetings>
        <vocabulary:greeting xml:lang="en-US" type="informal">
          Hi!
        </vocabulary:greeting> 
        <vocabulary:greeting xml:lang="en-US" type="intermediate">
          Hello!
        </vocabulary:greeting> 
        <vocabulary:greeting xml:lang="en-US" type="formal">
          Good morning to <b>you</b>!
        </vocabulary:greeting> 
      </vocabulary:greetings>
    </root>

## Namespaces
Element and attribute names live in namespaces that are URIs. Namespaces are bound to prefixes that are used in the actual element and attribute names, which are called QNames.

This document binds a namespace to the prefix `prefix` and defines a default namespace, bound with the absence of prefix.

    <?xml version="1.0"?>
    <document
        xmlns="http://www.example.com/default-namespace"
        xmlns:prefix="http://www.example.com/another-namespace">
      <prefix:element/>
    </document>

More details on namespaces can be found [in this topic](https://www.wikiod.com/xml/namespaces)

