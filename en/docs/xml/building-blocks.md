---
title: "Building blocks"
slug: "building-blocks"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Attributes
Attributes are name-value pairs associated with an element.

They are represented by values in single or double quotes inside the opening element tag, or the empty element tag if it is an empty element.

    <document>
      <anElement foo="bar" abc='xyz'><!-- some content --></anElement>
      <anotherElement a="1"/>
    </document>

Attributes are not ordered (unlike elements). The following two elements have the same sets of attributes:

    <foo alpha="1" beta="2"/>

    <foo beta="2" alpha="1"/>

Attributes cannot be repeated in the same element (unlike elements). The following document is not well-formed: <strike>`<foo a="x" a="y"/>`</strike> because 
the attribute `a` appears twice in the same element.

The following document is well-formed. Values can be identical, it is the attribute name that can't be repeated.

    <foo a="x" b="x"/> 

Attributes cannot be nested (unlike elements).

## Text
Text is made of all characters outside of any markup (opening element tags, closing element tags, etc).

    <?xml version="1.0"?>
    <document>
      This is some text and <b>this is some more text</b>.
    </document>

The precise XML terminology for text is *character data*. The XML specification actually uses the word *text* for the entire XML document, or a parsed entity, because it defines XML at the syntactic level. However some data models such as the XDM (XQuery and XPath Data Model), which represent XML documents as trees, refer to character data as *text nodes*, such that *text* is often understood as a synonym for character data in practice.

Character data may not contain a `<` character -- this would be interpreted as the first character of an opening element tag -- neither can it contain the `]]>` character sequence. The appropriate characters must be escaped with an entity reference instead.

    <?xml version="1.0"?>
    <document>
      It is fine to escape the &lt; character, as well as ]]&gt;.
    </document>

For convenience, one can also escape a bigger chunk of text with a CDATA section (but the sequence `]]>` is still not allowed for obvious reasons):

    <?xml version="1.0"?>
    <document>
      <![CDATA[
        In a CDATA section, it is fine to write < or even & and entity references
        such as &amp; are not resolved.
      ]]>
    </document>


## Processing instructions
A processing instruction is used to directly pass on some information or instruction to the application via the parser.

    <?my-application some instructions ?>

The token after the initial question mark (here `my-application`) is called the target and identifies the application at which the instruction is aimed. What follows it is not further specified and it is up to the application to interpret it. Entity and character references are not recognized.

It can appear at the top-level, or in element content.

## Elements
Elements come with angle brackets are the most prominent building block of XML.

Elements can either be empty, in which case they are made of an empty tag (notice the ending slash):

    <an-empty-element/>

Or they can have content, in which case they have an opening tag (no slash) and a closing tag (beginning slash):

    <a-non-empty-element>Content</a-non-empty-element>

Elements can nest (but only between opening and closing tags):

    <parent-element>
      <child-element/>
      <another-child-element>
        Some more content.
      </another-child-element>
    </parent-element>

Element names are called QNames (qualified names). All above elements are in no namespace, but element names can also be defined in namespaces using prefixes like so:

    <my-namespace:parent-element xmlns:my-namespace="http://www.example.com/">
      <my-namespace:child-element/>
      <my-namespace:another-child-element>
        Some more content.
      </my-namespace:another-child-element>
    </my-namespace:parent-element>

Namespaces and element names are described in greater details [in this section of the documentation][1].


  [1]: https://www.wikiod.com/xml/namespaces

## Comments
Comments in XML look like so:

    <!-- This is a comment -->

They can appear in element content or top-level:

    <?xml version="1.0"?>
    <!-- a comment at the top-level -->
    <document>
      <!-- a comment inside the document -->
    </document>

Comments cannot appear inside tags or inside attributes:

   <strike>`<element <!-- comment with -- inside --> />`</strike>

or

<strike>`<element attr="<!-- comment with -- inside -->"/>`</strike>

are not well-formed.

The character sequence `--` cannot appear in the middle of a comment. This is not well-formed XML:

<strike>`<!-- comment with -- inside -->`</strike>

Comments in XML, unlike in other languages such as C++, are **part of the data model**: they are parsed, forwarded, and visible to the consuming application.

