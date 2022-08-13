---
title: "Entities"
slug: "entities"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

From a storage perspective, an XML document is made of entities. One of the entities is the document entity, which is the main XML document itself.

Entities can be classified like so (tentatively sorted by descending order of usage):
- **document entity**: this is the main XML file.
- **internal general entities**: this is the most common one besides the document entity, and the one most XML users are aware of. Often, the word **entity** is casually used for them. They allow specifying some shortcuts for longer replacement texts in document content. They are declared in the DTD.
- **the external DTD subset**: another file in which part of the DTD is outsourced.
- **parameter entities**: shortcuts, for use in the DTD.
- **external parsed general entities**: they are XML fragments stored in other files.
- **unparsed entities**: these can be any files on which XML places no restrictions, including images, sounds, etc.

In many cases, an XML document consists solely of the document entity.



## User-defined general (internal) entities
It is possible to define one's own general entities. The declaration occurs in the DTD subset, with a name and the associated replacement text.

It can then be used in the document using the entity reference syntax `&...;`, either in text, or in attribute values.

    <?xml version="1.0"?>
    <!DOCTYPE my-document [
      <!ENTITY my-entity "This is my entity">
    ]>
    <my-document>
        The entity was declared as follows: &my-entity;
        <element attribute="Entity: &my-entity;"/>
    </my-document>


## External parsed entities
XML fragments, also known under the name of *external parsed entities*, can be stored in separate files.

XML fragments, unlike XML documents, are less restrictive, in that several elements can appear top-level, as well as text nodes. Like an XML document, an external parsed entity may begin with an XML declaration, but this declaration is not considered part of its replacement text.

This is an example of external parsed entity:

    <?xml version="1.0" encoding="UTF-8"?>
    This is some text
    <element/>
    <element/>

An external parsed entity can then be declared in an XML document, in the DTD, and it can be used with an entity reference, which has the same syntax as for general internal entities:

    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE root [
    <!ENTITY fragment SYSTEM "fragment.xml">
    ]>
    <root>
        &fragment;
    </root>

With the entity reference resolved, this document is equivalent to:

    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE root [
    <!ENTITY fragment SYSTEM "fragment.xml">
    ]>
    <root>
      This is some text
      <element/>
      <element/>
    </root>

Every opening element tag in an external parsed entity must have a corresponding ending tag: it is not allowed to spread single elements over multiple entities, nor to spread markup.

A validating parser is required to resolve the entity reference and include its replacement text in the document as above. A non-validating parser may skip this, and instead tell the consuming application that there is an unresolved reference to an external parsed entity.

## Pre-defined general entities
XML pre-defines five general entities that can be used without declaring them:

    & " ' < >

They are associated with the names `amp`, `quot`, `apos`, `lt` and `gt`.

    <?xml version="1.0"?>
    <entities>
      &amp; is an ampersand.
      &quot; is a quote.
      &apos; is an apostrophe.
      &lt; is a lower-than sign.
      &gt; is a greater-than sign. 
    </entities>

