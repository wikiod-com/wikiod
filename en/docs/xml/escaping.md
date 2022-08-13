---
title: "Escaping"
slug: "escaping"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Characters can be escaped in XML using entity references and character references, or CDATA sections.

XML pre-defines five entities:

|Named entity|Replacement text|
|-----|-----|
|amp|&|
|quot|"|
|apos|'|
|lt|<|
|gt|\>|

Consuming applications will not know whether each character has been escaped or not, and how.

## Ampersand
The `&` character appears first in entity references and must be escaped in element content or in attribute content.

    <?xml version="1.0"?>
    <document attribute="An ampersand is escaped as &amp;">
      An ampersand can also be escaped as &amp; in element content.
    </document>


## Lower-than sign
The `<` character appears first in entity tags and must be escaped in element content or in attribute content.

    <?xml version="1.0"?>
    <document attribute="A lower-than sign is escaped as &lt;">
      2 + 2 &lt; 5
    </document>


## Greater-than sign
The `]]>` character sequence is not allowed in element content. The easiest way to escape it is to escape `>` as `&gt;`.

    <?xml version="1.0"?>
    <document>
      The sequence ]]&gt; cannot appear in element content.
    </document>


## Apostrophes and quotes
Attribute values can appear in simple or double quotes. The appropriate character must be escaped.

    <?xml version="1.0"?>
    <document
      quot-attribute="This is a &quot;double quote&quot; and this one is 'simple'"
      apos-attribute='This is a &apos;simple quote&apos; and this one is "double"'>
    </document>

## CDATA sections
Longer portions of text containing special characters can be escaped with a CDATA section. CDATA sections can only appear in element content.

    <?xml version="1.0"?>
    <document>
      This is a CDATA section : <![CDATA[ plenty of special characters like & < > " ; ]]>
    </document>

A CDATA section cannot contain the sequence `]]>` because it ends it.

## Character references
Characters can be escaped using character references, in element content or attribute values. Their Unicode codepoint can be specified in decimal or hex.

    <?xml version="1.0"?>
    <document>
      The line feed character can be escaped with a decimal (&#10;) or hex (&#xA;)
      representation of its Unicode codepoint (10).
    </document>

XML restricts characters that can appear in a document, even escaped. In particular, the only control characters allowed are line feed (10), carriage return (13) or horizontal tab (9).

