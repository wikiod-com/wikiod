---
title: "XML Handling"
slug: "xml-handling"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Beautify or Pretty-Print XML
The [`PrettyPrinter`][1] utility will 'pretty print' XML documents. The following code snippet pretty prints unformatted xml:

    import scala.xml.{PrettyPrinter, XML}
    val xml = XML.loadString("<a>Alana<b><c>Beth</c><d>Catie</d></b></a>")
    val formatted = new PrettyPrinter(150, 4).format(xml)
    print(formatted)

This will output the content using a page width of `150` and an indentation constant of `4` white-space characters:

    <a>
        Alana
        <b>
            <c>Beth</c>
            <d>Catie</d>
        </b>
    </a>

You can use `XML.loadFile("nameoffile.xml")` to load xml from a file instead of from a string.

[1]: http://www.scala-lang.org/api/2.11.8/scala-xml/#scala.xml.PrettyPrinter

