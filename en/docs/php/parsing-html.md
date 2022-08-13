---
title: "Parsing HTML"
slug: "parsing-html"
draft: false
images: []
weight: 9935
type: docs
toc: true
---

## Parsing HTML from a string
PHP implements a [DOM Level 2][1] compliant parser, allowing you to work with HTML using familiar methods like `getElementById()` or `appendChild()`.

    $html = '<html><body><span id="text">Hello, World!</span></body></html>';

    $doc = new DOMDocument();
    libxml_use_internal_errors(true);
    $doc->loadHTML($html);

    echo $doc->getElementById("text")->textContent;

Outputs:
    
    Hello, World!

Note that PHP will emit warnings about any problems with the HTML, especially if you are importing a document fragment. To avoid these warnings, tell the DOM library (libxml) to handle its own errors by calling [`libxml_use_internal_errors()`][2] before importing your HTML. You can then use [`libxml_get_errors()`][3] to handle errors if needed.


  [1]: https://www.w3.org/TR/2000/REC-DOM-Level-2-Core-20001113/http://
  [2]: http://php.net/manual/en/function.libxml-use-internal-errors.php
  [3]: http://php.net/manual/en/function.libxml-get-errors.php

## Using XPath
    $html = '<html><body><span class="text">Hello, World!</span></body></html>';

    $doc = new DOMDocument();
    $doc->loadHTML($html);

    $xpath = new DOMXPath($doc);
    $span = $xpath->query("//span[@class='text']")->item(0);

    echo $span->textContent;

Outputs:
    
    Hello, World!

## SimpleXML
# Presentation

 - SimpleXML is a PHP library which provides an easy way to work with XML documents (especially reading and iterating through XML data).

 - The only restraint is that the XML document must be well-formed.

# Parsing XML using procedural approach

    // Load an XML string
    $xmlstr = file_get_contents('library.xml');
    $library = simplexml_load_string($xmlstr);
    
    // Load an XML file
    $library = simplexml_load_file('library.xml');
    
    // You can load a local file path or a valid URL (if allow_url_fopen is set to "On" in php.ini

# Parsing XML using OOP approach

    // $isPathToFile: it informs the constructor that the 1st argument represents the path to a file,
    // rather than a string that contains 1the XML data itself.
    
    // Load an XML string
    $xmlstr = file_get_contents('library.xml');
    $library = new SimpleXMLElement($xmlstr);
    
    // Load an XML file
    $library = new SimpleXMLElement('library.xml', NULL, true);
    
    // $isPathToFile: it informs the constructor that the first argument represents the path to a file, rather than a string that contains 1the XML data itself.

# Accessing Children and Attributes
  - When SimpleXML parses an XML document, it converts all its XML elements, or nodes, to properties of the resulting SimpleXMLElement object
  - In addition, it converts XML attributes to an associative array that may be accessed from the property to which they belong.

## When you know their names:

    $library = new SimpleXMLElement('library.xml', NULL, true);
    foreach ($library->book as $book){
        echo $book['isbn'];
        echo $book->title;
        echo $book->author;
        echo $book->publisher;
    }

  - The major drawback of this approach is that it is necessary to know the names of every element and attribute in the XML document.

## When you don't know their names (or you don't want to know them):

    foreach ($library->children() as $child){
        echo $child->getName();
        // Get attributes of this element
        foreach ($child->attributes() as $attr){
            echo ' ' . $attr->getName() . ': ' . $attr;
        }
        // Get children
        foreach ($child->children() as $subchild){
            echo ' ' . $subchild->getName() . ': ' . $subchild;
        }
    }

