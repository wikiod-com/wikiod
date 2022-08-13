---
title: "Getting started with lxml"
slug: "getting-started-with-lxml"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Why we need lxml and how to use it ?
**First, why do we need lxml ?**

lxml.etree is a generic API for XML and HTML handling. It aims for ElementTree compatibility and supports the entire XML infoset. It is well suited for both mixed content and data centric XML. Its generality makes it the best choice for most applications.

The lxml library, is an extension of the old libxml2 and libxsit and it has some major benefits:
1. Very easy python API
2. Well documented
3. No need to deal with memory management
4. No need to worry for segmentation fault

It is also provides a very natural way to deal with any XML data format. Data is automatically converted to Python data types and can be manipulated with normal Python operators

**Great! now how can I use it ?**

On Linux machines you can install the lxml library using apt-get:

    sudo apt-get install python-lxml

To import and use the library:

    from lxml import etree

To parse the xml file, you can use:

        try:
            parser = ET.XMLParser(remove_comments=False, remove_blank_text=True)
            tree = ET.parse(file, parser=parser)
        except (Exception):
            print ('Failed to open file %s' % file, exc_info=True)
        return tree


## lxml install
Installing lxml is very easy, had become an easy jobs since Python 2.7.9 (because it comes with an utility which helps developers to download install dependency in an easy manner like Maven for Java) at first you have to run the command then start coding.

    pip install lxml
The second way is to install using easy_install. More details instruction might be found [here][1]
    


  [1]: http://lxml.de/installation.html

## Installation or Setup
Detailed instructions on getting lxml set up or installed.

