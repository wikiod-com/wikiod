---
title: "Getting started with xquery"
slug: "getting-started-with-xquery"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Sum over values
Given the following XML document :

    <?xml version="1.0" encoding="UTF-8"?>
    <values>
      <value>1</value>
      <value>3</value>
      <value>5</value>
    </values>

We can produce an XML document describing the sum of the values with the following XQuery :

    <total>{sum(/values/value)}</total>

Which will result in the following document :

    <?xml version="1.0" encoding="UTF-8"?>
    <total>9</total>

## Installation or Setup
Detailed instructions on getting xquery set up or installed.

## Writing static XML
XML Data can be written as is in XQuery and will be found in the output.  
The following code can be considered valid XQuery :

    <application>
      <id>MyApp</id>
      <name>My Application</name>
      <version>1.0</version>
    </application>

Note that your XQuery code must produce a valid XML document and as such is restricted to output all its data in a single root tag.

Moreover, by default most XQuery implementations will add the XML header if you omit it. By example, the above code would produce this result :

    <?xml version="1.0" encoding="UTF-8"?>
    <application>
      <id>MyApp</id>
      <name>My Application</name>
      <version>1.0</version>
    </application>

## Extracting XML Data
To address data from an XML input, XQuery uses [XPath](https://www.wikiod.com/xpath).  
It makes it easy to filter data and restructure it.

Given the following XML input

    <?xml version="1.0" encoding="UTF-8"?>
    <applications>
      <application>
        <id>MyApp</id>
        <name>My Application</name>
        <version>1.0</version>
      </application>
      <application>
        <id>SomeApp</id>
        <name>Some Application</name>
        <version>4.2</version>
      </application>
      <application>
        <id>TheOtherApp</id>
        <name>That one</name>
        <version>13.37</version>
      </application>
    </applications>    

The following XQuery code will extract the application whose id is `MyApp` :

    /applications/application[id='MyApp']

It produces the following XML document :

    <?xml version="1.0" encoding="UTF-8"?>
    <application>
      <id>MyApp</id>
      <name>My Application</name>
      <version>1.0</version>
    </application>
   
And this code will extract the applications whose version is lower than 10, outputting them in a `<oldApplications>` tag :

        <oldApplications>{/applications/application[version < 10]}</oldApplications> 

It procudes the following XML document :

    <?xml version="1.0" encoding="UTF-8"?>
    <oldApplications>
      <application>
        <id>MyApp</id>
        <name>My Application</name>
        <version>1.0</version>
      </application>
      <application>
        <id>SomeApp</id>
        <name>Some Application</name>
        <version>4.2</version>
      </application>
    </oldApplications>   

