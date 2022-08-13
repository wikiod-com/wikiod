---
title: "Getting started with xpages"
slug: "getting-started-with-xpages"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Use external texts with message.properties in XPages
## Prepare ##
First create a "`message.properties`" file in Resources/Files/. Example:

    ##############
    # Test message.properties
    ##############
    label.age=Enter your age:
    validate.error.reqired.age=Sorry, but you have to give away the secret of your age ...



Next, connect the resource with your XPage or Custom Control:

<!-- language: lang-xml -->

    <xp:this.resources>
        <xp:bundle src="/messages.properties" var="appMsg" />
        ....
    </xp:this.resources>

*Note: The "var" defines the name you want to use in your XPages or Custom Controls to reference the message map.*

## Usage ##
Now you can use the message map with server-side JavaScript (`#{javascript:appMsg.getString('...')}`) or with EL (`#{appMsg['...']}`).


Example usage:
<!-- language: lang-xml -->

    ...
    <!-- to show the error message: -->
    <xp:messages />
    
    <!-- use with ssjs: -->
    <xp:text value="#{javascript:appMsg.getString('label.age')}" escape="false" />

    <!-- use with EL: -->
    <xe:djNumberSpinner value="#{myDoc.age}" maxLength="2" javaType="int">
        <xp:this.validators>
            <xp:validateRequired message="#{appMsg['validate.error.reqired.age']}" />
        </xp:this.validators>
    </xe:djNumberSpinner>
    ...



## Installation or Setup
In short:
XPages is part of IBM Domino Designer. Extra setup or installation isn't required for XPages.


----------

First XPage / Hello-World-Example
-----------

To create your first XPage you have to create a new NSF first. Open the IBM Domino Designer, and open the menu "**File**" -> "**New**" -> "**Application**". 

In the popup Dialog make these settings:

 1. Select the server where the NSF will be created (can also be "local"). 
 1. Then enter a title, for example "**`Hello World NSF`**".
 1. Then enter a file name of your new NSF, for example "**`hello-world.nsf`**".
 1. Ignore the "Encription ..." button to keep the default settings.
 1. Check the option "**full index**".
 1. Then click "OK".

The new NSF is created.
 
 
Now right-click on section "[XPages]" in the application navigator and select "**new XPage ...**". 

 1. Enter a title of your new XPage, for example "**`HelloWorld`**". This will create a file named "HelloWorld.xsp".
 1. The comment field can be left empty for this simple example.
 1. Click "OK", and the page is created.


Double-click at your new HelloWorld XPage, which you can find under the "[XPages]" section.

Select the tab "Source" (which is located at the bottom of the editor), and add a simple textfield component to your page. This should be the result:

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>
    <xp:view xmlns:xp="http://www.ibm.com/xsp/core" pageTitle="Hello My World">
    
      <xp:text id="simpleTextField" value="Hello World!!!!" />

    </xp:view>

Save the page and build the project (right-click at your application "Hello World NSF" and select menu entry "build").

Now, open a browser like Internet Explorer, and navigate to the new XPage of your application's NSF. For example "http://mydominoserver.com/hello-world.nsf/HelloWorld.xsp" and you will see your Hello World text.

