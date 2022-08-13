---
title: "Getting started with xpath"
slug: "getting-started-with-xpath"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Testing Xpaths in browser console
A quick way to test your xpath is in your browser developer tool console.

Format is 

       $x('//insert xpath here')

$ - specifies it is a selector.

x - specifies it is using xpaths

Example:

    $x("//button[text() ='Submit']")

When this command is entered it will return all occurrences of elements that are buttons with text equal to Submit. 

## Select text
For the sample XML (without namespaces):

This XPath,

    /r/f/text()

will select the text node with this string value:

    "Text 1"

And this XPath,

    string(/r/f)

will return the string value of `f`, which is also:

    "Text 1"



## Select an element
For the sample XML (without namespaces):

This XPath,

    /r/e

will select this element:

    <e a="1"/>


## Common HTML Operations
If the input HTML DOM is

    <html>
        <body>
            <a>link</a>
            <div class='container' id='divone'>
                <p class='common' id='enclosedone'>Element One</p>
                <p class='common' id='enclosedtwo'>Element Two</p>
            </div>
        </body>
    </html>


Find an element with a specific id in the entire page

    //*[@id='divone'] # Returns <div class='container' id='divone'>

Find an element with a specific id in a particular path

    /html/body/div/p[@id='enclosedone'] # Returns <p class='common' id='enclosedone'>Element One</p>

Select an element with a particular id & class

    //p[@id='enclosedone' and @class='common'] # Returns <p class='common' id='enclosedone'>Element One</p>

Select the text of a particular element

    //*[@id='enclosedone']/text() # Returns Element One

## Sample XML (without namespaces)
Here is some sample XML against which example XPaths can be written:

    <r>
      <e a="1"/>
      <f a="2" b="1">Text 1</f>
      <f/>
      <g>
        <i c="2">Text 2</i>
        Text 3
        <j>Text 4</j>
      </g>
    </r>


