---
title: "InputBox"
slug: "inputbox"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Syntax
+ InputBox(prompt[, title][, default][, xpos][, ypos][, helpfile, context])

## Parameters
|Argument|Detail|
|--------|------|
|`prompt`|Text to display above the input field (usually an instruction as to what is required form the user).|
|`title`|Caption displayed in the titlebar of the input box.|
|`default`|A placeholder for the text field, used as the return value if the user doesn't overwrite.|
|`xpos`|Horizontal distance in twips to display input box from the left edge of the screen.|
|`ypos`|Vertical distance in twips to display input box from the top edge of the screen.|
|`helpfile`|A string to determine the help file to be used in order to provide contextual help with the input box.|
|`context`|If the helpfile argument is supplied, a context number must also be supplied to identify the appropriate help topic in the help file.|


_<sup>1</sup>All arguments except `prompt` are optional._  
_<sup>2</sup>`helpfile` and `context` are coupled - if one is supplied, the other must also be supplied._

## Use InputBox to assign user input to a string

<!-- language-all: lang-vb -->

    ' Omitting the 4th and 5th argument ("xpos" and "ypos") will result in the prompt
    ' being display center of the parent screen

    exampleString = InputBox("What is your name?", "Name Check", "Jon Skeet", 2500, 2000)

    WScript.Echo "Your name is " & exampleString


