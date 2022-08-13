---
title: "Text Formatting"
slug: "text-formatting"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Text formatting in markdown usually requires characters at both the beginning and end of the text.

## Horizontal Rules
    You can create a horizontal break to divide your text by placing three (or more) underscores 
    
    ___
    
    or asterisks 
    
    ***
    
    or hyphens

    ---

    on their own line.

> You can create a horizontal break to divide your text by placing three (or more) underscores 
>    
> ___
>    
>or asterisks 
>    
>***
>
>or hyphens
>
>---
>    
>on their own line.

----

There can be spaces between the characters, and a horizontal rule can be immediately followed by another one:

    _ _ _
    * * * *
    and the spaces don't have to be evenly distributed
    *  ****

>_ _ _
>* * * *
>and the spaces don't have to be evenly distributed
>*  ****


## Bold
Bold text can be created by surrounding text with either double asterisks or double underscores:

    **Bolded text**

    __Also bolded text__

Result:

> **Bolded text**
> 
> __Also bolded text__

## Italic
Italics can be created by surrounding text with either asterisks or with underscores:

    *Italicized text*

    _Also italicized_

Result:

> *Italicized text*
>
> _Also italicized_

## Strikethrough
To create <s>strike-through text</s>, surround the text with `~~double tildes~~`.

Note: on StackExchange this formatting isn't included. Instead use the html tag `<s>text</s>`. (In chat you can use `---three hyphens---`.)

## Bold + Italic
    Creating ***bold italic*** text is simply a matter of using both
    **bold** (two asterisks) and *italic* (one asterisk) at the same time,
    for a total of three asterisks on either side of the text you want to format at once.

> Creating ***bold italic*** text is simply a matter of using both
    **bold** (two asterisks) and *italic* (one asterisk) at the same time,
    for a total of three asterisks on either side of the text you want to format at once.

## HTML
Some HTML tags can also be used in Markdown.

- `<b>bold</b>` <b>bold</b>
- `<i>italic</i>` <i>italic</i>
- `<a href="http://stackoverflow.com/">link</a>` <a href="http://stackoverflow.com/">link</a>
- `<kbd>Ctrl</kbd>` <kbd>Ctrl</kbd>

Named anchors can also be used to allow easier navigation within the document. Note that Stack Overflow Markdown doesn't seem to support this.

    <a name="heading"></a>
    # Heading 1
    Text under the heading
    Click on a link like [Go to Heading 1](#heading1) to go to that named anchor.

<a name="heading1"></a>
# Heading 1
Text under the heading
Click on a link like [Go to Heading 1](#heading1) to go to that named anchor.



## Subscript/Superscript
`x<sub>2</sub>` produces x<sub>2</sub>

`x<sup>2</sup>` produces x<sup>2</sup>

## Line Breaks and Paragraphs
End a line with two or more spaces to create a line break.

    Ending a line with no spaces
    or with just one space 
    doesn't create a line beak.  
    Use two or more spaces   
    to create a line break.
    
    Use an empty line to make a new paragraph.

> Ending a line with no spaces
> or with just one space
> doesn't create a line beak.  
> Use two or more spaces   
> to create a line break.
> 
> Use an empty line to make a new paragraph.

