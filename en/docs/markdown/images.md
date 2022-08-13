---
title: "Images"
slug: "images"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Syntax
 - ![Alt text]\(/path/to/img.jpg)
 - \![Alt text]\(/path/to/img.jpg "Optional title")
 - \![Alt text][id]
     
    \... intervening content ...

    \[id]: /path/to/img.jpg "Optional title"

The syntax for images is the same as that for [links](https://www.wikiod.com/markdown/linking), but with an exclamation mark `!` in front of the text (which is used as alt-text).

## Inline Images
In this type of image addition, the image URL is included at the location where the image will be displayed. If you need to add the same image several times, you must include its URL every time. 

**Markdown Source**

    Picture of Duck:

    ![Duck](http://i.stack.imgur.com/ukC2U.jpg)

**HTML Output**

> Picture of Duck:
>
> ![Duck](http://i.stack.imgur.com/ukC2U.jpg)

## Reference-Style Images
In this type of image addition, one image can be used several times without duplicating its URL, making it a good choice when using one image multiple times in a document.

**Markdown Source**
    
    Picture of Duck:

    ![Duck][1]

    Same picture of Duck:

    ![Same Duck][1]
    
    [1]: http://i.stack.imgur.com/ukC2U.jpg

**HTML Output**

>Picture of Duck:
>
> ![Duck][1]
> 
> Same picture of Duck: 
>
> ![Same Duck][1]
> 
> [1]: http://i.stack.imgur.com/ukC2U.jpg

