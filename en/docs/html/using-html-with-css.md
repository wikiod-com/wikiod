---
title: "Using HTML with CSS"
slug: "using-html-with-css"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

[CSS](https://www.wikiod.com/docs/css) provides styles to HTML elements on the page. Inline styling involves usage of the style attribute in tags, and is highly discouraged. Internal stylesheets use the `<style>` tag and are used to declare rules for directed portions of the page. External stylesheets may be used through a `<link>` tag which takes an external file of CSS and applies the rules to the document. This topic covers usage of all three methods of attachment.

## Syntax
 - `<link rel="stylesheet" type="text/css" href="stylesheet.css">`
 - `<style></style>`

## External Stylesheet Use
Use the `link` attribute in the document's `head`:

    <head>
        <link rel="stylesheet" type="text/css" href="stylesheet.css">
    </head>

You can also use stylesheets provided from websites via a content delivery network, or CDN for short. (for example, Bootstrap):

    <head>
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">
    </head>

Generally, you can find CDN support for a framework on its website.

## Internal Stylesheet
You can also include CSS elements internally by using the `<style>` tag:
    
    <head>
        <style type="text/css">
            body {
               background-color: gray;
            }
        </style>  
    </head>
Multiple internal stylesheets can be included in a program as well.

    <head>
        <style type="text/css">
            body {
               background-color: gray;
            }
        </style>

        <style type="text/css">
            p {
               background-color: blue;
            }
        </style>  
    </head>

## Inline Style
You can style a specific element by using the `style` attribute:

    <span style="color: red">This text will appear in red.</span>

> Note: Try to avoid this -- the point of CSS is to separate content from presentation.

## Multiple Stylesheets
It's possible to load multiple stylesheets:

    <head>
        <link rel="stylesheet" type="text/css" href="general.css">
        <link rel="stylesheet" type="text/css" href="specific.css">
    </head>

Note that **later files and declarations will override earlier ones**. So if `general.css` contains:

    body {
        background-color: red;
    }

and `specific.css` contains:

    body { 
        background-color: blue;
    }

if both are used, the background of the document will be blue.

