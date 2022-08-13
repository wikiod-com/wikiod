---
title: "Maruku"
slug: "maruku"
draft: false
images: []
weight: 9999
type: docs
toc: true
---

Maruku is a Markdown interpreter for Ruby. It allows for exporting Markdown to HTML and PDF.

## Syntax
- Maruku.new(str) => parses Markdown into a Maruku object
- Maruku#to_html_document => returns the Maruku object as a full HTML document (as a string)

## Parameters
| Parameter | Usage |
| ------ | ------ |
| str | Markdown string to be parsed |

## Reading Markdown into Maruku
    require 'maruku'
    
    str = "
        # Title
        * List 1
        * List 2
        * List 3
    "
    markdown = Maruku.new(str)
    puts markdown.inspect

## Markdown to HTML
    require 'maruku'
    
    str = "
        # Title
        * List 1
        * List 2
        * List 3
    "
    markdown = Maruku.new(str)
    puts markdown.to_html_document 


