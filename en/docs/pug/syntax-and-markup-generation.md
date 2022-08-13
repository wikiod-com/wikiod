---
title: "Syntax and markup generation"
slug: "syntax-and-markup-generation"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

A preview of the difference between pug code and the generated markup

Pug makes possible to write HTML in a simplest way, using a clean, whitespace sensitive syntax. 

## From Pug to HTML
    doctype html
    html(lang="en")
      head
        title= pageTitle
        script(type='text/javascript').
          if (foo) bar(1 + 5)
      body
        h1 Pug - node template engine
        #container.col
          if youAreUsingPug
            p You are amazing
          else
            p Get on it!
          p.
            Pug is a terse and simple templating language with a
            strong focus on performance and powerful features.

Becomes:

    <!DOCTYPE html>
    <html lang="en">
      <head>
        <title>Pug</title>
        <script type="text/javascript">
          if (foo) bar(1 + 5)
        </script>
      </head>
      <body>
        <h1>Pug - node template engine</h1>
        <div id="container" class="col">
          <p>You are amazing</p>
          <p>Pug is a terse and simple templating language with a strong focus on performance and powerful features.</p>
        </div>
      </body>
    </html>

