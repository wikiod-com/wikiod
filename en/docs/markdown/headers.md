---
title: "Headers"
slug: "headers"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Atx-Style Headers
Text prefixed with one to six pound signs (hash symbols, `#`) becomes a header `<h1>` through `<h6>`, according to how many pound signs were used.

    # This is a first-level (`<h1>`) header
    ## This is a second-level (`<h2>`) header
    ### And so on.

> # This is a first-level (`<h1>`) header
> ## This is a second-level (`<h2>`) header
> ### And so on.

Atx-style headers may be optionally closed by adding trailing pound signs, which are ignored.

    ### This is a header with some trailing hashes ###
> ### This is a header with some trailing hashes ###

At present Stack Exchange's flavor of markdown only seems to support up to three header levels (`###`), though [Markdown supports all the way up to 6](https://daringfireball.net/projects/markdown/syntax#header).

## Setext-Style Headers
To create a first-level (`<h1>`) header, use the equal sign (`=`) in a line under your text:

    All About Dogs
    ==============

> All About Dogs
> ==============

Use hyphens (`-`) for second-level (`<h2>`) headers:

    The Debut Novel
    ---------------

> The Debut Novel
> ---------------

The line below the header can be of any length.

    Another header
    --
    
    Another header
    -

> Another header
> --
> 
> Another header
> -

