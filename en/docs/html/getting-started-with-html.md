---
title: "Getting started with HTML"
slug: "getting-started-with-html"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
<hr />

# Introduction #

[HTML][1] (**H**yper**t**ext **M**arkup **L**anguage) uses a markup system composed of elements which represent specific content. _Markup_ means that with HTML you declare _what_ is presented to a viewer, not _how_ it is presented. Visual representations are defined by [Cascading Style Sheets (CSS)][2] and realized by browsers. [Still existing elements that allow for such][3], like e.g. [`font`][4], "are entirely obsolete, and must not be used by authors"<span id="obsolete-ref"><sup><a href="#obsolete-note">\[1]</a></sup></span>.

HTML is sometimes called a programming language but it has no logic, so is a **markup language**. HTML tags provide semantic meaning and machine-readability to the content in the page.

An element usually consists of an opening tag (`<element_name>`), a closing tag (`</element_name>`), which contain the element's name surrounded by angle brackets, and the content in between: `<element_name>...content...</element_name>`

There are some HTML elements that don't have a closing tag or any contents. These are called [void elements][5]. Void elements include `<img>`, `<meta>`, `<link>` and `<input>`.

Element names can be thought of as descriptive keywords for the content they contain, such as `video`, `audio`, `table`, `footer`. 

A HTML page may consist of potentially hundreds of elements which are then read by a web browser, interpreted and rendered into human readable or audible content on the screen.

For this document it is important to note the difference between elements and tags:

**Elements:** `video`, `audio`, `table`, `footer`

**Tags:** `<video>`, `<audio>`, `<table>`, `<footer>`, `</html>`, `</body>`

<br><hr />

# Element insight #

Let's break down a tag...

The `<p>` tag represents a common paragraph. 

Elements commonly have an opening tag and a closing tag. The opening tag contains the element's name in angle brackets (`<p>`). The closing tag is identical to the opening tag with the addition of a forward slash (`/`) between the opening bracket and the element's name (`</p>`).

Content can then go between these two tags: `<p>This is a simple paragraph.</p>`. 

<br /><hr />

# Creating a simple page #

The following HTML example creates a simple ["Hello World"][6] web page. 

HTML files can be created using any [text editor][7]. The files must be saved with a `.html` or `.htm`<span id="htm-ref"><sup><a href="#htm-note">\[2]</a></sup></span> extension in order to be recognized as HTML files.

Once created, this file can be opened in any web browser.

<br />
    
    <!DOCTYPE html>
    <html lang="en">
    
        <head>
            <meta charset="UTF-8">
            <title>Hello!</title>
        </head>
    
        <body>
            <h1>Hello World!</h1>
            <p>This is a simple paragraph.</p>
        </body>
    
    </html>

<br /><hr />

# Simple page break down #

These are the tags used in the example:

Tag | Meaning
--- | ---
`<!DOCTYPE>` | Defines the HTML version used in the document. In this case it is HTML5.<br>See the [doctypes topic][8] for more information.
`<html>` | Opens the page. No markup should come after the closing tag (`</html>`). The `lang` attribute declares the primary language of the page using the [ISO language codes][9] (`en` for English).<br />See the [Content Language topic](https://www.wikiod.com/html/content-languages) for more information.
`<head>` | Opens the head section, which does not appear in the main browser window but mainly contains information *about* the HTML document, called *metadata*. It can also contain imports from external stylesheets and scripts. The closing tag is `</head>`.
`<meta>` | Gives the browser some metadata about the document. The `charset` attribute declares the [character encoding][10]. Modern HTML documents should always use [UTF-8][11], even though it is not a requirement. In HTML, the `<meta>` tag does not require a closing tag.<br />See the [Meta topic](https://www.wikiod.com/html/meta-information) for more information.
`<title>` | The title of the page. Text written between this opening and the closing tag (`</title>`) will be displayed on the tab of the page or in the title bar of the browser.
`<body>` | Opens the part of the document displayed to users, i.e. all the visible or audible content of a page. No content should be added after the closing tag `</body>`.
`<h1>` | A level 1 heading for the page.<br />See [headings][12] for more information.
`<p>` | Represents a common paragraph of text.

<hr>

<span id="obsolete-note"><a href="#obsolete-ref">1. ↑</a></span> [HTML5, 11.2 Non-conforming features][3]<br />
<span id="htm-note"><a href="#htm-ref">2. ↑</a></span> `.htm` is inherited from the legacy [DOS][13] three character file extension limit.


  [1]: https://en.wikipedia.org/wiki/HTML
  [2]: https://en.wikipedia.org/wiki/CSS
  [3]: https://www.w3.org/TR/html5/obsolete.html#non-conforming-features
  [4]: https://www.w3.org/wiki/HTML/Elements/font
  [5]: https://www.wikiod.com/html/void-elements
  [6]: https://en.wikipedia.org/wiki/%22Hello,_World!%22_program
  [7]: https://en.wikipedia.org/wiki/Text_editor
  [8]: https://www.wikiod.com/html/doctypes
  [9]: https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
  [10]: https://www.w3.org/International/questions/qa-html-encoding-declarations.en
  [11]: https://en.wikipedia.org/wiki/UTF-8
  [12]: https://www.wikiod.com/html/headings
  [13]: https://en.wikipedia.org/wiki/DOS

