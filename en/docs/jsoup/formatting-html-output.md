---
title: "Formatting HTML Output"
slug: "formatting-html-output"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Parameters
| Parameter | Detail | 
| --------- | ------ |
| `boolean outline()` | Get if outline mode is enabled. Default is false. If enabled, the HTML output methods will consider all tags as block. |
| `Document.OutputSettings outline(boolean)` | Enable or disable HTML outline mode. |


[Jsoup 1.9.2 API][1]


  [1]: https://jsoup.org/apidocs/overview-summary.html

## Display all elements as block
By default, Jsoup will display only [block-level elements][1] with a trailing line break.  [Inline elements][2] are displayed without a line break.

Given a body fragment, with inline elements:

    <select name="menu">
        <option value="foo">foo</option>
        <option value="bar">bar</option>
    </select>

Printing with Jsoup:

    Document doc = Jsoup.parse(html);

    System.out.println(doc.html());

Results in:

    <html>
     <head></head>
     <body>
      <select name="menu"> <option value="foo">foo</option> <option value="bar">bar</option> </select> 
     </body>
    </html>

To display the output with each element treated as a block element, the `outline` option has to be enabled on the document's `OutputSettings`.

    Document doc = Jsoup.parse(html);

    doc.outputSettings().outline(true);

    System.out.println(doc.html());

**Output**

    <html>
     <head></head>
     <body>
      <select name="menu"> 
       <option value="foo">foo</option> 
       <option value="bar">bar</option> 
      </select> 
     </body>
    </html>


Source: http://stackoverflow.com/q/38855874/1176178


  [1]: https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements
  [2]: https://developer.mozilla.org/en-US/docs/Web/HTML/Inline_elements

