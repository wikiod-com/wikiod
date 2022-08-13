---
title: "Headings"
slug: "headings"
draft: false
images: []
weight: 9905
type: docs
toc: true
---

HTML provides not only plain paragraph tags, but six separate header tags to indicate headings of various sizes and thicknesses. Enumerated as heading 1 through heading 6, heading 1 has the largest and thickest text while heading 6 is the smallest and thinnest, down to the paragraph level. This topic details proper usage of these tags.

## Syntax
- `<h1>...</h1>`
- `<h2>...</h2>`
- `<h3>...</h3>`
- `<h4>...</h4>`
- `<h5>...</h5>`
- `<h6>...</h6>`

- An `h1`–`h6` element must have both a start tag and an end tag.<sup>[**1**][1]</sup>

- `h1`–`h6` elements are block level elements by default (CSS style: `display: block`).<sup>[**2**][2]</sup>

- `h1`–`h6` elements should not be confused with the [section element][3]

- Heading tags (`h1`–`h6`) are not related to the `head` tag. 

- Permitted Content: [phrasing content][4]

- The different CSS-styles for headings differ usually in `font-size` and `margin`. The following CSS-settings for `h1`–`h6` elements can serve as an orientation (characterized as 'informative' by the [W3C][5])

- Search engine spiders (the code that adds a page to a search engine) automatically pays more attention to higher importance (h1 has most, h2 has less, h3 has even less, ...) headings to discern what a page is about.


  [1]: https://www.w3.org/TR/html-markup/h1.html
  [2]: https://www.w3.org/TR/html401/struct/global.html#h-7.5.3
  [3]: https://www.wikiod.com/html/sectioning-elements
  [4]: https://www.w3.org/TR/html-markup/terminology.html#phrasing-content
  [5]: https://www.w3.org/TR/CSS21/sample.html

## Using Headings
Headings can be used to describe the topic they precede and they are defined with the `<h1>` to `<h6>` tags. Headings support all the [global attributes][1].

- `<h1>` defines the most important heading.
- `<h6>` defines the least important heading.

**Defining a heading:**

    <h1>Heading 1</h1>
    <h2>Heading 2</h2>
    <h3>Heading 3</h3>
    <h4>Heading 4</h4>
    <h5>Heading 5</h5>
    <h6>Heading 6</h6>

---

Correct structure matters
========================

**Search engines** and other **user agents** usually index page content based on heading elements, for example to create a table of contents, so using the correct structure for headings is important.

In general, an article should have one `h1` element for the main title followed by `h2` subtitles – going down a layer if necessary. If there are `h1` elements on a higher level they shoudn't be used to describe any lower level content.

**Example document (extra intendation to illustrate hierarchy):**

    <h1>Main title</h1>
    <p>Introduction</p>
    
        <h2>Reasons</h2>

            <h3>Reason 1</h3>
            <p>Paragraph</p>
    
            <h3>Reason 2</h3>
            <p>Paragraph</p>
    
        <h2>In conclusion</h2>
        <p>Paragraph</p>


  [1]: https://www.wikiod.com/html/global-attributes

