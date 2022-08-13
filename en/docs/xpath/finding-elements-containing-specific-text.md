---
title: "Finding elements containing specific text"
slug: "finding-elements-containing-specific-text"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Find all elements with certain text
Imagine the following XML:

<!-- language: lang-xml -->
```
<root>
    <element>hello</element>
    <another>
        hello
    </another>
    <example>Hello, <nested> I am an example </nested>.</example>
</root>
```

The following XPath expression:

<!-- language: lang-xpath -->

    //*[text() = 'hello']

will return the `<element>hello</element>` element, but not the `<another>` element.
This is because the `<another>` element contains whitespace surrounding the `hello` text.

To retrieve both `<element>` and `<another>`, one could use:

    //*[normalize-space(text()) = 'hello']

or

    //*[normalize-space() = 'hello']

which will trim the surrounding whitespace before doing the comparison. Here we can see that the `text()` node specifier is optional when using `normalize-space`.

To find an element *containing* specific text, you can use the `contains` function. The following expression will return the `<example>` element:

    //example[contains(text(), 'Hello')]

----

If you want to find text that spans multiple children/text nodes, then you can use `.` instead of `text()`. `.` refers to the entire text content of the element and it's children.

    //example[. = 'Hello,  I am an example .']

To see the multiple text nodes, you can use:

    //example//text()

which will return:

> - "Hello, "
> - " I am an example "
> - "."

And to more clearly see the entire text content of an element, one can use the `string` function:

    string(//example[1])

or just

    string(//example)

> Hello,  I am an example .

The latter works because, if a node-set is passed into functions like `string`, XPath 1.0 just looks at the first node (in document order) in that node-set, and ignores the rest.

so:

    string(/root/*)

would return:

> hello


