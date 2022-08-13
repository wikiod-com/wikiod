---
title: "Finding elements containing specific attributes"
slug: "finding-elements-containing-specific-attributes"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Find all elements with a certain attribute
Imagine the following XML:

<!-- language: lang-xml -->
```
<root>
    <element foobar="hello_world" />
    <element example="this is one!" />
</root>
```

<!-- language: lang-xpath -->

    /root/element[@foobar]

and will return the `<element foobar="hello_world" />` element.


## Find all elements with a certain attribute value
Imagine the following XML:

<!-- language: lang-xml -->
```
<root>
    <element foobar="hello_world" />
    <element example="this is one!" />
</root>
```

The following XPath expression:

<!-- language: lang-xpath -->

    /root/element[@foobar = 'hello_world']

will return the `<element foobar="hello_world" />` element.

double quotes can also be used:

    /root/element[@foobar="hello_world"]

