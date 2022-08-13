---
title: "Namespaces"
slug: "namespaces"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

XPath 1.0 doesn't have the concept of a default namespace.

Also, the namespace prefixes defined in the original XML document do not affect XPath - namespace prefixes have to be explicitly registered with the XPath provider, otherwise prefixes can't be used at all in the XPath expression.

## Namespace aware functions
<!-- language: lang-xml -->
```
<root xmlns="http://test/">
    <element xmlns:example="http://foobar/">
        <example:hello_world attribute="another example" />
    </element>
</root>
```

The expression `/root` will return nothing, because there is no non-namespaced element called `root` at the root level of the document. However, The following *will* return the `<root xmlns="http://test/">` element.

    /*[namespace-uri() = 'http://test/' and local-name() = 'root']




