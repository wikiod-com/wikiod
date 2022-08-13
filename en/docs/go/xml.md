---
title: "XML"
slug: "xml"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

While many uses of the [`encoding/xml`](https://godoc.org/encoding/xml) package include marshaling and unmarshaling to a Go `struct`, it's worth noting that this is not a direct mapping. The package documentation states:

> Mapping between XML elements and data structures is inherently flawed:
> an XML element is an order-dependent collection of anonymous values,
> while a data structure is an order-independent collection of named values.

For simple, unordered, key-value pairs, using a different encoding such as Gob's or [JSON](https://www.wikiod.com/go/json) may be a better fit. For ordered data or event / callback based streams of data, XML may be the best choice.

## Basic decoding / unmarshalling of nested elements with data
XML elements often nest, have data in attributes and/or as character data. The way to capture this data is by using `,attr` and `,chardata` respectively for those cases.

```
var doc = `
<parent>
  <child1 attr1="attribute one"/>
  <child2>and some cdata</child2>
</parent>
`

type parent struct {
    Child1 child1 `xml:"child1"`
    Child2 child2 `xml:"child2"`
}

type child1 struct {
    Attr1 string `xml:"attr1,attr"`
}

type child2 struct {
    Cdata1 string `xml:",cdata"`
}

func main() {
    var obj parent
    err := xml.Unmarshal([]byte(doc), &obj)
    if err != nil {
        log.Fatal(err)
    }

    fmt.Println(obj.Child2.Cdata1)

}
```
[`Playground`](https://play.golang.org/p/yQrZPNTaWo)

