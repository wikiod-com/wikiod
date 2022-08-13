---
title: "Working with fncount, xdmpestimate and ctsfrequency"
slug: "working-with-fncount-xdmpestimate-and-ctsfrequency"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

This section provides an overview of [fn:count][1], [xdmp:estimate][2] and [cts:frequency][3] along with examples and use cases

[1]:https://docs.marklogic.com/fn:count
[2]:https://docs.marklogic.com/xdmp:estimate
[3]:https://docs.marklogic.com/cts:frequency

## Using fn:count() to get the number of matching documents
The XML document, I will be using throughout the examples is -

    <a>
        <b>test-value</b>
        <d>fragment-d</d>
        <c-root>
            <d>fragment-d</d>
            <e>fragment-e</e>
        </c-root>
    </a>

The following queries returns the number of documents with value `fragment-d` for element `d` -

 - Using a [cts:search](https://docs.marklogic.com/cts:search)

    `fn:count(cts:search(fn:doc(), cts:element-value-query(xs:QName("d"), "fragment-d")))`

 - Using XPath

    `fn:count(fn:doc()[//d="fragment-d"]))`



## Using xdmp:estimate() to get the number of matching documents
    xdmp:estimate(cts:search(fn:doc(), cts:element-value-query(xs:QName("d"), "fragment-d")))

> [xdmp:estimate][1] can not be used on XPaths unlike [fn:count][2] is used in previous example

> [xdmp:estimate][1] actually gives the number of matching fragments

[1]: https://docs.marklogic.com/xdmp:estimate
[2]: https://docs.marklogic.com/fn:count

## Counting documents when fragments are defined
The XML document to consider in this example -

    <a>
        <b>test-value</b>
        <d>fragment-d</d>
        <c-root>
            <d>fragment-d</d>
            <e>fragment-e</e>
        </c-root>
    </a>

A fragment root is declared on `<c-root>`

If this is the only document in the database, [xdmp:estimate][2] and [fn:count][1] are going to behave differently -

    xdmp:estimate(cts:search(fn:doc(), cts:element-value-query(xs:QName("d"), "fragment-d")))

Result of the above query will be `2` (Number of fragments)

    fn:count(cts:search(fn:doc(), cts:element-value-query(xs:QName("d"), "fragment-d")))

Result of the above query will be `1` (Number of documents)

> In terms of performance [xdmp:estimate][2] is much better than [fn:count][1] as it takes the advantages of indexes while resolving the search results

[1]:https://docs.marklogic.com/fn:count
[2]:https://docs.marklogic.com/xdmp:estimate

