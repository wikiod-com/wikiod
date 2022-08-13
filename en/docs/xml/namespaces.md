---
title: "Namespaces"
slug: "namespaces"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

Element and attribute names in XML are called QNames (qualified names).

A QName is made of:
- a namespace (a URI)
- a prefix (an NCName, NC because it contains no colon)
- a local name (an NCName)

Only the namespace and the local name are relevant for comparing two QNames. The prefix is only a proxy to the namespace.

The namespace and prefix are optional, but the namespace is always present if the prefix is present (this is ensured at the syntactic level, so this cannot be done wrong).

The lexical representation of a QName is `prefix:local-name`. The namespace is bound separately using the special `xmlns:...` attributes (reminder: attributes beginning with *xml* are reserved in XML).

If the prefix is empty, no colon is used in the lexical representation of the QName, which only contains the `local-name`. QNames with an empty prefix either have no namespace (if no default namespace is in scope) or are in the default namespace.

## Attribute names with no prefix
Elements and attributes behave differently with respect to default namespaces. This is often the source of confusion.

An attribute whose name has no prefix lives in no namespace, **also when a default namespace is in scope**.

    <?xml version="1.0"?>
    <foo attr="value" xmlns="http://www.example.com/my-namespace">
      <!-- The attribute attr is in no namespace, even though
           a default namespace is in scope. The element foo,
           however, is in the default namespace. -->
    </foo>

## Irrelevance of prefixes
These two documents are semantically equivalement, as namespaces matter, not prefixes.

    <?xml version="1.0"?>
    <myns:foo xmlns:myns="http://www.example.com/my-namespace">
    </myns:foo>

    <?xml version="1.0"?>
    <ns:foo xmlns:ns="http://www.example.com/my-namespace">
    </ns:foo>

## Default namespace
The default namespace is the namespace corresponding to the absence of any prefix. It can be declared with the special `xmlns` attribute.

    <?xml version="1.0"?>
    <foo xmlns="http://www.example.com/my-namespace">
      <!-- the element foo is in the namespace
           http://www.example.com/my-namespace -->
    </foo>

If no default namespace is declared, then names with no prefix are in no namespace.

## Scope of namespace bindings
A namespace binding (special `xmlns` or `xmlns:...` attribute) is in scope for all the descendants of the enclosing element, including this element.

    <?xml version="1.0"?>
    <root>
      <my:element xmlns:my="http://www.example.com/ns1">
        <!-- here, the prefix my is bound to http://www.example.com/ns1 -->
      </my:element>
      <my:element xmlns:my="http://www.example.com/ns2">
        <!-- here, the prefix my is bound to http://www.example.com/ns2 -->
      </my:element>
    </root>

The binding can be overriden in a nested element (this affects readability though):

    <?xml version="1.0"?>
    <my:element xmlns:my="http://www.example.com/ns1">
      <!-- here, the prefix my is bound to http://www.example.com/ns1 -->
      <my:first-child-element/>

      <my:child-element xmlns:my="http://www.example.com/ns2">
        <!-- here, the prefix my is bound to http://www.example.com/ns2,
             including for the element my:child-element -->
      </my:child-element>

      <!-- here, the prefix my is bound to http://www.example.com/ns1 -->
      <my:last-child-element/>

    </my:element>
    
It is very common to declare all namespace bindings in the root element, which improves readability.

    <?xml version="1.0"?>
    <root
      xmlns="http://www.example.com/default-namespace"
      xmlns:ns1="http://www.example.com/ns1"
      xmlns:ns2="http://www.example.com/ns2">
      
      <ns1:element>
        <ns2:other-element/>
      </ns1:element>

    </root>
    


## Bind a prefix to a namespace
A namespace is a URI, but to avoid verbosity, prefixes are used as a proxy.

In the following example, the prefix `my-prefix` is bound to the namespace `http://www.example.com/my-namespace` by using the special attribute `xmlns:my-prefix` (`my-prefix` can be replaced with any other prefix):

    <?xml version="1.0"?>
    <my-prefix:foo xmlns:my-prefix="http://www.example.com/my-namespace">
      <!-- the element my-prefix:foo
           lives in the namespace http://www.example.com/my-namespace -->
    </my-prefix:foo>


## Absence of namespace
In XML, element and attribute names live in namespaces.

By default, they are in no namespace:

    <?xml version="1.0"?>
    <foo attr="value">
      <!-- the foo element is in no namespace, neither is the attr attribute -->
    </foo>



