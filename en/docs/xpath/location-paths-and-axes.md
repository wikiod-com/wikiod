---
title: "Location paths and axes"
slug: "location-paths-and-axes"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

An XPath *location path* is a series of *location steps* separated by a `/` character:

    step1/step2/step3

A *location step* contains an *axis*, a *node test*, and an optional list of *predicates*. The *axis* and the *node test*  are separated by two colon characters `::`. The *predicates* are enclosed in square brackets:

    axis::nodeTest[predicate1][predicate2]

The evaluation of a *location path* starts with a node set containing the *context node* given by the context of the expression, or the *root node*, if the location path starts with a `/`. At each step, each node *N* in the original node set is replaced with the set of nodes that

* can be reached from *N* following the given *axis*,
* matches the *node test*, and
* matches all *predicates*.

The result of a *location path* expression is the final node set obtained after processing all *location steps*.

## Traversing child elements
Traversing from the root node to a descendant element using the `child` axis:

    /child::html/child::body/child::div/child::span

Since the `child` axis is the default axis, this can be abbreviated to:

    /html/body/div/span



## Traversing all descendants
The `descendant` and `descendant-or-self` axes can be used to find all descendant elements of a node at any depth. In contrast, the `child` axis only traverses immediate children.

    /child::html/descendant::span
    /child::html/descendant-or-self::*

The double slash `//` is a shortcut for `/descendant-or-self::node()/`. So the following expressions are equivalent:

    table//td
    child::table/descendant-or-self::node()/child::td
    child::table/descendant::td
    table/descendant::td


## Traversing ancestors
The `parent` axis contains only the parent of a node. The following expression selects the `html` element by taking a detour over the `body` element:

    /child::html/child::body/parent::html

`..` is a shortcut for `parent::node()`

The `ancestor` and `ancestor-or-self` axes traverse all ancestors of a node. The following expression returns all `div` elements that are ancestors of the context node:

    ancestor::div


## The "self" axis
The `self` axis only contains the context node itself. The expression `.` is a shortcut for `self::node()` and always matches the context node. The `.` shortcut is useful for enumerating descendants of the context node. The following expressions are equivalent:

    .//span
    self::node()/descendant-or-self::node()/child::span
    descendant::span

The `self` axis can be helpful in XPath 1.0 predicates. For example, to select all `h1`, `h2`, and `h3` children of the context node:

    *[self::h1 or self::h2 or self::h3]


## Traversing following and preceding nodes
The `following-sibling` and `preceding-sibling` axes contain the siblings before or after the context node, and the `following` and `preceding` axes contain all nodes in the document before or after the context node, but:

* None of these axes contain attribute or namespace nodes.
* The `following` axis doesn't contain any descendants.
* The `preceding` axis doesn't contain any ancestors.

Examples:

    following::span[1]
    following-sibling::*[last()]


## Traversing attribute and namespace nodes
The `attribute` and `namespace` axes contain all attribute and namespace nodes of an element. The shortcut `@` stands for `attribute::`, so the following are equivalent:

    child::div/attribute::class
    div/@class


