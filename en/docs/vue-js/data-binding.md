---
title: "Data Binding"
slug: "data-binding"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Text
The most basic form of data binding is text interpolation using the “Mustache” syntax (double curly braces):
```
<span>Message: {{ msg }}</span>
```
The mustache tag will be replaced with the value of the `msg` property on the corresponding data object. It will also be updated whenever the data object’s `msg` property changes.

You can also perform one-time interpolations that do not update on data change:
```
<span>This will never change: {{* msg }}</span>
```

## Raw HTML
The double mustaches interprets the data as plain text, not HTML. In order to output real HTML, you will need to use triple mustaches:
```
<div>{{{ raw_html }}}</div>
```
The contents are inserted as plain HTML - data bindings are ignored. If you need to reuse template pieces, you should use partials.

## Attributes
Mustaches can also be used inside HTML attributes:

```
<div id="item-{{ id }}"></div>
```

Note that attribute interpolations are disallowed in Vue.js directives and special attributes. Don’t worry, Vue.js will raise warnings for you when mustaches are used in wrong places.

## Filters
Vue.js allows you to append optional “filters” to the end of an expression, denoted by the “pipe” symbol:
```
{{ message | capitalize }}
```
Here we are “piping” the value of the `message` expression through the built-in `capitalize` filter, which is in fact just a JavaScript function that returns the capitalized value. Vue.js provides a number of built-in filters, and we will talk about how to write your own filters later.

Note that the pipe syntax is not part of JavaScript syntax, therefore you cannot mix filters inside expressions; you can only append them at the end of an expression.

Filters can be chained:
```
{{ message | filterA | filterB }}
```
Filters can also take arguments:
```
{{ message | filterA 'arg1' arg2 }}
```
The filter function always receives the expression’s value as the first argument. Quoted arguments are interpreted as plain string, while un-quoted ones will be evaluated as expressions. Here, the plain string `'arg1'` will be passed into the filter as the second argument, and the value of expression `arg2` will be evaluated and passed in as the third argument.

