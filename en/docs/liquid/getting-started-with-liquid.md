---
title: "Getting started with Liquid"
slug: "getting-started-with-liquid"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## The basics of Liquid
Liquid code can be categorised into objects, tags and filters.

# Objects
Objects tell Liquid where to show content on a page. Objects and variables are denoted with double curly braces. `{{` and `}}`

```
<!-- input -->
{{ page.title }}

<!-- output -->
Getting started with Liquid
```
# Tags
Tags are used to create logic control the flow of the templates and are denoted with curly brace and percentage signs `{%` and `%}`.
```
<!-- input -->
{% if user %}
  Hello {{ user.name }}!
{% endif %}

<!-- output -->
Hello George!
```

# Filters
Filters are used to manipulate an object and are denoted with a pipe `|`.
Multiple filters can be applied are are applied from left to right.
```
<!-- input -->
{{ "world" | capitalize | prepend: "Hello " | append: "!" }}

<!-- output -->
Hello World!

