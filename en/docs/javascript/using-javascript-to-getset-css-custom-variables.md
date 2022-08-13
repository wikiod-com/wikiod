---
title: "Using javascript to getset CSS custom variables"
slug: "using-javascript-to-getset-css-custom-variables"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## How to get and set CSS variable property values.
To get a value use the .getPropertyValue() method

    element.style.getPropertyValue("--var")
To set a value use the .setProperty() method.

    element.style.setProperty("--var", "NEW_VALUE")

