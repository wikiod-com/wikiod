---
title: "switch"
slug: "switch"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

`<switch>` is a conditional processing attribute. It doesn't prevent elements from being referenced by other elements. In our case, `<switch>` evaluates the **systemLanguage** value on its direct child elements that matches the user's language. Once is found, the child is rendered and the other children will be bypassed.

If the **systemLanguage** is not specified, the child will be displayed, allowing us specifying a fallback.

[Related W3C Recommendation informations](https://www.w3.org/TR/SVG/struct.html#SwitchElement)

## Alternate viewing depending on the user's language
    <svg xmlns="http://www.w3.org/2000/svg">
      <switch>
        <text systemLanguage="en-UK" x="10" y="10">UK English</text>
        <text systemLanguage="fr" x="10" y="10">Français</text>
        <text systemLanguage="ru" x="10" y="10">Русский</text>
        <text x="10" y="20">English</text> <!-- fallback (if none of the languages match) -->
      </switch>
    </svg>

