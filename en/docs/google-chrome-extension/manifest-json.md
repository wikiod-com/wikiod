---
title: "manifest.json"
slug: "manifestjson"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

Official documentation
======

[Manifest File Format][1]

Format
======

Manifest file is written in [JSON][2] (JavaScript Object Notation) format.

This format differs from more loose rules of writing object literals in JavaScript code. Among important differences:

* Every key name and string literal **must be in double quotes**.

  * Correct: `"key": "value"`

  * Wrong: `key: "value"`, `'key': 'value'`

* **No comments** are allowed by the format.

  * Wrong: `"key": "value" // This controls feature foo`

* Strict comma rules: **items separated by commas, no dangling commas**.

  * Correct:

        {
          "foo": "bar",
          "baz": "qux"
        }

  * Wrong (comma missing):

        {
          "foo": "bar"
          "baz": "qux"
        }

  * Wrong (dangling comma):

        {
          "foo": "bar",
          "baz": "qux",
        }


  [1]: https://developer.chrome.com/extensions/manifest
  [2]: http://json.org/

## Absolute minimum manifest.json
`manifest.json` gives information about the extension, such as the most important files and the capabilities that the extension might use. Among the supported manifest fields for extensions, the following **three** are required.


    {
        "manifest_version": 2,
        "name": "My Extension",
        "version": "1.0"
    }

## Obtaining manifest from extension code
[`chrome.runtime.getManifest()`][1] returns the extension's manifest in a form of a parsed object.

This method works both on content scripts and all extension pages, it requires no permissions,

Example, obtaining the extension's version string:

    var version = chrome.runtime.getManifest().version;


  [1]: https://developer.chrome.com/extensions/runtime#method-getManifest

