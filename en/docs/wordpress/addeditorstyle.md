---
title: "add_editor_style()"
slug: "add_editor_style"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

The function allows user to load stylesheets for the TinyMCE editor

## Syntax
 - add_editor_style( $stylesheet )

## Parameters
| Parameter | Details |
| ------ | ------ |
| $ stylesheet | (array or string) (Optional) Stylesheet name or array thereof, relative to theme root. Defaults to 'editor-style.css' |

## Loading a single css file
**Code**

    function add_new_style() {
        add_editor_style( 'file-name-here.css' );
    }
    add_action( 'admin_init', 'add_new_style' );

**Explanation**

In the above code, we used add_editor_style to load the css file. We also used add_action to make sure that WordPress runs our function.

