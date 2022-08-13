---
title: "Template hierarchy"
slug: "template-hierarchy"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Plugins for debugging in WordPress:

* https://wordpress.org/plugins/query-monitor/
* https://wordpress.org/plugins/debug-bar/
* https://wordpress.org/plugins/debug-bar-console/
* https://wordpress.org/plugins/kint-debugger/
* https://wordpress.org/plugins/rest-api-console/

## Introduction
One of the most important things to learn when you are making a WordPress theme is the WordPress Template hierarchy for themes. The template hierarchy defines what template file that will be loaded for each request and in what order. If the first template does not exist in the hierarchy WordPress will try to load the next one and so on until you end up in `index.php`.

To describe the template hierarchy in detail the best way is of course to use an image with the full structure:

[![The WordPress Template hierarchy][1]][1]

The template hierarchy starts with the primary types of pages like archive, singular page or front page and then the subtypes like author archive, tag archive, page or blog post. 

After this we will find the actual templates that will be loaded. First there are the most specific templates that using slugs or IDs to target specific archive types or posts. 

For example `category-$slug.php` that only targets the category with a specific slug, for example `category-books.php` would be used only for the category with the slug `book`. Another example is `page-$id.php` that only targets a page with a specific ID, for example `page-41.php` would target only the page with the ID 41.

After the templates which targets specific types or posts we get to the generic type templates, like archive.php for all archive pages or `page.php` for all pages. But remember, those will only be used if the current page does not match any of the templates that are higher in the hierarchy. 

Lastly, if WordPress couldn't find any matching templates in the template directory, the last fallback is always `index.php` which is the only required template file in a WordPress theme.


  [1]: http://i.stack.imgur.com/jqnhV.png


## Debugging
Its easy to get lost while debugging the hiearchy. You can use PHP's built in command `debug_backtrace`.

Put the next snippet inside any template you want to debug and view the generated page:

    <!--
    <?php print_r( debug_backtrace() ) ?>
    -->

