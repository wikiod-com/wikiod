---
title: "the_field()"
slug: "the_field"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - the_field($field_name, $post_id);

## Parameters
| Paramenter | Details |
| ------ | ------ |
| $field_name   | the name of the field to be retrieved. eg “page_content” (required)   |
| $post_id   | Specific post ID where your value was entered. Defaults to current post ID (not required). This can also be options / taxonomies / users / etc   |

Use get_field() when assigning field values to variables, or when manipulating the returned content in your code. the_field() is the same as `echo get_field($field_name);`

## Check if field exists
    <?php if( get_field('text_field') ): ?>
        <?php the_field('text_field'); ?>
    <?php endif; ?>

Here we use get_field() to determine if a value exists, and the_field() to echo it.

