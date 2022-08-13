---
title: "get_the_title()"
slug: "get_the_title"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

This function returns the title of the current post or the designated post.

## Syntax
 - get_the_title( $post )

## Parameters
| Parameter | Details|
| ------ | ------ |
| $post | (int) (optional) Post ID or post object. Default is the the current post's id. |

If you plan to get the title of a post or page using a post loop, it is suggested to use the_title() instead.

## Simple use of get_the_title
**Code**

    get_the_title();

**Output**

The title of the current post or page

## Get the title of a specified post id
**Code**

    get_the_title( 44 );

**Output**

The title of the post id:44.

