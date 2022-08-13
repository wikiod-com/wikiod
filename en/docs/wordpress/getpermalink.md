---
title: "get_permalink()"
slug: "get_permalink"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

This function returns the full paralink of the current post or the designated post.

## Syntax
 - get_permalink( $post, $leavename )

## Parameters
| Parameter | Details|
| ------ | ------ |
| $post | (int) (optional) Post ID or post object. Default is the the current post's id. |
| $leavename | (bool) (optional) Whether to keep post name or page name. |

For the parameter $leavename, it is false by default.

## Simple use of get_parmalink
**Code**

    echo get_permalink();

**Output**

The link of the current page, for example: http://website.com/category/name-of-post/

## Specifying the post to get the link
**Code**

    echo get_permalink( 44 );

**Output**

The link of the post id:44, for example: http://website.com/category/name-of-post/

## Get the link without the post's name
**Code**

    echo get_permalink( 44 , false );

**Output**

The link of the post id:44 without the name of the post, for example: http://website.com/category/%postname%/

