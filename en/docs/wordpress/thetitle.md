---
title: "the_title()"
slug: "the_title"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

This function returns the title of the current post.

## Syntax
 - the_title( $before, $after, $echo );

## Parameters
| Parameter | Details |
| ------ | ------ |
| $before| (string) (optional) Text to place before the title. |
| $after| (string) (optional) Text to place after the title. |
| $echo| (Boolean) (optional) Display the title or return it for use in PHP |

 - For the parameter $echo, use true to display the title and use false to return it for use in PHP
 - Please note that the_title can only be used in loops, if you want to use it outside of loops, please use get_the_title

## Simple use of the_title
**Code**

    the_title( );

**Output**

The title of the current post or page

## Printing the title with code before and after
**Code**

    the_title( '<h1>', '</h1>' );

**Output**

The title of the current post or page in h1 tags

