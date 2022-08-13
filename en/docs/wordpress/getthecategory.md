---
title: "get_the_category()"
slug: "get_the_category"
draft: false
images: []
weight: 9870
type: docs
toc: true
---

This function returns all categories as an array of the current post or page or the designated post or page.

## Syntax
 - get_the_category( $id )

## Parameters
| Parameter | Details |
| ------ | ------ |
| $id | (int) (Optional) default to current post ID. The post ID. |

Please note that get_the_category() returns an array, which means that you can't directly echo the value returned.

Here is a list of objects of each category that you can print:
 - term_id
 - name
 - slug
 - term_group
 - term_taxonomy_id
 - taxonomy
 - description
 - parent
 - count
 - object_id
 - filter
 - cat_ID
 - category_count
 - category_description
 - cat_name
 - category_nicename
 - category_parent

## Get all names of categories of the post
**Code**

    $categories = get_the_category();
     foreach( $categories as $category ) {
        echo  $category->name . '<br />';
    }

**Output**

All names of categories of the current page, one on each line.

## Get all ids of categories of the post
**Code**

    $categories = get_the_category();
     foreach( $categories as $category ) {
        echo  $category->term_id . '<br />';
    }

**Output**

All ids of categories of the current page, one on each line.

