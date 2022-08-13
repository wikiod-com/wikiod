---
title: "add_theme_support()"
slug: "add_theme_support"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

This function registers features that the theme supports.

## Syntax
 - add_theme_support( $feature )

## Parameters
| Parameter | Details |
| ------ | ------ |
| $feature | (string) The feature being added. |

List of features to be used in $feature:
 - 'post-formats'
 - 'post-thumbnails'
 - 'html5'
 - 'custom-logo'
 - 'custom-header-uploads'
 - 'custom-header'
 - 'custom-background'
 - 'title-tag'
 - 'starter-content'

## Adding theme support for post formats
    add_theme_support( 'post-formats', array( 'formatone', 'formattwo' ) );

## Adding theme support for post thumbnails to posts
    add_theme_support( 'post-thumbnails', array( 'post' ) );

The above code only allows post thumnails on all posts. To enable the feature on all post types, do:

    add_theme_support( 'post-thumbnails' );

