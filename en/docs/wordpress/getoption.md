---
title: "get_option()"
slug: "get_option"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Retrieves an option value based on an option name.

## Syntax
 - get_option( $option, $default )

## Parameters
| Parameter | Details |
| ------ | ------ |
| $option| (string) Name of option to retrieve. Expected to not be SQL-escaped. |
| $default | (mixed) (Optional) Default value to return if the option does not exist. |

List of arguments for $option
 - 'admin_email'
 - 'blogname'
 - 'blogdescription'
 - 'blog_charset'
 - 'date_format'
 - 'default_category'
 - 'home'
 - 'siteurl'
 - 'template'
 - 'start_of_week'
 - 'upload_path'
 - 'users_can_register'
 - 'posts_per_page'
 - 'posts_per_rss'

## Show blog title
**Code**

    <h1><?php echo get_option( 'blogname' ); ?></h1>

**Output**

<h1>Name of the blog in H1 style</h1>

## Show Character Set
**Code**

    <p><?php echo esc_html( sprintf( __( 'Character set: %s', 'textdomain' ), get_option( 'blog_charset' ) ) ); ?></p>

**Output**

Character set: UTF-8

## Handling non-existing options
**Code**

    <?php echo get_option( 'something_bla_bla_bla' ); ?>

**Output**

false


----------


**Code**

    <?php echo get_option( 'something_bla_bla_bla', 'Oh no!' ); ?>

**Output**

Oh no!

