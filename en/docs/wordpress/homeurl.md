---
title: "home_url()"
slug: "home_url"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Syntax
  -  home_url( $path, $scheme );

## Parameters
| Parameter | Details |
| ------ | ------ | 
| $path  | (*String*,*Optional*) To adding more segment after the home url.   | 
| $scheme  | (*String*,*Optional*) Scheme to give the home url context. Accepts 'http', 'https', 'relative', 'rest', or null. 

## Getting the Home URL
[`home_url()`][1] is used for retrieving the current site home url.
 
    <?php echo esc_url( home_url( '/' ) ) ); ?>
 
**Output** 

    http://www.example.com




  [1]: https://codex.wordpress.org/Function_Reference/home_url

## Site url
Returns the ‘site_url’ option with the appropriate protocol (https://)

    <?php echo site_url(); ?>

**Output**

    http://www.example.com

