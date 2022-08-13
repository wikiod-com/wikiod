---
title: "get_bloginfo()"
slug: "get_bloginfo"
draft: false
images: []
weight: 9656
type: docs
toc: true
---

Retrieves information about the current site.

## Syntax
 - get_bloginfo( $show , $filter )


## Parameters
| Parameter | Details |
| --------- | ------- |
| *$show*   | *(string)* The site setting information to retrieve. |
| *$filter* | *(string)* The specification on whether to return a filtered value or not. |

**$show**

| Values | Description | Example |
| ------ | ----------- | ------- |
| 'name'&nbsp;(Default) | Site title | `'Matt Mullenweg'` |
| 'description' | Site tagline | `'Just another WordPress site'` |
| 'wpurl' | URL of the WordPress installation. Same as the `site_url()` function | `'http://example.com'` , `'http://localhost/wordpress'` |
| 'url' | URL of the site. Same as the `home_url()` function | `'http://example.com'` , `'http://localhost/wordpress'` |
| 'admin_email' | Email address of the main Administrator | `'matt@mullenweg.com'` |
| 'charset' | Character encoding of the pages and feeds | `'UTF-8'` |
| 'version' | Current version of the WordPress installation | `'4.5'` |
| 'html_type' | content-type value of the HTML | `'text/html'` |
| 'text_direction' | Text direction determined by the site’s language | `'ltr'` |
| 'language' | ISO 639-1 based language code | `'en-US'` |
| 'stylesheet_url' | URL of the stylesheet of the activated theme. Value priority: **Child&nbsp;theme**&nbsp;&raquo;&nbsp;Parent&nbsp;theme. | `'http://example.com/wp-content/themes/twentysixteen/style.css'` |
| 'stylesheet_directory' | Resource location of the activated theme. Value priority: **Child&nbsp;theme**&nbsp;&raquo;&nbsp;Parent&nbsp;theme. | `'http://example.com/wp-content/themes/twentysixteen'` |
| 'template_url' | URL directory of the activated theme. Value priority: **Parent&nbsp;theme**&nbsp;&raquo;&nbsp;Child&nbsp;theme. | `'http://example.com/wp-content/themes/twentysixteen'` |
| 'template_directory' | Same as `'template_url'` |  |
| 'pingback_url' | Pingback XML-RPC file | `'http://example/xmlrpc.php'` |
| 'atom_url' | Atom feed URL | `'http://example/feed/atom/'` |
| 'rdf_url' | RDF/RSS 1.0 feed URL | `'http://example/feed/rdf/'` |
| 'rss_url' | RSS 0.92 feed URL | `'http://example/feed/rss/'` |
| 'rss2_url' | RSS 2.0 feed URL | `'http://example/feed/'` |
| 'comments_atom_url' | Comments Atom feed URL | `'http://example/comments/feed/atom/'` |
| 'siteurl' | *(deprecated)* Use ‘url’ instead |
| 'home' | *(deprecated)* Use ‘url’ instead |

**$filter**

| Values | Description | Example |
| ------ | ----------- | ------- |
| 'raw'&nbsp;(Default) | No filters will be applied | *raw data* |
| 'display' | Filters will be applied to the return value if `$show` is neither `'url'` , `'directory'` , `'home'`  | *filtered data* |

## Getting the site title

    <?php echo get_bloginfo( 'name' ); ?>

or

    <?php echo get_bloginfo(); ?>

**Output**

    Matt Mullenweg

Based on these sample settings

[![enter image description here][1]][1]

  [1]: http://i.stack.imgur.com/kLrza.png

## Getting the active theme URL
    <?php echo esc_url( get_bloginfo( 'stylesheet_directory' ) ); ?>

**Output**

    http://example.com/wp-content/themes/twentysixteen

**Alternatives**

Internally, `get_bloginfo( 'stylesheet_directory' )` calls `get_stylesheet_directory_uri()`, so you may want to use that instead:

    <?php echo esc_url( get_stylesheet_directory_uri() ); ?>

Many developers prefer to use these dedicated functions because of inconsistent naming conventions between them and `get_bloginfo()`. For example, `get_stylesheet_directory()` returns the child theme path; however, as our previous example illustrates, `get_bloginfo( 'stylesheet_directory' )` returns the child theme URL. If you use `get_stylesheet_directory_uri()` instead, there's less chance of confusion over whether you're retrieving a path or a URL.

## Getting the site tagline
    <?php echo get_bloginfo( 'description' ); ?>

**Output**

    Just another WordPress site

Based on these sample settings

[![enter image description here][1]][1]



  [1]: http://i.stack.imgur.com/kLrza.png

## Get Email Address of Site Administrator
We can use the `get_bloginfo` function to retrieve the email address of the site administrator.

    <?php echo get_bloginfo('admin_email'); ?>

## Get site url
    <?php echo esc_url(get_bloginfo('url')); ?>
or if you needed to link to a sub page

    <?php echo esc_url(get_bloginfo('url') . '/some-sub-page');  ?>

