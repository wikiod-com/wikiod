---
title: "Child Theme Basics"
slug: "child-theme-basics"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
- **template** –– is the name of the main WordPress theme, the parent.
- **child-theme** –– is the package which overrides the **template**.

I've been advertising that the use of a child theme is always a good thing but there is always a *But ...*

In our Template overwriting example let's imagine that the author of a theme is adding his own improvements to the sidebar template and there will be a new one at

`/themes/template/sidebar.php`

    <?php
    /**
     * The template for the sidebar containing the main widget area
     *
     * @link https://developer.wordpress.org/themes/basics/template-files/#template-partials
     */
    
    if ( is_active_sidebar( 'sidebar-1' )  ) : ?>
        <aside id="secondary" class="sidebar widget-area" role="complementary">
            <?php dynamic_sidebar( 'sidebar-1' ); ?>
        </aside><!-- .sidebar .widget-area -->
    <?php endif; ?>

Now our website won't benefit from the new `role="complementary"` spec because our child theme is still overwriting the **template** with its own file at 
`/themes/child-theme/sidebar.php`

It is our duty as website maintainers to keep a track about what templates do we overwrite and, in the imminent case of an update, look carefully at the changelog so you update the child theme files if necessary.
    

## 2) The purpose
The child themes are meant to be a safe way to keep customizations of the main template without fearing to lose them on a theme update.

Basically, whenever you want to edit a file inside the active template from your website you have to ask yourself "**What is going to happen when I will update the theme?**

And the answer is simple: **You lose them because the update will bring an entirely new theme folder**.

So let's look at a child theme as a folder with files which will overwrite the files with the same path in the parent theme. Now let's bring some real examples:

## Definition and requirements
A child theme is identified by WordPress when there is a directory (for example `child-theme`) inside `/wp-content/themes/` with the following files:

 - `style.css`

    This file must start with a comment template like this:
    
        /*
        Theme Name: Example Child
        Author: Me
        Author URI: https://example.com/
        Template: example
        Text Domain: example-child-theme
        Domain Path: /languages/
        */

    The most important things to consider here are:
      
      - The `Template` name must be exactly the folder name which holds the parent theme (aka the parent theme slug)

      - Name your child theme in such a way you can easily identify it in Dashboard (usually just append `Child` to the parent's name, like `Example Child`)

 - `index.php`
 - `functions.php`

## 3) Template overwriting
The most common usage of a child theme is to override template parts. For example, a sidebar, if we have a theme with the following file at

`/themes/template/sidebar.php`

    <?php
    /**
     * The sidebar containing the main widget area.
     *
     * @link https://developer.wordpress.org/themes/basics/template-files/#template-partials
     */ 

    if ( ! is_active_sidebar( 'sidebar-1' ) ) {
        return;
    }?>
    <div id="sidebar" class="widget-area">
        <?php dynamic_sidebar( 'sidebar-1' ); ?>
    </div>

We can definitely add our own file in child theme (with the specifications from the first example) with the following file


`/themes/child-theme/sidebar.php`


    <?php
    /**
     * The sidebar containing the main widget area.
     */ 
    
    if ( ! is_active_sidebar( 'sidebar-1' ) ) {
        return;
    }?>
    <div id="my-sidebar" class="my-own-awesome-class widget-area">
        <div class="my-wrapper">
            <?php dynamic_sidebar( 'sidebar-1' ); ?>
        </div>
    </div>

Now `my-own-awesome-class` is safe in **child theme** and it won't be removed at any theme update and WordPress will always choose a template from child themes when it does find one on the same path.

## Assets replacement
Even if it is not a best practice, sometimes you need to replace assets like CSS or JS files or libraries.

**Note** that the WordPress template overwriting system doesn't work with anything else than `.php` files, so when we talk about assets we refer to [registered][1] assets


One example could be the replacement of jQuery library with your desired version. In our child theme `functions.php` file we need to add a function that removes the current `jQuery` version and add our own from CDN(remember is just an example).

    /**
     * Dequeue the jQuery script and add our own version.
     *
     * Hooked to the wp_print_scripts action, with a late priority (100),
     * so that it is after the script was enqueued.
     */
    function my_own_theme_scripts() {
        // remove the current version
        wp_dequeue_script( 'jquery' );
        // register my desired version
        wp_register_script( 'jquery', 'https://code.jquery.com/jquery-3.1.0.min.js', false, '3.1.0' );
        // load my version, here or somewhere else
        wp_enqueue_script( 'jquery' );
    }
    add_action( 'wp_print_scripts', 'my_own_theme_scripts' );




  [1]: https://developer.wordpress.org/reference/functions/wp_enqueue_script/

