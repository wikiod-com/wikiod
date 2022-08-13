---
title: "Create Template for Custom Post Type"
slug: "create-template-for-custom-post-type"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Creating a custom template for Custom Post type book
To create a template for the single posts of our custom post type, we need to create a file called single-**post_type_name**.php where **post_type_name** is the name of our custom post type.

For example, if our custom post type is called “Books”, we need to create a PHP file called single-**book**.php. Note that we used the singular name of our custom post type.

Copy the contents of the single.php from the themes folder and paste it into the new template and save it then the template would be applied for the custom post type individual page.


## Custom Post Type Templates



----------


# **Custom Post Type Archive:**

To create an archive template for a custom post type you have to set the `has_archive` argument equal to `true` in your [`register_post_type()`][1] function. In the example below a custom post type is created for an Event post type.

    add_action( 'init', 'create_events_post_type' );
    function create_events_post_type() {
      register_post_type( 'event',
        array(
          'labels' => array(
            'name' => __( 'Events' ),
            'singular_name' => __( 'Event' )
          ),
          'public' => true,
          'has_archive' => true,
        )
      );
    }

To [create a template][2] for new custom post types you will have to create a new template file. To create a template for the single post pages you would name it `single-{post_type}.php` and `archive-{post_type}.php` for the archive.

The filename for our archive template will be `archive-event.php` and for the event page it would be `single-event.php`. Both files should be in your themes root directory.

An example archive template would look like this. Pulled from the [twentyseventeen theme][3].
```
<?php
/**
 * The template for displaying archive pages
 *
 * @link https://codex.wordpress.org/Template_Hierarchy
 *
 * @package WordPress
 * @subpackage Twenty_Seventeen
 * @since 1.0
 * @version 1.0
 */

get_header(); ?>

<div class="wrap">

    <?php if ( have_posts() ) : ?>
        <header class="page-header">
            <?php
                the_archive_title( '<h1 class="page-title">', '</h1>' );
                the_archive_description( '<div class="taxonomy-description">', '</div>' );
            ?>
        </header><!-- .page-header -->
    <?php endif; ?>

    <div id="primary" class="content-area">
        <main id="main" class="site-main" role="main">

        <?php
        if ( have_posts() ) : ?>
            <?php
            /* Start the Loop */
            while ( have_posts() ) : the_post();

                /*
                 * Include the Post-Format-specific template for the content.
                 * If you want to override this in a child theme, then include a file
                 * called content-___.php (where ___ is the Post Format name) and that will be used instead.
                 */
                get_template_part( 'template-parts/post/content', get_post_format() );

            endwhile;

            the_posts_pagination( array(
                'prev_text' => twentyseventeen_get_svg( array( 'icon' => 'arrow-left' ) ) . '<span class="screen-reader-text">' . __( 'Previous page', 'twentyseventeen' ) . '</span>',
                'next_text' => '<span class="screen-reader-text">' . __( 'Next page', 'twentyseventeen' ) . '</span>' . twentyseventeen_get_svg( array( 'icon' => 'arrow-right' ) ),
                'before_page_number' => '<span class="meta-nav screen-reader-text">' . __( 'Page', 'twentyseventeen' ) . ' </span>',
            ) );

        else :

            get_template_part( 'template-parts/post/content', 'none' );

        endif; ?>

        </main><!-- #main -->
    </div><!-- #primary -->
    <?php get_sidebar(); ?>
</div><!-- .wrap -->

<?php get_footer();
```

# **Custom Post Type Single template:**

Here is an example of a single template. Pulled from the [twentyseventeen theme][4].


----------



```
<?php
/**
 * The template for displaying all single posts
 *
 * @link https://developer.wordpress.org/themes/basics/template-hierarchy/#single-post
 *
 * @package WordPress
 * @subpackage Twenty_Seventeen
 * @since 1.0
 * @version 1.0
 */

get_header(); ?>

<div class="wrap">
    <div id="primary" class="content-area">
        <main id="main" class="site-main" role="main">

            <?php
                /* Start the Loop */
                while ( have_posts() ) : the_post();

                    get_template_part( 'template-parts/post/content', get_post_format() );

                    // If comments are open or we have at least one comment, load up the comment template.
                    if ( comments_open() || get_comments_number() ) :
                        comments_template();
                    endif;

                    the_post_navigation( array(
                        'prev_text' => '<span class="screen-reader-text">' . __( 'Previous Post', 'twentyseventeen' ) . '</span><span aria-hidden="true" class="nav-subtitle">' . __( 'Previous', 'twentyseventeen' ) . '</span> <span class="nav-title"><span class="nav-title-icon-wrapper">' . twentyseventeen_get_svg( array( 'icon' => 'arrow-left' ) ) . '</span>%title</span>',
                        'next_text' => '<span class="screen-reader-text">' . __( 'Next Post', 'twentyseventeen' ) . '</span><span aria-hidden="true" class="nav-subtitle">' . __( 'Next', 'twentyseventeen' ) . '</span> <span class="nav-title">%title<span class="nav-title-icon-wrapper">' . twentyseventeen_get_svg( array( 'icon' => 'arrow-right' ) ) . '</span></span>',
                    ) );

                endwhile; // End of the loop.
            ?>

        </main><!-- #main -->
    </div><!-- #primary -->
    <?php get_sidebar(); ?>
</div><!-- .wrap -->

<?php get_footer();

```

**Both template examples are pulling in [partials][5] to display the inner content.**

If your child/parent theme has a single/archive template you should use that code as boilerplate for your new templates.


  [1]: https://codex.wordpress.org/Function_Reference/register_post_type
  [2]: https://codex.wordpress.org/Post_Types#Custom_Post_Type_Templates
  [3]: https://github.com/WordPress/twentyseventeen/blob/master/archive.php
  [4]: https://github.com/WordPress/twentyseventeen/blob/master/single.php
  [5]: https://github.com/WordPress/twentyseventeen/tree/master/components

