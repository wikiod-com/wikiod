---
title: "Post Formats"
slug: "post-formats"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The following Post Formats are available for users to choose from, if the theme enables support for them.

Note that while the actual post content entry won't change, the theme can use this user choice to display the post differently based on the format chosen. For example, a theme could leave off the display of the title for a "Status" post. How things are displayed is entirely up to the theme, but here are some general guidelines.

- aside - Typically styled without a title. Similar to a Facebook note update.
- gallery - A gallery of images. Post will likely contain a gallery shortcode and will have image attachments.
- link - A link to another site. Themes may wish to use the first <a href=””> tag in the post content as the external link for that post. An alternative approach could be if the post consists only of a URL, then that will be the URL and the title (post_title) will be the name attached to the anchor for it.
- image - A single image. The first <img /> tag in the post could be considered the image. Alternatively, if the post consists only of a URL, that will be the image URL and the title of the post (post_title) will be the title attribute for the image.
- quote - A quotation. Probably will contain a blockquote holding the quote content. Alternatively, the quote may be just the content, with the source/author being the title.
- status - A short status update, similar to a Twitter status update.
- video - A single video or video playlist. The first <video /> tag or object/embed in the post content could be considered the video. Alternatively, if the post consists only of a URL, that will be the video URL. May also contain the video as an attachment to the post, if video support is enabled on the blog (like via a plugin).
audio - An audio file or playlist. Could be used for Podcasting.
- chat - A chat transcript

## Adding post type to Theme
### Add post-formats to post_type 'page'

    add_post_type_support( 'page', 'post-formats' );

Next example registers custom post type 'my_custom_post_type', and add Post Formats.

### Register custom post type 'my_custom_post_type'

    add_action( 'init', 'create_my_post_type' );
    function create_my_post_type() {
        register_post_type( 'my_custom_post_type',
          array(
            'labels' => array( 'name' => __( 'Products' ) ),
            'public' => true
        )
      );
    }

### Add post-formats to post_type 'my_custom_post_type'

    add_post_type_support( 'my_custom_post_type', 'post-formats' );

Or in the function register_post_type(), add 'post-formats', in 'supports' parameter array. Next example is equivalent to above one.

### Register custom post type 'my_custom_post_type' with 'supports' parameter

    add_action( 'init', 'create_my_post_type' );
    function create_my_post_type() {
        register_post_type( 'my_custom_post_type',
          array(
            'labels' => array( 'name' => __( 'Products' ) ),
            'public' => true,
            'supports' => array('title', 'editor', 'post-formats')
        )
      );
    } 



## Add Theme Support for post
### Function Call
    add_theme_support( 'post-formats' )



