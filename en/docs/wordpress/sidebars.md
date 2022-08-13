---
title: "Sidebars"
slug: "sidebars"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Syntax
 - register_sidebar( $args )
- get_sidebar(string $name = null)

## Parameters
| Parameter | Details |
| --------- | ------- |
| *$args*   | *(string \| array) (Optional)* Builds sidebar based on the `name` and `id` vvalues |
| *$name*   | *(string) (Optional) The name of the specialised sidebar. Default value: null
 |

Argument options are:

 - **name** - Sidebar name *(default: localized 'Sidebar' and numeric ID)*.
- **id** - Sidebar id - Must be all in lowercase, with no spaces *(default: a numeric auto-incremented ID)*. If you do not set the id argument value, you will get `E_USER_NOTICE` messages in debug mode, starting with version 4.2.
 - **description** - Text description of what/where the sidebar is. Shown on widget management screen. (Since 2.9) *(default: empty)*
 - **class** - CSS class to assign to the Sidebar in the Appearance -> Widget admin page. This class will only appear in the source of the WordPress Widget admin page. It will not be included in the front end of your website. **Note**: The value `sidebar` will be prepended to the class value. For example, a class of `tal` will result in a class value of `sidebar-tal`. *(default: empty)*.
 - **before_widget** - HTML to place before every widget *(default:* `<li id="%1$s" class="widget %2$s">`*)* **Note**: uses `sprintf` for variable substitution
 - **after_widget** - HTML to place after every widget *(default:* `</li>\n`*)*.
 - **before_title** - HTML to place before every title *(default:* `<h2 class="widgettitle">`*)*.
 - **after_title** - HTML to place after every title *(default:* `</h2>\n`*)*.

## Registering sidebars
In your `functions.php` you can register new sidebars with this code

    /**
     * Registers sidebars
     *
     * @param array Array with default or specified array values
     * @since       1.0.0
     */
    if ( function_exists( 'register_sidebar' ) ) {
        register_sidebar( array (
            'name'          => esc_html__( 'Primary Sidebar', 'mytheme'),
            'id'            => 'primary-widget-area',
            'description'   => esc_html__( 'The Primary Widget Area', 'mytheme'),
            'before_widget' => '<div id="%1$s" class="widget %2$s">',
            'after_widget'  => '</div>',
            'before_title'  => '<div class="sidebar-widget-heading"><h3>',
            'after_title'   => '</h3></div>',
        ) );

        register_sidebar( array (
            'name'          => esc_html__( 'Secondary Sidebar', 'mytheme'),
            'id'            => 'secondary-widget-area',
            'description'   => esc_html__( 'The Secondary Widget Area', 'mytheme'),
            'before_widget' => '<div id="%1$s" class="widget %2$s">',
            'after_widget'  => '</div>',
            'before_title'  => '<div class="sidebar-widget-heading"><h3>',
            'after_title'   => '</h3></div>',
        ) );
    }

You can add as many sidebars as you want to.

## Get Sidebar
You can also create your own sidebar file in the theme to call it on different templates.
Copy and paste sidebar.php of current theme and change the name (i.e. sidebar-book.php)

In the template you can call this sidebar using `get_sidebar('book')`. Using this you can call different sidebars on different pages.

