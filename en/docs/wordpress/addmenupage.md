---
title: "add_menu_page()"
slug: "add_menu_page"
draft: false
images: []
weight: 9912
type: docs
toc: true
---

This function is to add an item in the admin panel's nav bar.

## Syntax
 - add_menu_page( $page_title, $menu_title, $capability, $menu_slug, $function, $icon_url, $position)

## Parameters
| Parameter | Details |
| ------ | ------ |
| $page_title | (string) The text to be displayed in the title tags of the page when the menu is selected.   |
| $menu_title | (string) The text to be used for the menu.   |
| $capability | (string) The capability required for this menu to be displayed to the user.   |
| $menu_slug | (string) The slug name to refer to this menu by (should be unique for this menu). |
| $function | (callable) (optional) The function to be called to output the content for this page.|
| $icon_url | (string) (optional) The URL to the icon to be used for this menu.|
| $position | (int) (optional) The position in the menu order this one should appear.   |

Here is a list of the default positions (for $position)

 - 2 – Dashboard
 - 4 – Separator
 - 5 – Posts
 - 10 – Media
 - 15 – Links
 - 20 – Pages
 - 25 – Comments
 - 59 – Separator
 - 60 – Appearance
 - 65 – Plugins
 - 70 – Users
 - 75 – Tools
 - 80 – Settings
 - 99 – Separator

## Adding the "Theme page title" item to the nav bar
**Code**

    function add_the_theme_page(){
        add_menu_page('Theme page title', 'Theme menu label', 'manage_options', 'theme-options', 'page_content', 'dashicons-book-alt');
    }
    add_action('admin_menu', 'add_the_theme_page');
    function page_content(){
        echo '<div class="wrap"><h2>Testing</h2></div>';
    }

**Output**

[![enter image description here][1]][1]

**Explanation**

In the code, we created a function named `add_the_theme_page` and we used `add_menu_page` to add the item to the navbar. Please check the parameters part in this page to know about the arguments we passed in. Then we used `add_action` to run our `add_the_theme_page` function. Finally, we created the function `page_content` to display contents in the page.

  [1]: https://i.stack.imgur.com/uHyIy.png

## OOP & how to load scripts/styles on menu page
    <?php
    /*
     *  Plugin Name: Custom Admin Menu
     */
    
    class SO_WP_Menu {
    
        private $plugin_url;
        
        public function __construct() {
            $this->plugin_url = plugins_url( '/', __FILE__ );
            add_action( 'plugins_loaded', array( $this, 'init' ) );
        }
    
        public function init() {
            add_action( 'admin_menu', array( $this, 'add_menu' ) );
        }
    
        public function add_menu() {
            $hook = add_menu_page(
                'My Menu',                 // Title, html meta tag
                '<span style="color:#e57300;">My Menu</span>', // Menu title, hardcoded style
                'edit_pages',              // capability
                'dummy-page-slug',         // URL
                array( $this, 'content' ), // output
                null,                      // icon, uses default
                1                          // position, showing on top of all others
            );
            add_action( "admin_print_scripts-$hook", array( $this, 'scripts' ) );
            add_action( "admin_print_styles-$hook", array( $this, 'styles' ) );
        }
    
        public function content() {
            ?>
                <div id="icon-post" class="icon32"></div>
                <h2>Dummy Page</h2>
                <p> Lorem ipsum</p>
            <?php
        }
    
        # Printing directly, could be wp_enqueue_script
        public function scripts() {
            ?><script>alert('My page');</script><?php
        }
    
        # Enqueing from a CSS file on plugin directory
        public function styles() {
            wp_enqueue_style( 'my-menu', $this->plugin_url . 'my-menu.css' );
        }
    }
    
    new SO_WP_Menu();

What's important to note in this example is that, when using `add_menu_page()`, it returns a hook that can be used to target our exact page and load Styles and Scripts there.  
A common mistake is to enqueue without targeting and that spills scripts and styles all over `/wp-admin`.  
Using OOP we can store common variables to be used among internal methods.

