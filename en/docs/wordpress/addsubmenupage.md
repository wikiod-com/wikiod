---
title: "add_submenu_page()"
slug: "add_submenu_page"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

This function is to add a sub-item to an existing item in the admin panels nav bar.

## Syntax
 - add_submenu_page( $parent_slug, $page_title, $menu_title, $capability, $menu_slug, $function )

## Parameters
| Parameter | Details |
| ------ | ------ |
| $parent_slug   | (string) The slug name for the parent menu (or the file name of a standard WordPress admin page).|
| $page_title| (string) The text to be displayed in the title tags of the page when the menu is selected.|
| $menu_title| (string) The text to be used for the menu.|
| $capability| (string) The capability required for this menu to be displayed to the user.|
| $menu_slug| (string) The slug name to refer to this menu by (should be unique for this menu).|
| $function | (callable) (Optional) The function to be called to output the content for this page.|

Here are a list of slugs for $parent_slug

 - Dashboard: ‘index.php’
 - Posts: ‘edit.php’
 - Media: ‘upload.php’
 - Pages: ‘edit.php?post_type=page’
 - Comments: ‘edit-comments.php’
 - Custom Post Types: ‘edit.php?post_type=your_post_type’
 - Appearance: ‘themes.php’
 - Plugins: ‘plugins.php’
 - Users: ‘users.php’
 - Tools: ‘tools.php’
 - Settings: ‘options-general.php’
 - Network Settings: ‘settings.php’

## Adding the "Submenu Page" as a sub-page of "Tools" to the nav bar
**Code**

    add_action('admin_menu', 'register_my_custom_submenu_page');
     
    function register_my_custom_submenu_page() {
        add_submenu_page(
            'tools.php',
            'Submenu Page',
            'My Custom Submenu Page',
            'manage_options',
            'my-custom-submenu-page',
            'my_custom_submenu_page_content' );
    }
     
    function my_custom_submenu_page_content() {
        echo '<div class="wrap">';
            echo '<h2>Page Title</h2>';
        echo '</div>';
    }

**Output**

[![enter image description here][1]][1]


**Explanation**

In the code, we created a function named `register_my_custom_submenu_page` and we used `add_submenu_page` to add the item to the navbar as a child of tools.php, which is the Tools page. 

Please check the parameters part in this page to know about the arguments we passed in. Then we used `add_action` to run our `register_my_custom_submenu_page` function. Finally, we created the function `my_custom_submenu_page_content` to display contents in the page.

  [1]: https://i.stack.imgur.com/PFnkN.png

