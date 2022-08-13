---
title: "WordPress Plugin creation"
slug: "wordpress-plugin-creation"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

WordPress plugins should have a focus on the server logic and/or admin parts of your website application. Good plugins are like good apps, they do one thing really well. They are intended to enhance and automate parts of the CMS in a modular way, since you can activate and deactivate them. Good plugins make use of WordPress core actions, filters, and existing javascript and css frameworks.

## Minimal Setup of a Plugin Folder and Files
First step of creating a plugin is creating the folder and file which the plugin will load from.

Plugins are located in `/wp-content/plugins/`.

The WordPress standard is to create a folder and filename that mirror each other like so:
```
/wp-content/plugins/myplugin/
/wp-content/plugins/myplugin/myplugin.php
```

After creating your plugin file you need to start your plugin with a `Plugin Header`. This allows WordPress to scan your plugin file and store the meta data about the plugin, and allow users to use this and determine if they want your plugin active, or inactive. Copy this template into the top of your main plugin file you created, and modify it as needed:
```
<?php
/**
 * Plugin Name: PLUGIN-NAME
 * Plugin URI: HTTP-LINK-TO-WEBSITE-PLUGIN-PAGE-OR-REPO
 * Description: BREIF DESCRIPTION - KEEP IT SHORT
 * Author: WORDPRESS-DOT-ORG-USERNAME
 * Version: 0.0.1
 * Author URI: HTTP-LINK-TO-MAINTAINER
 * License: GNU General Public License v2 or later
 * License URI: http://www.gnu.org/licenses/gpl-2.0.html
 * Text Domain: short_prefix
 */

// Begin custom PHP WordPress plugin work
```
*Note that WordPress plugins should typically be licensed as GPL. Licensing should not be discussed as part of this topic though.*

At this point, you should already by able to see your new plugin in the WordPress Admin area. In a standard setup you would locate this area at `/wp-admin/plugins.php`. Go ahead and activate your plugin, and you are ready move into the next steps of building your plugin!

Just to end our example on something actionable, you could now add the following to the bottom of your plugin file:

```php
die('My custom plugin is loaded : '. __FILE__);
```

Refreshing your site after this change should result in all pages printing this text. *Never do this in production (live) sites, and always remember to take this back out before continuing.*

