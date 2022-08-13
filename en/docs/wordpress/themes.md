---
title: "Themes"
slug: "themes"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

WordPress themes are the front-end of your website. They are what people see when they visit the site. There are thousands of themes to choose from, paid and free versions. You can even create your own custom theme with just a couple of necessary files.

## WordPress Themes
## How To Choose A Theme ##

Each WordPress install comes with a pre-installed theme. You manage your themes from the Dashboard. Go to Appearance > Themes to install, preview, delete, activate, and update Themes. The current theme is found in the upper left corner of this menu. 

Hovering over the theme image reveals a 'Theme Details' button. This button provides information about the theme, such as the version and description. Clicking on the current theme image gives you access to customize specific theme settings, like the title.

## Update Available ##

If updates are available for installed themes, you'll find a message that informs you that a new version is available. You should be able to view the new version details or update now.

 - View Version Details

    Clicking the version details link will navigate you to a page from the               WordPress Theme Directory. Here you'll find the details for the upgrade version.

 - Update Now 

    Clicking the update now link will install the Theme upgrade. Themes can also be upgraded from the Administration > Dashboard > Updates screen. 

In addition to your current Theme, the Manage Themes screen also shows the other Themes that are installed but currently inactive. Each Theme is represented by a small screenshot. Hovering over these images reveals 'Theme Details', 'Activate', and 'Live Preview' buttons. You'll also be able to upgrade or delete inactive themes from this page. Each page on this screen will display up to 15 Theme screenshots at a time.

 - Activate

    Clicking this link makes this the Current Theme.

 - Live Preview

    Clicking this link displays a preview of blog with this specific theme version.

 - Delete

    Clicking this link completely deletes this Theme, include all Theme files and folders. Anything not backed up will be lost forever.

 - Update Available

    Refer to the Update Available section above.

## Install Themes ##

Listed below are several ways to install Themes:

 - Automated Theme Installer
   
    This can be used to install Themes from the WordPress Theme Directory. Go to Administration > Appearance > Themes to find the Appearance Themes Screen. Click the Add New button. From here you'll find Themes to use that are free of change. At the top of this screen there is search feature with three available methods to find a new Theme; Filter, Keyword, and Attribute search.

 - Using The Upload Method

    The upload method installs a Theme via a ZIP file. All the Themes in the WordPress Theme Directory can be installed this way. After downloading the ZIP file, visit Administration > Appearance > Themes, and click the Add New button. Next, click the Upload Theme link. Browse for the ZIP file and click Install Now. To finish making this the Current Theme click the Activate link.

 - Using The FTP Method
    
    To install a Theme with the FTP Method you must first download the Theme files to your local computer. Extract the contents of ZIP file, preserving the file structure, and add them to a new folder. If there are instructions from the Theme author be sure to follow them. 

    Use an FTP client to access your site's web server. Add the uploaded Theme files to your wp-content/themes directory provided by WordPress. If need be, create a folder to contain your new Theme inside the wp-content/themes directory. An example of this would be, if your Theme is named Test it should live in wp-content/themes/test.

    Go to Administration > Appearance > Themes, and click the Activate link select the Theme as your Current Theme.

 - Installing With cPanel

    cPanel control panels offer another method to install Themes with a ZIP or GZ files. In the cPanel Manager, If WordPress is installed, go to your Themes folder. The path would look similar to 'public_html/wp-content/themes'. Click on Upload file(s) and upload the ZIP file. Select the ZIP file in cPanel and click on Extract File Contents in the panel to the right to uncompress that file.

    Go to Administration > Appearance > Themes, and click the Activate link select the Theme as your Current Theme.

All information listed above is according to the WordPress Codex. It has been shortened for brevity. The original source material can be found [here](https://codex.wordpress.org/Using_Themes). Or for more information visit [codex.wordpress.org](https://codex.wordpress.org/).

## Creating A Custom Theme ## 

These instructions create a very basic, minimum standards compliant WordPress Theme. 

The first step is to create a new theme folder inside your WordPress themes directory. The correct path will be: > wp-content > themes > <your-theme-here>
To create a valid theme, WordPress themes require at least these two files live there:

 - index.php

 - style.css

Your stylesheet should contain a comment that alerts WordPress that a theme exists here.

    /*
    Theme Name: <theme name>
    Author: <author name>
    Description: <description goes here>
    Version: <theme version #>
    Tags: <tag to id theme>
    */

Your theme has now been created. Go to the WordPress dashboard, and click on Appearance > Themes, to activate it.

