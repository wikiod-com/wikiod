---
title: "Custom Theme"
slug: "custom-theme"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

**`luma`** theme as parent

    {
        "name": "magento/luma",
        "description": "N/A",
        "require": {
            "php": "~5.5.0|~5.6.0|~7.0.0",
            "magento/theme-luma": "100.0.*",
            "magento/framework": "100.0.*"
        },
        "type": "magento2-theme",
        "version": "100.0.1",
        "license": [
            "OSL-3.0",
            "AFL-3.0"
        ],
        "autoload": {
            "files": [
                "registration.php"
            ]
        }
    }

**at the end**

Run `php bin/magento setup:upgrade` this command after than below commands also needed sometimes

- `php bin/magento setup:static-content:deploy <language_pack_1> <language_pack_2> ... <language_pack_n>`
   - **<language_pack>**: `en_US` `nl_NL` `en_GB` etc

- `php bin/magento cache:flush` or `php bin/magento cache:clean`

## Sample Theme
Theme.xml

`app/design/frontend/Magento/mytheme/theme.xml`

    <theme xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="urn:magento:framework:Config/etc/theme.xsd">
         <title>My theme</title> <!-- your theme's name -->
         <parent>Magento/blank</parent> <!-- the parent theme, in case your theme inherits from an existing theme -->
         <media>
             <preview_image>media/preview.jpg</preview_image> <!-- the path to your theme's preview image -->
         </media>
     </theme>


`app/design/frontend/Magento/mytheme/composer.json`

    {
        "name": "magento/theme-frontend-blank",
        "description": "N/A",
        "require": {
            "php": "~5.5.0|~5.6.0|~7.0.0",
            "magento/theme-frontend-blank": "100.0.*",
            "magento/framework": "100.0.*"
        },
        "type": "magento2-theme",
        "version": "100.0.1",
        "license": [
            "OSL-3.0",
            "AFL-3.0"
        ],
        "autoload": {
            "files": [
                "registration.php"
            ]
        }
    }

   `app/design/frontend/Magento/mytheme/registration.php` 

    <?php
    /**
    * Copyright © 2015 Magento. All rights reserved.
    * See COPYING.txt for license details.
    */
    \Magento\Framework\Component\ComponentRegistrar::register(
        \Magento\Framework\Component\ComponentRegistrar::THEME,
        'frontend/Magento/mytheme',
        __DIR__
    );

  **at the end** 

  

    php bin/magento setup:upgrade

