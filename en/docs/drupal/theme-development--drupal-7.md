---
title: "Theme development - Drupal 7"
slug: "theme-development---drupal-7"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Theme .info File
    name = MyCompany Theme
    description = A Bootstrap Sub-theme.
    core = 7.x
    base theme = bootstrap

    ;;;;;;;;;;;;;;;;;;;;;
    ;; Regions
    ;;;;;;;;;;;;;;;;;;;;;

    regions[navigation]     = 'Navigation'
    regions[header]         = 'Top Bar'
    regions[highlighted]    = 'Highlighted'
    regions[help]           = 'Help'
    regions[content]        = 'Content'
    regions[sidebar_first]  = 'Primary'
    regions[sidebar_second] = 'Secondary'
    regions[footer]         = 'Footer'
    regions[page_top]       = 'Page top'
    regions[page_bottom]    = 'Page bottom'

    ;;;;;;;;;;;;;;;;;;;;;
    ;; MyCompany Custom Regions
    ;;;;;;;;;;;;;;;;;;;;;
    regions[footer_menu_left] = 'Footer menu left'
    regions[footer_menu_right] = 'Footer menu right'

    ;;;;;;;;;;;;;;;;;;;;;
    ;; CSS
    ;; these css files will be included on every page
    ;;;;;;;;;;;;;;;;;;;;;

    stylesheets[all][] = css/bootstrap.min.css
    stylesheets[all][] = css/MyCompany.css

    ;;;;;;;;;;;;;;;;;;;;;
    ;; JS
    ;; this JS file will be included on every page
    ;;;;;;;;;;;;;;;;;;;;;

    scripts[] = js/MyCompany.min.js

## Writing theme .info files
The .info file is a static text file for defining and configuring a theme. Each line in the .info file is a key-value pair with the key on the left and the value on the right, with an "equals sign" between them (e.g. *name = my_theme*). 

Semicolons are used to comment out a line. Some keys use a special syntax with square brackets for building a list of associated values, referred to as an "array". If you are unfamiliar with arrays, have a look at the default .info files that come with Drupal and read the explanations of the examples that follow. Even though the .info file extension is not natively opened by an Application, you can use TextEdit on a Mac or Notepad on a Windows computer in order to view, edit, and save your changes.

**Theme name requirements**

The name should start with an alphabetic character, can contain numbers and underscores, but not hyphens, spaces or punctuation. The name will be used by Drupal in forming various functions in PHP and therefore it has the same limitations.

*Do not choose names that are already used by installed modules*, as all installed components must have unique names.

One of the best practices is to use prefixes when naming a site's custom theme, to guarantee unique names for themes. A site named example.com might use theme names such as ex_themename.

Because the .info file is cached, you must clear the cache before any changes are displayed in your site.

The .info file can also specify which theme settings should be accessed from the Drupal administration interface, as you will soon see.

**Encoding**

The file must be saved as UTF-8 without a Byte Order Mark (BOM).

**Contents**

Drupal understands the keys listed below. Drupal will use default values for the optional keys not present in the .info file. See the examples set for core themes.

- [name][1] ***required***
- [description][2] ***recommended***
- [screenshot][3]
- [version][4] ***discouraged***
- [core][5] ***required***
- [engine][6]
- [base theme][7]
- [regions][8]
- [features][9]
- [theme settings][10]
- [stylesheets][11]
- [scripts][12]
- [php][13]


  [1]: https://www.drupal.org/node/171205#name
  [2]: https://www.drupal.org/node/171205#description
  [3]: https://www.drupal.org/node/171205#screenshot
  [4]: https://www.drupal.org/node/171205#version
  [5]: https://www.drupal.org/node/171205#core
  [6]: https://www.drupal.org/node/171205#engine
  [7]: https://www.drupal.org/node/171205#base-theme
  [8]: https://www.drupal.org/node/171205#regions
  [9]: https://www.drupal.org/node/171205#features
  [10]: https://www.drupal.org/node/171205#theme-settings
  [11]: https://www.drupal.org/node/171205#stylesheets
  [12]: https://www.drupal.org/node/171205#scripts
  [13]: https://www.drupal.org/node/171205#php

