---
title: "Drupal Folder Structure"
slug: "drupal-folder-structure"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

This page contains information related to default Drupal 7 installation folder strucutre and related information.

 - **includes** - This folder contains include files shipped with Drupal 7. These inc files are majorly the Drupal 7 APIs. For example - batch.inc, cache.inc, ajax.inc etc. This folder **MUST** not be used for any custom development or keeping custom inc files.

 - **misc** - This folder contains miscellaneous JavaScript and Images used by Drupal 7. This folder **MUST** not be used for keeping any JavaScript/Images used by website.

 - **modules** - This folder contains all the default modules shipped with Drupal 7. This folder **MUST** not be used for keeping any custom/contributed modules. There is a separate place available for keeping custom/contributed modules, explained in this documentation later.

 - **profiles** - This folder contains the default installation profiles shipped with Drupal 7. For example - Minimal, Standard etc. These profiles are selected while installing Drupal 7. Profiles are the combination of Themes, Modules and Configurations by default available after installation. This folder can be used for development of custom profile.

 - **scripts** - This folder contains default shell scripts shipped with Drupal 7. For example - Password Hash Reset script, Drupal shell etc.

 - **sites** - This folder is used for keeping files any website specific files. For single and multisite setups both, this folder is used for the purpose. This folder by default contains two folders i.e. all, default.

      - **all** - This folder should be used for keeping any custom/contributed modules/themes/files which can be used by all the sites available in the Drupal installation.

      - **default** - This folder should be used for keeping custom/contributed modules/themes/files which can be used by the default site of the Drupal installation.

       For other sites multiple folder can be created in sites folder.

 - **themes** - This folder contains all the default themes shipped with Drupal 7. This folder **MUST** not be used for keeping any custom/contributed themes.

## Screenshot
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/mo177.png

