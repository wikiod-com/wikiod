---
title: "Themes and Packages"
slug: "themes-and-packages"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Atom's packages allow you to customize the editor to your needs. This topic will explains how packages and themes are created, published, and installed.

## Downloading and Installing Packages and Themes
To view your installed packages or themes, open settings with <kbd>Ctrl</kbd>+<kbd>,</kbd> and select either the "Packages" or "Themes" tab in the left-hand navigation bar. Note, the packages or themes you installed from other publishers will show up under the "Community Themes" section and themes pre-installed with Atom will show up under the "Core Themes" section.

# Packages 

 1. Press <kbd>Ctrl</kbd>+<kbd>,</kbd> to open the settings tab
 2. Select the "Install" item on the left navigation pane
 3. Ensure the "Packages" button is selected in the top right

[![package button selected][1]][1]
 4. Use the search bar at the top to find a package (ex. `file icons`)
 5. Click the Install button to download and install the package

[![install package button][2]][2]

To view information on packages and their settings, click the package name.

Browse Atom packages online [here](https://atom.io/packages).

# Themes

Downloading and installing themes follows a similar process to that of packages.

 1. Press <kbd>Ctrl</kbd>+<kbd>,</kbd> to open the settings tab
 2. Select the "Install" item on the left navigation pane
 3. Ensure the "Themes" option is selected by the search bar.
 4. Search for a theme (ex. `atom-sublime-monokai-syntax`)
 5. Click the install button to download and install

To view information on themes and their settings, click the theme name.

Browse Atom themes online [here](https://atom.io/themes).

  [1]: https://i.stack.imgur.com/uxRg2.png
  [2]: https://i.stack.imgur.com/SSxu2.png

## Use Atom Package Manager
[apm][1] is Atom's native package manager. It allows the user to manage packages and themes without having to initialise Atom itself. `apm` comes with the official installation and is automatically added to `%PATH%` if you're on Windows.

To use `apm`, go to Command Prompt and type

    $ apm <command>

Here is the list of what you can do with this package manager.

    clean, config, dedupe, deinstall, delete, dev, develop, disable, docs,
    enable, erase, featured, home, i, init, install, link, linked, links, list,
    ln, lns, login, ls, open, outdated, publish, rebuild, rebuild-module-cache,
    remove, rm, search, show, star, starred, stars, test, uninstall, unlink,
    unpublish, unstar, update, upgrade, view.

For example, if you want to do upgrade all packages from atom: 

    apm upgrade --confirm false

Or if you want to install a specific package:

    apm install <package_name>


  [1]: https://github.com/atom/apm

