---
title: "Vocabulary"
slug: "vocabulary"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

This is a really small part, but still essential if you want to be able to communicate efficiently with other people using Sublime Text 3. 

*More to come: `settings`, `keymap`, `mousemap` etc...*

## Themes
A theme changes the global skin of Sublime Text 3. It changes the tabs, side bar, quick panels (goto, command palette), status bar, etc...

Here's a non-exhaustive list of themes (top 100):

- [Predawn](https://packagecontrol.io/packages/Predawn)
- [Material Theme](https://packagecontrol.io/packages/Material%20Theme)
- [Theme - Spacegray](https://packagecontrol.io/packages/Theme%20-%20Spacegray)
- [Seti_UI](https://packagecontrol.io/packages/Seti_UI)
- [Theme - Brogrammer](https://packagecontrol.io/packages/Theme%20-%20Brogrammer)
- [Theme - Flatland](https://packagecontrol.io/packages/Theme%20-%20Flatland)
- [Theme - Soda](https://packagecontrol.io/packages/Theme%20-%20Soda)

Note: the convention for the theme *packages* name is that they start with `Theme -`. 

```
Theme - Focus
    img/
    focus.light.sublime-theme
    focus.dark.sublime-theme
```

As you can see, this convention isn't really respected.

The themes are defined in a `.sublime-theme` file.

## Color Schemes
The color schemes changes the colors of the *code*. They can change, for example, with which color the keywords are highlighted, with which color the line the caret is on is highlighted (needs `highlight_line` to be set to `true` in the preferences (not part of the cojor scheme)), what is the color of the caret, etc.

Many themes come with color schemes, or many packages contain plenty of them.

*needs example of packages*

The color scheme are defined in a `.tmTheme` file. This is an XML file in the Property List format, which is used by many text editors.

The convention for the color schemes *packages* name is that they start with `Color Scheme -`

```
Color Scheme - Focus
    first.tmTheme
    second.tmTheme
    ...
```

## Tools

- you can use this web app to create/edit your color schemes: [tmTheme editor](http://tmtheme-editor.herokuapp.com/#!/editor/theme/Monokai).
- An other tool is [ColorSchemeEditor](https://packagecontrol.io/packages/ColorSchemeEditor) which allows to edit your color scheme *from* Sublime Text.
- You can also switch very quickly of color scheme using [Schemr](https://packagecontrol.io/packages/Schemr)




## Plugins
Plugins are `.py` files that changes the *behaviour* of Sublime Text 3. They are written, as you probably guessed, in [Python](https://www.python.org/). 

Because there is plenty of different sort of plugins (text manipulation, linting, formatting, preview, etc), we will not give any example to not over populate this page with a huge list. Have a look at the [Package Control browsing page](https://packagecontrol.io/browse) if you're interested in optimising your workflow with some plugins (you should be).


## Packages
A package is a folder that can contain anything that is listed in these examples (and other thing that can have nothing to do with Sublime Text 3, such as a `gulpfile.js` if you're automating some tasks). 

You can install any package using the create [Package Control](https://packagecontrol.io).

### A `.sublime-package` ?

Maybe you've seen that, when you install a package using package control, you only get a `.sublime-package` in the `Install Packages` folder... How come you don't get the code? In fact, a `.sublime-package` is a `.zip` file renamed. You can extract or preview some files *inside* a `.sublime-package` using [PackageResourceViewer](https://packagecontrol.io/packages/PackageResourceViewer).



## Settings
The settings, are, as many things on Sublime Text 3, simple [`JSON`](http://json.org/) files. Though, the extension of the file isn't `.json` but `.sublime-settings`. As you probably understood, the settings changes how the packages affects Sublime Text 3. *It is important to be aware of as much settings as possible to optimize your workflow*.

The system for most of the packages is the following: a default one, and a user one which overwrites the default one.

To understand the priority of the settings (which is strongly recommend), have a look at the unofficial-but-awesome documentation: <http://docs.sublimetext.info/en/latest/customization/settings.html#the-settings-hierarchy>


