---
title: "Package Control"
slug: "package-control"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Package Control is a full-featured package manager that helps discovering, installing, updating and removing packages for Sublime Text.

Package Control is the Sublime Text package manager. It includes a list of over 2,500 packages available for install, and users can add any GitHub or BitBucket repository themselves. Once installed, packages are kept up-to-date automatically.

The example code creates the Installed Packages folder for you (if necessary), and then downloads the Package Control.sublime-package into it. The download will be done over HTTP instead of HTTPS due to Python standard library limitations, however the file will be validated using SHA-256.

## Installing Package Control
If you are using Sublime Text 3 then the simplest way to install *Package Control* is to select `Install Package Control` in the `Tools` menu or in the `Command Palette` and Sublime Text will install it automatically.

### Manually Installing:

For Sublime Text 2, older versions of Sublime Text 3, or if you have a proxy server related problem, then *Package Control* can be installed using a Python script that must be pasted into the Sublime Text console.

- Open the [Package Control Installation Web Page][1] in your web browser.
- Click on the Sublime Text version you require.
- Copy the Python script into your clipboard.
- Open the Sublime Text console by selecting the `Show Console` in the `View` menu, or by using the `` Ctrl+` `` shortcut keys, or by selecting `Console: Show` in the `Command Palette`.
- Paste the Python script into the console and press the `Enter` key to run the script.
- It will take a few seconds to install but once it has been a new Sublime Text buffer will be displayed with information about *Package Control*.

You can new go to the [Package Control Web Site][2] and choose the packages that you want to install.

[1]: https://packagecontrol.io/installation
[2]: https://packagecontrol.io


## Customizing sublime text
Once you have package control installed, it is super easy to install any *plugin*, *theme*, *color scheme*, *syntax* that you want!

- plugin: perform an action (compile your less code into css for example)
- theme: change the entire skin of sublime text (tabs, sidebar, command palette, etc)
- color scheme: change the color of your code
- syntax: define *how* code should be highlighted.

## How do I find a package 

You can search for packages on this website: [packagecontrol.io][1], or simply search for some key words when you decide you need to install a package (see below).


## Once I choose which package I want to install, how do I install it?

From ST, bring up the command palette, and type `install package` until `package control: install package` is hightlighted, and press enter. 

Wait a bit, and a list with all the package available will come up. Type the name of the one you want to install, and it `enter`. Done!

**Note:** It is safe to restart sublime text after you've install a package. But it become less and less useful (sublime text gets better, so you almost don't need to now). So, just restart it if something is weird, not just because I said it. 

  [1]: https://packagecontrol.io



## Installing an unlisted package
By *unlisted* package, I mean a package that is not available through Package Control (yet). So, you can't find it in [packagecontrol.io][].

BUT, you can still install it using Package Control, so you'll get all the advantages. For example, they'll be automatically updated, just like a "regular" installation.

And it's *really* easy.

- Find your package repository (it'll often be on GitHub, but you can use a BitBucket one) and copy the URL. 
- Search for `Package Control: Add repository` in the command palette (<kbd>ctrl+shift+p</kbd>)
- Paste the URL
- Hit <kbd>enter</kbd>

And now, it's just like if this package was on the default channel (a channel is a simple list of packages. The default one is the one you get... by default 😃).

- Search for `Package Control: Install Package` in the command palette (<kbd>ctrl+shift+p</kbd>)
- (you might need to wait a few secs) Search for the package you want to install
- Hit <kbd>enter</kbd>!

That's it! Pretty cool, huh? So, if you're a package developer don't hesitate to let them know they can do it this way (always better than the `git clone` and `git pull` 😉). 

*Note*: Feel free to copy/paste this text in your readme, or adapt your own version!

[packagecontrol.io]: https://packagecontrol.io

