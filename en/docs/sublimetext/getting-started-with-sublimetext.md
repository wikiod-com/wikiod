---
title: "Getting started with sublimetext"
slug: "getting-started-with-sublimetext"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Customizing User Interface/Theme
To customize Sublime Text (including themes, syntax highlighting etc) you must have package control installed.

To install package control visit [www.packagecontrol.io/installation][1].

**Instead of following the above link**, you can open the Sublime console to install it. The console is accessed via the `ctrl+` shortcut or the `View > Show Console` menu. Once open, paste the following Python code for your version of Sublime Text into the console.

    import urllib.request,os,hashlib; h = 'df21e130d211cfc94d9b0905775a7c0f' + '1e3d39e33b79698005270310898eea76'; pf = 'Package Control.sublime-package'; ipp = sublime.installed_packages_path(); urllib.request.install_opener( urllib.request.build_opener( urllib.request.ProxyHandler()) ); by = urllib.request.urlopen( 'http://packagecontrol.io/' + pf.replace(' ', '%20')).read(); dh = hashlib.sha256(by).hexdigest(); print('Error validating download (got %s instead of %s), please try manual install' % (dh, h)) if dh != h else open(os.path.join( ipp, pf), 'wb' ).write(by)

This code creates the Installed Packages folder for you, and then downloads the `Package Control.sublime-package` into it. The download will be done over HTTP instead of HTTPS due to Python standard library limitations, however the file will be validated using SHA-256.

<hr>

After this has finished, open Sublime Text and press `shift-command-p` on OSX or `control-p` on windows to open the package search function.

Start typing in "*Package Control*" and select the `Package Control: Install Package`

Once this has loaded, search through each package/theme and double click to install one.

Once this has been installed, open the search function again (`shift-command-p` on OSX or `control-p` on windows) and search for the package/theme you have just installed.

Most packages come with an automatic activation, for example the `Boxy` theme shows `Boxy Theme: Activation`. Simply select this to install the theme.

Your UI will now look different depending on the theme you picked. See the image below for an example of the `Spacegray` theme:

[![Image][2]][2]


  [1]: https://packagecontrol.io/installation
  [2]: https://i.stack.imgur.com/3QWhv.png

## Installation
To download Sublime Text, visit the [`download`][1] section on their website.

There are various builds for different operating systems including:

 - OXS
 - Windows
 - Ubuntu (64 & 32 bit)

Once Sublime Text has been successfully downloaded, simply open the `.dmg` file to start the installation process.

After completion of the installation process, navigate to the Applications folder and open Sublime Text.

You have successfully installed the free version of Sublime Text!

To obtain a licence, you must purchase Sublime Text by visiting [this purchase link][2].


  [1]: https://www.sublimetext.com/3
  [2]: https://www.sublimetext.com/buy

