---
title: "Installation"
slug: "installation"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

This covers Ruby only, which is the main SASS compiler for many systems but other options exist. A very common one for any node developer would be [node-sass][1] which could be easier, and orders of magnitude faster, for many users.


[1]: https://github.com/sass/node-sass

## Mac
Ruby comes pre-installed on a Mac computer.

Follow the instructions below to install Sass:

 1. Open CMD
 2. Run `gem install sass`
 3. If you get an error message, try `sudo gem install sass`
 4. Check it works using `sass -v`

## Linux
Ruby will need to be installed first before setup. You can install Ruby through the apt package manager, rbenv, or rvm.

Then Run

    sudo su -c "gem install sass"

## Windows
The fastest way to get Ruby on your Windows computer is to use [Ruby Installer][1]. It's a single-click installer that will get everything set up for you super fast. After installing Ruby, follow the instructions below to install Sass:

 1. Open CMD
 2. Run `gem install sass`
 3. If you get an error message, try `sudo gem install sass`
 4. Check it works using `sass -v`


  [1]: http://rubyinstaller.org/

