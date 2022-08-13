---
title: "Installation"
slug: "installation"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Fedora Installation
    dnf install erlang elixir

## OSX Installation
On OS X and MacOS, Elixir can be installed via the common package managers:

# Homebrew 

    $ brew update
    $ brew install elixir 

# Macports
  

     $ sudo port install elixir

## Debian/Ubuntu Installation
```
# Fetch and install package to setup access to the official APT repository
wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb

# Update package index
sudo apt-get update

# Install Erlang and Elixir
sudo apt-get install esl-erlang
sudo apt-get install elixir
```

## Gentoo/Funtoo Installation
Elixir is available in main packages repository.<br/>
Update the packages list before installing any package:

    emerge --sync

This is one step installation:

    emerge --ask dev-lang/elixir

