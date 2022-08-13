---
title: "Getting started with homebrew"
slug: "getting-started-with-homebrew"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Homebrew can be installed using the following command in the terminal:

    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

It runs as the current user; you don’t need to use `sudo`. The script [can be viewed online](https://raw.githubusercontent.com/Homebrew/install/master/install) and will print a summary of what it’ll do before actually doing it.

You then have to add `/usr/local/bin` to your `PATH` if it’s not already there. Run the following command to check:

    echo $PATH

If you don’t see `/usr/local/bin` in the output you’ll have to add it. Open your `~/.bash_profile` file in an editor and add the following lines at the end:

    # Add Homebrew prefix to the PATH
    export PATH="/usr/local/bin:$PATH"

Now run `source ~/.bash_profile`. The `brew` command should now be available:

    brew help

## Syntax
**Example Usage:**

    brew search [TEXT|/REGEX/]
    brew (info|home|options) [FORMULA...]
    brew install FORMULA...
    brew update
    brew upgrade [FORMULA...]
    brew uninstall FORMULA...
    brew list [FORMULA...]

**Troubleshooting:**

    brew config
    brew doctor
    brew install -vd FORMULA

**Developers:**
    brew create [URL [--no-fetch]]
    brew edit [FORMULA...]
  
**Further help:**
    man brew
    brew help [COMMAND]
    brew home

