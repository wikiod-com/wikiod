---
title: "Yarn Package Manager"
slug: "yarn-package-manager"
draft: false
images: []
weight: 9864
type: docs
toc: true
---

[Yarn](https://yarnpkg.com) is a package manager for Node.js, similar to npm. While sharing a lot of common ground, there are some key differences between Yarn and npm.

## Creating a basic package
The `yarn init` command will walk you through the creation of a `package.json` file to configure some information about your package. This is similar to the `npm init` command in npm.

Create and navigate to a new directory to hold your package, and then run `yarn init`

    mkdir my-package && cd my-package
    yarn init

Answer the questions that follow in the CLI

    question name (my-package): my-package
    question version (1.0.0): 
    question description: A test package
    question entry point (index.js): 
    question repository url: 
    question author: StackOverflow Documentation
    question license (MIT): 
    success Saved package.json
    ✨  Done in 27.31s.

This will generate a `package.json` file similar to the following

    {
      "name": "my-package",
      "version": "1.0.0",
      "description": "A test package",
      "main": "index.js",
      "author": "StackOverflow Documentation",
      "license": "MIT"
    }

Now lets try adding a dependency. The basic syntax for this is `yarn add [package-name]`

Run the following to install ExpressJS

`yarn add express`

This will add a `dependencies` section to your `package.json`, and add ExpressJS

    "dependencies": {
        "express": "^4.15.2"
    }

## Yarn Installation
This example explains the different methods to install Yarn for your OS.

# macOS #

## Homebrew ##

    brew update
    brew install yarn

## MacPorts ##

    sudo port install yarn

## Adding Yarn to your PATH ##

Add the following to your preferred shell profile (`.profile`, `.bashrc`, `.zshrc` etc)

    export PATH="$PATH:`yarn global bin`"

# Windows #

## Installer ## 

First, install Node.js if it is not already installed.

Download the Yarn installer as an `.msi` from the [Yarn website](https://yarnpkg.com/en/docs/install).

## Chocolatey ##

    choco install yarn

# Linux #

## Debian / Ubuntu ##

Ensure Node.js is installed for your distro, or run the following

    curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -
    sudo apt-get install -y nodejs

Configure the YarnPkg repository

    curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
    echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list

Install Yarn

    sudo apt-get update && sudo apt-get install yarn

## CentOS / Fedora / RHEL ##

Install Node.js if not already installed

    curl --silent --location https://rpm.nodesource.com/setup_6.x | bash -

Install Yarn

    sudo wget https://dl.yarnpkg.com/rpm/yarn.repo -O /etc/yum.repos.d/yarn.repo
    sudo yum install yarn

## Arch ##

Install Yarn via AUR.

Example using yaourt:

    yaourt -S yarn

## Solus ##

    sudo eopkg install yarn

## All Distributions ##

Add the following to your preferred shell profile (`.profile`, `.bashrc`, `.zshrc` etc)

    export PATH="$PATH:`yarn global bin`"

# Alternative Method of Installation #

## Shell script ##

    curl -o- -L https://yarnpkg.com/install.sh | bash

or specify a version to install

    curl -o- -L https://yarnpkg.com/install.sh | bash -s -- --version [version]


## Tarball ##

    cd /opt
    wget https://yarnpkg.com/latest.tar.gz
    tar zvxf latest.tar.gz

## Npm ##

If you already have npm installed, simply run

    npm install -g yarn

# Post Install #

Check the installed version of Yarn by running

    yarn --version

## Install package with Yarn
Yarn uses the same registry that npm does. That means that every package that is a available on npm is the same on Yarn.

To install a package, run `yarn add package`.

If you need a specific version of the package, you can use `yarn add package@version`.

If the version you need to install has been tagged, you can use `yarn add package@tag`.

