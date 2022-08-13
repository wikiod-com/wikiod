---
title: "Getting started with npm"
slug: "getting-started-with-npm"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
## Install

`npm` is bundled with [Node.js](https://nodejs.org), so if you install Node.js you'll automatically have `npm` installed too. You can choose between a **Current** and a **LTS** version

### Windows
For Microsoft Windows you can download a **MSI installer** from https://nodejs.org/en/download/.

### OS X
For Apple OS X you can download a **PKG installer** from the same location https://nodejs.org/en/download/.

### Linux
For Linux you can use your package manager to install Node.js and npm.

You can also compile Node.js from source and you'll still get `npm`. There is also a script you can run which will install `npm`:

    curl -L https://www.npmjs.com/install.sh | sh

## Upgrade npm to the latest version

The recommended method of updating your `npm` installation is to simply have `npm` install itself:

    npm install -g npm@latest

You can alternatively upgrade to the current LTS version rather than the latest version:

    npm install -g npm@lts

You can also install any version of Node (and npm) with [`nvm`](https://www.wikiod.com/node-js/installing-nodejs#Using Node Version Manager (nvm)). When installing globally with `npm` with an `nvm` installation, you do not need to use `sudo` (or Run as Administrator on Windows).


## Installing Global Packages
Install a global package

Globally installed packages drops modules in `{prefix}/lib/node_modules`, and puts executable files in `{prefix}/bin`, where `{prefix}` is usually something like `/usr/local`. Installing a global module means that its binaries end up in your `PATH` environment variable. Usually you'll want to install a global module if it's a command line tool, or something that you want to use in your shell.


    npm install --global package-name

Remove a global package

    npm uninstall --global package-name

Note: the `--global` argument can be simplified to `-g`. So, for instance, the first command could have been `npm install -g package-name`, with the exact same outcome.

Note: in *nix systems, installing global packages may require super-user permissions. Failing to do so will fail with: `EACCES`. In that case, run:

    sudo npm install --global package-name

## Install packages
Notice that packages can be installedThis command installs the newest available version of the named packages:

 both locally or globally.

Local installation means that **npm** installs your package in the current working directory. Node modules go in `./node_modules`, executables go in `./node_modules/.bin/`. Usually you'll want to install local modules for usage inside your program, as a dependency, and they will work only on where they're installed.

    npm install <package names> 

Shorthand:

    npm i <package names>

`npm` can interact with a `package.json` file in the current directory in various useful ways, through the objects `dependencies` and `devDependencies` stored in `package.json` (installing multiple modules):

The `npm install` command with no parameters

    npm install 

installs all packages named as object keys in the `dependencies` and `devDependencies` objects in `package.json`, using semantic versioning restrictions as indicated by the object values.

When developing new software:

Use option `-S`  to append the `<package names>` and versions of npm modules you are installing that should always be included with your module.  Appends to the list of `dependencies`  tracked in the `package.json` file,  after installing.

    npm i <package names> -S

Use option `-D`  to append the `<package names>` and versions of npm modules you are installing that are needed by other developers to further develop or test your module.  Appends to the list of `devDependencies`  tracked in the `package.json` file,  after installing.

    npm i <package names> -D



Where `lodash` and `mocha` are package names.

## Updating packages
In every applications life-cycle comes a day where it's components needs to be updated. Everyone knows the pain of updating every single dependency one-by-one. Well here you just need to issue the command:

    npm update (-g)
If the "-g" is there then npm will update the global packages.

## Using npm to manage dependencies
So you want to deploy your app to multiple sites? and your project has too many dependencies to install them one-by-one? Npm has a solution just issue the following command:

    npm init

In the project's root folder then follow the instructions on screen (type in the desired value then press enter) and then if you want to save a dependency then add:

    --save

after your

    npm install

commands for example:

    npm install mypackagename --save

And then that dependency will be saved then you don't have to move the "node_modules" folder. In order to install all saved dependency issue:

    npm install

and all saved dependencies will be installed.

