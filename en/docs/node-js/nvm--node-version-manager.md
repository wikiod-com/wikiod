---
title: "nvm - Node Version Manager"
slug: "nvm---node-version-manager"
draft: false
images: []
weight: 9850
type: docs
toc: true
---

The urls used in the above examples reference a specific version of Node Version Manager. It is most likely that the latest version is different to what's being referenced. To install nvm using the latest version, [click here][1] to access nvm on GitHub, which will provide you with latest urls.


  [1]: https://github.com/creationix/nvm

## Install NVM


## Check NVM version


## Installing an specific Node version
Listing available remote versions for installation

    nvm ls-remote

Installing a remote version

    nvm install <version>

For example

    nvm install 0.10.13




## Using an already installed node version
To list available local versions of node through NVM:

    nvm ls

For example, if `nvm ls` returns:

    $ nvm ls
         v4.3.0
         v5.5.0

You can switch to `v5.5.0` with:
    
    nvm use v5.5.0

## Install nvm on Mac OSX
INSTALLATION PROCESS
------------------------

You can install Node Version Manager using git, curl or wget. You run these commands in **Terminal** on **Mac OSX**.

**curl example:**

    curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.31.3/install.sh | bash

**wget example:**

    wget -qO- https://raw.githubusercontent.com/creationix/nvm/v0.31.3/install.sh | bash

TEST THAT NVM WAS PROPERLY INSTALLED
------------------------------------

 
To test that nvm was properly installed, close and re-open Terminal and enter `nvm`. If you get a **nvm: command not found** message, your OS may not have the necessary **.bash_profile** file. In Terminal, enter `touch ~/.bash_profile` and run the above install script again.

If you still get **nvm: command not found**, try the following:

 - In Terminal, enter `nano .bashrc`. You should see an export script almost identical to the following:

> export NVM_DIR=”/Users/johndoe/.nvm” [ -s “$NVM_DIR/nvm.sh” ] && .
> “$NVM_DIR/nvm.sh”

 - Copy the export script and remove it from **.bashrc**
 - Save and Close the .bashrc file (CTRL+O – Enter – CTRL+X)
 - Next, enter `nano .bash_profile` to open the Bash Profile
 - Paste the export script you copied into the Bash Profile on a new line
 - Save and Close the Bash Profile (CTRL+O – Enter – CTRL+X)
 - Finally enter `nano .bashrc` to re-open the **.bashrc** file
 - Paste the following line into the file:

> source ~/.nvm/nvm.sh

 - Save and Close (CTRL+O – Enter – CTRL+X)
 - Restart Terminal and enter `nvm` to test if it's working

## Setting alias for node version
If you want to set some alias name to installed node version, do:

    nvm alias <name> <version>

Similary to unalias, do:

    nvm unalias <name>

A proper usecase would be, if you want to set some other version than stable version as default alias. `default` aliased versions are loaded on console by default.

Like:

    nvm alias default 5.0.1

Then every time __console/terminal__ starts 5.0.1 would be present by default.

Note:
    
    nvm alias # lists all aliases created on nvm


## Run any arbitrary command in a subshell with the desired version of node
List all the node versions installed

    nvm ls
        v4.5.0
        v6.7.0
Run command using any node installed version

    nvm run 4.5.0 --version or nvm exec 4.5.0 node --version
    Running node v4.5.0 (npm v2.15.9)
    v4.5.0
<hr>

    nvm run 6.7.0 --version or nvm exec 6.7.0 node --version
    Running node v6.7.0 (npm v3.10.3)
    v6.7.0
<hr>
using alias

    nvm run default --version or nvm exec default node --version
    Running node v6.7.0 (npm v3.10.3)
    v6.7.0
To install node LTS version

    nvm install --lts
Version Switching 

    nvm use v4.5.0 or nvm use stable ( alias )

