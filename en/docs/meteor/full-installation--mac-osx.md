---
title: "Full Installation - Mac OSX"
slug: "full-installation---mac-osx"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Install Node & NPM
This quickstart is written for Mac OSX Mavericks, and is a bit more verbose than other installation instructions. It should hopefully cover a few edge cases, such as setting your path, and configuring NPM, which can cause an installation to go awry.

```
    # install node
    # as of OSX Mavericks, we need the GUI installer (?!)
    # when a good command line alternative is found, we'll post it
    http://nodejs.org/download/
    
    # install npm
    curl -0 -L https://npmjs.org/install.sh | sh
    
    # check node is installed correctly
    node --version
    
    # check npm is installed correctly
    npm -version
    
    # find your npm path
    which npm
    
    # make sure npm is in your path
    sudo nano ~/.profile
      export PATH=$PATH:/usr/local/bin
```

##  Meteor Installation Walkthrough
This quickstart is written for Mac OSX Mavericks, and is a bit more verbose than other installation instructions. It should hopefully cover a few edge cases, such as setting your path, and configuring NPM, which can cause an installation to go awry.

```
# install meteor
    curl https://install.meteor.com | sh
    
    # check it's installed correctly
    meteor --version
    
    # install node
    # as of OSX Mavericks, we need the GUI installer (?!)
    # when a good command line alternative is found, we'll post it
    http://nodejs.org/download/
    
    # install npm
    curl -0 -L https://npmjs.org/install.sh | sh
    
    # check node is installed correctly
    node --version
    
    # check npm is installed correctly
    npm -version
    
    # find your npm path
    which npm
    
    # make sure npm is in your path
    sudo nano ~/.profile
      export PATH=$PATH:/usr/local/bin
```

## Mongo Installation
Meteor doesn't exist in isolation, and it's common to install a number of extra tools for development, such as Mongo, Robomongo, Atom, Linters, etc.

```
# make sure mongo is in your local path
nano ~/.profile
  export PATH=$PATH:/usr/local/mongodb/bin

# or install it to the global path
nano /etc/paths
  /usr/local/mongodb/bin

# create mongo database directory
mkdir /data/
mkdir /data/db
chown -R username:admin /data

# run mongodb server
mongod
ctrl-c

# check that you can connect to your meteor app with stand-alone mongo
terminal-a$ meteor create helloworld
terminal-a$ cd helloworld
terminal-a$ meteor

terminal-b$ mongo -port 3001

# install robomongo database admin tool 
http://robomongo.org/

# check you can connect to your mongo instance with robomongo
terminal-a$ meteor create helloworld
terminal-a$ cd helloworld
terminal-a$ meteor

Dock$ Robomongo > Create > localhost:3001
```

## Other Development Tools

```
# install node-inspector
terminal-a$  npm install -g node-inspector

# start meteor
terminal-a$  cd helloworld
terminal-a$  NODE_OPTIONS='--debug-brk --debug' mrt run

# alternatively, some people report this syntax being better
terminal-a$  sudo NODE_OPTIONS='--debug' ROOT_URL=http://helloworld.com meteor --port 80

# launch node-inspector along side your running app
terminal-b$  node-inspector

# go to the URL given by node-inspector and check it's running
http://localhost:8080/debug?port=5858

# install jshint
npm install -g jshint 
```

