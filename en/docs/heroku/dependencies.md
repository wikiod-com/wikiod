---
title: "Dependencies"
slug: "dependencies"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Syntax
-  "dependencies": {
    ...
  }

## Bower dependancy
To automatically install bower and its components, one must

1. Specify the bower dependency in `package.json`:

       "dependencies": {
           "bower": "^1.7.9"
       }

2. Use `scripts` to execute a `postinstall` command

       "scripts": {
           "postinstall": "./node_modules/bower/bin/bower install"
       }

3. Create a `.bowerrc` file to set the directory for bower_components to install. Otherwise bower_components are installed in root directory.
        
       {
           "directory" : "app/bower_components"
       }


Now, Heroku automatically executes `bower install` command after `npm install`

