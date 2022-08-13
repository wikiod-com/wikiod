---
title: "Getting started with gruntjs"
slug: "getting-started-with-gruntjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing Grunt
# Prerequesites

Grunt requires Node.js and npm to be installed. If you don’t have Node.js and/or npm installed on your machine, go to https://nodejs.org and download the installer or package for your operating system.

# First-time install

If you're installing Grunt for the first time, you'll first have to install the Grunt command-line interface package `grunt-cli` globally.

```bash
npm install -g grunt-cli
```

This installs the command-line interface for Grunt globally so you can run the local version of Grunt in your project.

You can verify that you have `grunt-cli` package installed by running the following command:

```
grunt --version
```

This should print at least the current version of your `grunt-cli` package.

# Installing Grunt in your project

After you have `grunt-cli` up and running, you can install the actual `grunt` task runner and your first Grunt package `grunt-contrib-jshint`:

```bash
npm install grunt --save-dev
npm install grunt-contrib-jshint --save-dev
```

This downloads the packages from NPM package manager and saves them as `devDependencies` to your `package.json` file.

## Gruntfile

Next you need a `Gruntfile.js` in your project root that acts as a config file for Grunt tasks:

```javscript
module.exports = function(grunt) {

    grunt.initConfig({
        jshint: {
            files: ['Gruntfile.js'],
        }
    });

    grunt.loadNpmTasks('grunt-contrib-jshint');


    grunt.registerTask('default', ['jshint']);
};
```

This file does three things:

1. It tells Grunt to load the `grunt-contrib-jshint` task from NPM package
2. It advices the `jshint` task to run against the file `Gruntfile.js`
3. It creates a Grunt task named `default` that runs the `jshint` task

## Running Grunt

After you have set up your project you can run the `default` task of Grunt by calling:

```bash
grunt
```

This fires up `grunt-cli` that runs the local `grunt` which looks for a Grunt task named `default` which is configured to run the task called `jshint`.

## Running tasks
# Tasks in configuration

All attributes of `grunt.initConfig` are valid tasks, so if your Gruntfile looks like this:

    module.exports = function(grunt) {
    
        grunt.initConfig({
            jshint: {
                files: ['Gruntfile.js'],
            }
        });
    
        grunt.loadNpmTasks('grunt-contrib-jshint');
    
    
        grunt.registerTask('default', ['jshint']);
    };

The shell command `$ grunt jshint` will run the `jshint` task.

# Tasks with targets

Tasks can have different targets. Take this snippet of code for example:

        grunt.initConfig({
            jshint: {
                gruntfile: {
                    files: ['Gruntfile.js']
                },
                project: {
                    files: 'src/**/*.js'
                }
            }
        });

Here, jshint can target the gruntfile or all JavaScript files of your project. If we run `$ grunt jshint` both targets will be used, but if we run `$ grunt jshint:gruntfile` the linter will only be applied to the gruntfile.

# Registered tasks

The default tasks registered like this `grunt.registerTask('default', ['jshint']);` will run with the shell command `$ grunt`.

New registered tasks will run passing its name as a command line argument to grunt. For example:

    grunt.registerTask('gruntfile', ['jshint:gruntfile']);

Will runt with `$ grunt gruntfile`.

## Help
Run grunt -h to see the following:

* Command line parameters
* All available standalone tasks in the Gruntfile of the current directory
* All subtasks of each suite of tasks in the Gruntfile of the current directory 

