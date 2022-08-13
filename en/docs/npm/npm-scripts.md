---
title: "npm scripts"
slug: "npm-scripts"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
- The "scripts" property in `package.json` allows you to run npm packages locally. 
- The `"karma": "karma"` script references the `karma` shell script is the `node_modules/.bin` directory.  This reference needs to be grab and an alias needs to be applied to it in order to be used in other npm scripts, such as `"test": "karma start"`.


# [Pre-recognized scripts](https://docs.npmjs.com/misc/scripts) 
- `prepublish`: Run before the package is published
- `publish`, `postpublish`: Run after the package is published
- `preinstall`: Run before the package is installed
- `install`, `postinstall`: Run after the package is installed
- `preversion`, `version`: Run before bump the package version
- `postversion`: Run after bump the package version
- `pretest`, `test`, `posttest`: Run by the `npm test` command
- `prestop`, `stop`, `poststop`: Run by the `npm stop` command
- `prestart`, `start`, `poststart`: Run by the `npm start` command
- `prerestart`, `restart`, `postrestart`: Run by the `npm restart` command.  Note: `npm restart` will run the stop and start scripts if no `restart` script is provided.

It can be deduced that the `"scripts"` property in `package.json` is a very powerful tool. It can be used as a build tool, similar to the likes of Grunt and Gulp, but with over 250,000 packages available.  NPM scripts runs npm packages installed locally to your project from the `node_modules/.bin` directory.


## Running npm scripts
There are two types of npm scripts, and the command to run each is slightly different.  The first type of npm scripts are "pre-recognized" scripts.  These scripts are automatically recognized by npm and don't need a special prefix (as you will see for the other type) to run them.  The other type of scripts are "custom" scripts.  These scripts aren't pre-recognized by npm and have to be prefixed by a special command to run them.  There are a list of pre-recognized scripts in the remarks section.

To run pre-recognized scripts:

`npm start` or `npm test`

To run custom scripts you need to use the `run` command:

`npm run karma`

## What npm scripts are and how are they triggered
The npm scripts are commands that `npm` will run for you when called with the proper arguments. The power and sense of this is to NOT install the npm packages globally poluting your environment.

The difference between pre-recognized and custom scripts relies on the `run` word between the tags, **`custom` scripts will need the `run` between npm and the script name**

Based on this we can differenciate and create different tasks or scripts to be run with npm.

Given the following example on the `package.json` file:

    {
      "name": "MyApp",
      "version": "1.0.0",
      "description": "",
      "main": "app.js",
      "scripts": {
        "test": "mocha --recursive ./tests/",
        "test:watch": "npm run test -- -w",
        "start": "nodemon --inspect ./app.js",
        "build": "rm -rf ./dist/ && gulp build"
      }
    ...
    }

We can see different tasks to be run:

- `npm test` Would work fine as it is a pre-recognized script
- `npm run test` Would work fine as it is a valid way to execute a npm script
- `npm run test:watch` Would work also, and it's calling npm run test inside itself

- `npm run build` Would before running `gulp build` delete the `dist` folder that is in the directory (assuming you are in Linux or the command `rm` is recognized)




## Running karma locally
`package.json` snippet

    {
        "scripts":
            "test": "karma start",
            "karma": "karma"
        }
    }

