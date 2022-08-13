---
title: "Getting started with mocha"
slug: "getting-started-with-mocha"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Mocha example for string split method
    var assert = require('assert');

    describe('String', function() {
        describe('#split', function() {
          it('should return an array', function() {
            assert(Array.isArray('a,b,c'.split(',')))
          });
        });
    });

## Installation or Setup
You can install `mocha` either globally or in your project folder. The latter is the preferred way.
In all the example let's assume that all the test files are in a `test` folder within the project folder.

## Install Mocha locally

To install `mocha` in your project folder, you can use the following `npm` command:

    $ cd my-project/folder
    $ npm install mocha --save-dev

This command will install `mocha` inside the `node_modules` folder in your project and add a (development) dependency entry inside the `package.json` file.

### Use mocha in the CLI

To use `mocha` from the CLI you can either use the `mocha` command inside the `./node_modules/.bin/` folder:

    $ ./node_modules/.bin/mocha ./test

Or use a `npm script` (a `npm script` uses by default the commands in the `.bin` folder).

    # package.json
    {
     "name": "my-project",
     "version": "0.0.1",
     "description": "my first tested project",
     "scripts": {
        "start": "node app.js",
        "test": "mocha ./test"
     },
     ...
    }

To call that script you can do now:

    $ npm run test

Or simply (`test` is a special script in `npm`):
   
    $ npm test

### Use mocha in a webpage

To use `mocha` inside a webpage (just front-end), just include the `mocha.js` file inside `node_modules/mocha/mocha.js` inside your webpage:

    ## HTML page with tests
    <script src="node_modules/mocha/mocha.js"></script>

## Install mocha globally

For a global install use `npm` as follow:

    $ npm install mocha -g

This will install `mocha` in your global environment and bind the `mocha` command to your CLI., so you can call `mocha` from the terminal in any place.

    $ mocha ./test

