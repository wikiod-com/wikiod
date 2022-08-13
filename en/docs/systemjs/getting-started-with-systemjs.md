---
title: "Getting started with systemjs"
slug: "getting-started-with-systemjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Using SystemJS to load moment.js
SystemJS allows to write and use modular javacsript code that relies on ECMAScript 6 [import][1] and [export][2] statements. One good example is moment.js library, which started publishing ECMAScript 6 source code on npm since 2.10.0 release of moment.js.

**Installing prerequisites**

    npm install moment
    npm install systemjs
    npm install traceur

Note: SystemJS requries a transpiler to compile ECMAScript 6 javacsript into a code that could be run in current versions of browsers and node.js, none of which currently supports ECMAScript 6 modules. Current version of SystemJS uses and requires traceur by default so we need to install it, but SystemJS can be configured to use either traceur, babel or typescript (with the help of some plugins).

**Adding example code**

create file `test.js`:

    import moment from 'moment';
    
    export function test() {
        const m1 = moment().format('LLL');
        const m2 = moment().fromNow();
        return `The moment is ${m1}, which was ${m2}`;
    }

This is very simple javascript module, which also shows that you can use other new ECMAScript 6 features besides `import` and `export`.

**Running it in node.js**

create file `main.js`


    var SystemJS = require('systemjs');
    
    SystemJS.config({
        map: {
            'traceur': 'node_modules/traceur/bin/traceur.js',
            'moment': 'node_modules/moment/src'
        },
        packages: {
            'moment': {
                main: 'moment.js'
            }
        }
    });
    
    SystemJS.import('./test.js')
        .then(function(test) {
            var t = test.test();
            console.log(t);
        })
        .catch(function(e) {
            console.error(e)
        });

This file uses `SystemJS.import` to load our `test.js` file and execute `test()` function from it, instead of normal `require`. SystemJS has to be configured using `SystemJS.config()` so it could find source code for `traceur` and `moment` modules. The path for `moment` in `map` points to `moment/src` directory where ECMAScript 6 version of moment.js source code resides.

You can run this file using 

    node main.js


**Running it in the browser**

create file `index.html`

    <html>
    <head>
        <title>SystemJS example with moment.js</title>
        <meta charset="UTF-8">
    
        <script src="node_modules/systemjs/dist/system.src.js"></script>

        <script>
            SystemJS.config({
                map: {
                    'traceur': 'node_modules/traceur/bin/traceur.js',
                    'moment': 'node_modules/moment/src'
                },
                packages: {
                    'moment': {
                        main: 'moment.js'
                    }
                }
            });
    
            SystemJS.import('./test.js')
                    .then(function(test) {
                        var t = test.test();
                        document.body.appendChild(
                            document.createTextNode(t)
                        );
                    })
                    .catch(function(e) {
                        console.error(e)
                    });
    
        </script>
    </head>
    <body>
    </body>
    </html>

The only differences from `node.js` code is that we are loading SystemJS using `<script>` tag instead of `require` and we are adding text to the HTML document using `appendChild` instead of showing it in the console.


  [1]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import
  [2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/export







## Installation or Setup
Detailed instructions on getting systemjs set up or installed.

