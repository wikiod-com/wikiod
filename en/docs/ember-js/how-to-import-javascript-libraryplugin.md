---
title: "How to import JavaScript libraryplugin"
slug: "how-to-import-javascript-libraryplugin"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Open the directory of your ember.js project, You will find there a file named ember-cli-build.js. You can install Your libraries or plugins using bower, then point the import to the bower_components folder, but if you have a file You want to add, just drag them to the folder of Your project and write the app.import to that file.

## Syntax
 - app.import('path to file starting from project folder/file.js');

## Example ember-cli-build.js file
    var EmberApp = require('ember-cli/lib/broccoli/ember-app');
    
    module.exports = function(defaults) {
      var app = new EmberApp(defaults, {
        // Add options here
        datatables: {
            core: true,
            style: 'bs',
            extensions: [
                { name: 'buttons', extensions: ['colVis','flash','html5','print'] },
                { name: 'responsive', style: 'bs' },
                'select'
            ],
            pdfmake: false,
            vfs_fonts: false,
            jszip: true
        }
      });
      //Imports:
      app.import('bower_components/js-cookie/src/js.cookie.js');
      app.import('bower_components/moment/min/moment.min.js');
      app.import('bower_components/crypto-js/crypto-js.js');
      // Use `app.import` to add additional libraries to the generated
      // output files.
      //
      // If you need to use different assets in different
      // environments, specify an object as the first parameter. That
      // object's keys should be the environment name and the values
      // should be the asset to use in that environment.
      //
      // If the library that you are including contains AMD or ES6
      // modules that you would like to import into your application
      // please specify an object with the list of modules as keys
      // along with the exports of each module as its value.
    
      return app.toTree();
    };

