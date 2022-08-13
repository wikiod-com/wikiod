---
title: "Ionic CLI hooks"
slug: "ionic-cli-hooks"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

# Introduction #

Hooks are pieces of code that Cordova CLI executes at certain points in your Cordova/Ionic application build. Hooks can be used for example to manipulate files in our project, automatically add plugins into your application or as in the example above check for code errors in your files.

**Note**: It is highly recommended writing your hooks using Node.js so that they are cross-platform but you can write them also for example in [Javascript][1].

# Hook types #

The following hook types are supported and execution order is quite self-explanatory according to the name.

    after_build
    after_compile
    after_docs
    after_emulate
    after_platform_add
    after_platform_rm
    after_platform_ls
    after_plugin_add
    after_plugin_ls
    after_plugin_rm
    after_plugin_search
    after_prepare
    after_run
    after_serve
    before_build
    before_compile
    before_docs
    before_emulate
    before_platform_add
    before_platform_rm
    before_platform_ls
    before_plugin_add
    before_plugin_ls
    before_plugin_rm
    before_plugin_search
    before_prepare
    before_run
    before_serve
    pre_package/ <-- Applicable to Windows 8 and Windows Phone only. This hook is deprecated.

# Ways to define hooks: #

Hooks could be defined in project's `config.xml` using `<hook>` elements, for example:

    <hook type="after_build" src="scripts/appAfterBuild.js" />

As a plugin developer you can define hook scripts using `<hook>` elements in a `plugin.xml` like this:

    <hook type="after_build" src="scripts/afterBuild.js" />

`before_plugin_install`, `after_plugin_install`, `before_plugin_uninstall` plugin hooks will be fired exclusively for the plugin being installed/uninstalled.

**Note**: Placing hooks in the `root/hooks` directory is considered deprecated in favor of the hook elements in `config.xml` and `plugin.xml`. If you however use this approach remember to set execute rights to the files in the `root/hooks` folder.

Documentation for Cordova Hooks can be found [here][2].


  [1]: https://cordova.apache.org/docs/en/latest/guide/appdev/hooks/#javascript
  [2]: https://cordova.apache.org/docs/en/latest/guide/appdev/hooks/

## Checking for errors in your Javascript files in before_prepare using jshint
    #!/usr/bin/env node
    
    var fs = require('fs');
    var path = require('path');
    var jshint = require('jshint').JSHINT;
    var async = require('async');
    
    var foldersToProcess = [
      'js'
    ];
    
    foldersToProcess.forEach(function(folder) {
      processFiles("www/" + folder);
    });
    
    function processFiles(dir, callback) {
      var errorCount = 0;
      fs.readdir(dir, function(err, list) {
        if (err) {
          console.log('processFiles err: ' + err);
          return;
        }
        async.eachSeries(list, function(file, innercallback) {
          file = dir + '/' + file;
          fs.stat(file, function(err, stat) {
            if (!stat.isDirectory()) {
              if (path.extname(file) === ".js") {
                lintFile(file, function(hasError) {
                  if (hasError) {
                    errorCount++;
                  }
                  innercallback();
                });
              } else {
                innercallback();
              }
            } else {
              processFiles(file);
            }
          });
        }, function(error) {
          if (errorCount > 0) {
            process.exit(1);
          }
        });
      });
    }
    
    function lintFile(file, callback) {
      console.log("Linting " + file);
      fs.readFile(file, function(err, data) {
        if (err) {
          console.log('Error: ' + err);
          return;
        }
        if (jshint(data.toString())) {
          console.log('File ' + file + ' has no errors.');
          console.log('-----------------------------------------');
          callback(false);
        } else {
          console.error('Errors in file ' + file);
          var out = jshint.data(),
              errors = out.errors;
          for (var j = 0; j < errors.length; j++) {
            console.error(errors[j].line + ':' + errors[j].character + ' -> ' + errors[j].reason + ' -> ' + errors[j].evidence);
          }
          console.error('-----------------------------------------');
          setTimeout(function() {
            callback(true);
          }, 10);
        }
      });
    }

