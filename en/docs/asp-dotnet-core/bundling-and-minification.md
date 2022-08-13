---
title: "Bundling and Minification"
slug: "bundling-and-minification"
draft: false
images: []
weight: 9811
type: docs
toc: true
---

## Bundler and Minifier Extension
Visual Studio also features an available [Bundler and Minifier Extension](https://visualstudiogallery.msdn.microsoft.com/9ec27da7-e24b-4d56-8064-fd7e88ac1c40) that is capable of handling this process for you. The extension allows you to easily select and bundle the files you need without writing a line of code.

**Building Your Bundles**
-------------------------

After installing the extension, you **select all of the specific files that you want to include within a bundle and use the Bundle and Minify Files option from the extension :**

[![Building Your Bundle][1]][1]

This will prompt to you name your bundle and choose a location to save it at. You'll then notice a new file within your project called `bundleconfig.json` which looks like the following :

    [
      {
        "outputFileName": "wwwroot/app/bundle.js",
        "inputFiles": [
          "wwwroot/lib/jquery/dist/jquery.js",
          "wwwroot/lib/bootstrap/dist/js/bootstrap.js",
          "wwwroot/lib/jquery-validation/dist/jquery.validate.js",
          "wwwroot/lib/jquery-validation-unobtrusive/jquery.validate.unobtrusive.js" 
        ]
      }
    ]

> **NOTE:** The order in which the files are selected will determine the
> order that they appear in within the bundle, so if you have any
> dependencies, ensure you take that into account.

**Minifying Your Bundles**
-------------------------

Now the previous step will simply bundle your files, if you want to minify the bundle, then you need to indicate that within the bundleconfig.json file. **Simply add a `minify` block like the following to your existing bundle to indicate you want it minified :**

    [
      {
        "outputFileName": "wwwroot/app/bundle.js",
        "inputFiles": [
          "wwwroot/lib/jquery/dist/jquery.js",
          "wwwroot/lib/bootstrap/dist/js/bootstrap.js",
          "wwwroot/lib/jquery-validation/dist/jquery.validate.js",
          "wwwroot/lib/jquery-validation-unobtrusive/jquery.validate.unobtrusive.js" 
        ],
        "minify": {
          "enabled": true
        }
      }
    ]

**Automate Your Bundles**
-------------------------

Finally, if you want to automate this process, you can schedule a task to run whenever your application is built to ensure that your bundles reflect any changes within your application.

To do this, you'll need to do the following :

 - **Open the Task Runner Explorer** (via Tools > Task Runner Explorer).
 - **Right-click on the Update All Files option** below `bundleconfig.json`.
 - **Select your preferred binding** from the Bindings context menu.

[![enter image description here][2]][2]

After doing this, your bundles should be automatically updated at the preferred step that you selected.



  [1]: http://i.stack.imgur.com/4wie9.gif
  [2]: http://i.stack.imgur.com/LYWkS.gif

## The dotnet bundle Command
The ASP.NET Core RTM release introduced `BundlerMinifier.Core`, a new Bundling and Minification tool that can be easily integrated into existing ASP.NET Core applications and doesn't require any external extensions or script files.

**Using BundlerMinifier.Core**
----------

To use this tool, **simply add a reference to `BundlerMinifier.Core` within the `tools` section of your existing `project.json` file** as seen below :

    "tools": {
      "BundlerMinifier.Core": "2.0.238",
      "Microsoft.AspNetCore.Razor.Tools": "1.0.0-preview2-final",
      "Microsoft.AspNetCore.Server.IISIntegration.Tools": "1.0.0-preview2-final"
    }

**Configuring Your Bundles**
----------

After adding the tool, you'll need to **add a `bundleconfig.json` file in your project** that will be used to configure the files that you wish to include within your bundles. A minimal configuration can be seen below :

    [
      {
        "outputFileName": "wwwroot/css/site.min.css",
        "inputFiles": [
          "wwwroot/css/site.css"
        ]
      },
      {
        "outputFileName": "wwwroot/js/site.min.js",
        "inputFiles": [
          "wwwroot/js/site.js"
        ],
        "minify": {
          "enabled": true,
          "renameLocals": true
        },
        "sourceMap": false
      },
      {
        "outputFileName": "wwwroot/js/semantic.validation.min.js",
        "inputFiles": [
          "wwwroot/js/semantic.validation.js"
        ],
        "minify": {
          "enabled": true,
          "renameLocals": true
        }
      }
    ]

**Creating / Updating Bundles**
----------

After your bundles have been configured, you can bundle and minify your existing files via the following command :

    dotnet bundle

**Automated Bundling**
----------

The Bundling and Minification process can be automated as part of the build process by adding the `dotnet bundle` command in the precompile section of your existing `project.json` file :

    "scripts": {
      "precompile": [
        "dotnet bundle"
      ]
    }

**Available Commands**
----------

You can see a list of all of the available commands and their descriptions below :

 - **dotnet bundle** - Executes the bundle command using the `bundleconfig.json` file to bundle and minify your specified files.
 - **dotnet bundle clean** - Clears all of the existing output files from disk.
 - **dotnet bundle watch** - Creates a watchers that will automatically run `dotnet bundle` whenever an existing input file from the `bundleconfig.json` configuration to bundle your files.
 - **dotnet bundle help** - Displays all available help options and instructions for using the command-line interface.


## Grunt and Gulp
<!-- language-all: lang-js -->

In ASP.NET Core apps, you bundle and minify the client-side resources during design-time using third party tools, such as [Gulp](http://gulpjs.com/) and [Grunt](http://gruntjs.com/). By using design-time bundling and minification, the minified files are created prior to the application’s deployment. Bundling and minifying before deployment provides the advantage of reduced server load. However, it’s important to recognize that design-time bundling and minification increases build complexity and only works with static files.

This is done in ASP.NET Core by configuring Gulp via a `gulpfile.js` file within your project :

    // Defining dependencies
    var gulp = require("gulp"),  
        rimraf = require("rimraf"),
        concat = require("gulp-concat"),
        cssmin = require("gulp-cssmin"),
        uglify = require("gulp-uglify");

    // Define web root
    var webroot = "./wwwroot/"

    // Defining paths
    var paths = {  
        js: webroot + "js/**/*.js",
        minJs: webroot + "js/**/*.min.js",
        css: webroot + "css/**/*.css",
        minCss: webroot + "css/**/*.min.css",
        concatJsDest: webroot + "js/site.min.js",
        concatCssDest: webroot + "css/site.min.css"
    };

    // Bundling (via concat()) and minifying (via uglify()) Javascript
    gulp.task("min:js", function () {  
        return gulp.src([paths.js, "!" + paths.minJs], { base: "." })
            .pipe(concat(paths.concatJsDest))
            .pipe(uglify())
            .pipe(gulp.dest("."));
    });

    // Bundling (via concat()) and minifying (via cssmin()) Javascript
    gulp.task("min:css", function () {  
        return gulp.src([paths.css, "!" + paths.minCss])
            .pipe(concat(paths.concatCssDest))
            .pipe(cssmin())
            .pipe(gulp.dest("."));
    });

This approach will properly bundle and minify your existing Javascript and CSS files respectively accordingly to the directories and globbing patterns that are used.



