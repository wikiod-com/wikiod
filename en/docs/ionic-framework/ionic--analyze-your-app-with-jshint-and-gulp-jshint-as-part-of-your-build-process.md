---
title: "Ionic - Analyze your app with jshint and gulp-jshint as part of your build process"
slug: "ionic---analyze-your-app-with-jshint-and-gulp-jshint-as-part-of-your-build-process"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Linting your ionic app before running has huge advantages. It will analyse code for potential errors and save you tremendous amount of time. 

What is linting and how to install the required packages?
----------------

"Linting is the process of running a program that will analyse code for potential errors." - see What is "Linting"?

Your ionic app comes with a package.json file. Go to the root of you app in a Command Line/Terminal and install the following packages:

    npm install jshint --save-dev
    npm install jshint-stylish --save-dev
    npm install gulp-jshint --save-dev
 

## Add a gulp task
In the root of your ionic app, there is a gulpfile.js file. Open it in an editor and paste the following gulp task:

    gulp.task('lint', function() {
        return gulp.src(['./www/js/**/*.js']) 
            .pipe(jshint('.jshintrc'))
            .pipe(jshint.reporter('jshint-stylish')) 
            .pipe(jshint.reporter('fail'))
    });

This looks for a folder called 'js' inside the 'www' folder. If you have other folders containing JavaScript files, add those too. For example, lets also add a folder called 'views':
    
    gulp.task('lint', function() {
        return gulp.src(['./www/js/**/*.js','./www/views/**/*.js'])
            .pipe(jshint('.jshintrc'))
            .pipe(jshint.reporter('jshint-stylish')) 
            .pipe(jshint.reporter('fail'))
    });

Explanations: 

    1) /**/*.js - This syntax means to look at all the js files in the subfolders too
    2) .jshintrc - This is a configuration file that we will create in the next example. 

## Create .jshintrc file (Optional)
Create a file named '.jshintrc' in the root of your app, where package.json is.

*Note on windows: create a file named "jshintrc.txt". Then rename it to ".jshintrc." (notice the dot at the end).

This is a configuration file. It can for example tell jshint to ignore certain variables and many other things. Here is mine:

    {
        "predef": [ 
            "window",
            "console",
            "cordova",
            "device",
            "alert",
            "document",
            "debug",
            "setServiceVars",
            "StatusBar",
            "config"
        ],
        "globals": {
            "angular"        : false,
            "myApp"            : false,
            "myControllers"    : false,
            "myDirectives"    : false,
            "localStorage"     : false,
            "navigator"         : false, 
            "emit"             : false, 
            "atob"             : false,  
            "moment"        : false,
            "btoa"            : false
        },
        "node"          : true
    }

## Add Makefile
1) Create a file named: "Makefile" (with no extension) in the root of your app

2) Open it in a text editor and add this:


    android:
        gulp lint
        gulp sass
        ionic run android --device
    
    ios:
        gulp lint
        gulp sass
        ionic build ios

This will lint your app and if that passes, it will compile sass and build you app.

**Usage:** To run your app, instead of the regular "ionic run android --device", run these commands:

    Android: make android
    iOS    : make ios  

