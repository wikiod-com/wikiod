---
title: "Using Browserify"
slug: "using-browserify"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Parameters
| Options | Details |
| ------- | ------- |
| transform | Specifies a pipeline of functions (or module names) through which the browserified bundle will be run.   |
| debug | Enable source map support. `!gulp.env.production` would work well. |
| extensions | Array of extensions that you want to skip in `require()` calls in addition to .js and .json. Don't forget `.` |
| ignore | Array of paths which should be passed to the ignore function of browserify. |
| resolve | Custom module name resolution function. |
| nobuiltins | Remove builtins modules defined in `lib/builtins.js` (browserify module). `opts.builtins` must be not defined and `opts.nobuiltins` can be an Array of Strings or simply a String. |


## Using Browserify with Vanilla Javascript
First install gulp and browserify via `npm i gulp gulp-browserify`. This will install browserify into your `node_modules` folder.

gulpfile.js

    var gulp = require('gulp');
    var browserify = require('gulp-browserify');
    
    gulp.task('script', function() {
        gulp.src('./src/script.js')
            .pipe(browserify({
                insertGlobals: true
            })
            .pipe(gulp.dest('./build/'));
    }

## Using Browserify with Coffeescript
First install gulp and browserify via npm i gulp gulp-coffeeify. This will install browserify into your node_modules folder.

gulpfile.js

    var gulp = require('gulp');
    var coffeeify = require('gulp-coffeeify');
    
    gulp.task('script', function() {
        gulp.src('./src/script.coffee')
            .pipe(coffeeify())
            .pipe(gulp.dest('./build/'));
    }

