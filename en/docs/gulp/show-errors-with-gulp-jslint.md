---
title: "Show errors with gulp-jslint"
slug: "show-errors-with-gulp-jslint"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Installation and usage
**Installation**

    $ npm install gulp-jslint --save-dev

**Usage**

In gulpfile.js add:

    var gulp = require('gulp');
    var jslint = require('gulp-jslint');

    gulp.task('lint', function(){
        return gulp.src(['source.js'])
            .pipe(jslint({ /* this object represents the JSLint directives being passed down */ }))
            .pipe(jslint.reporter( 'my-reporter' ));
    });

for use this task:

    $ ./node_modules/gulp/bin/gulp.js lint




