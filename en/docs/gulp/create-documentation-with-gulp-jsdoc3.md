---
title: "Create documentation with gulp-jsdoc3"
slug: "create-documentation-with-gulp-jsdoc3"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Installation
First of all, install `gulp` and [`gulp-jsdoc3`][1] to your project:

    npm install gulp-jsdoc3 --save-dev

Then add it to your gulpfile.js

    var gulp = require('gulp');
    var jsdoc = require('gulp-jsoc3');



    gulp.task('doc', function (cb){
        gulp.src('src/*.js')
        .pipe(jsdoc(cb));
    });

In order to documentate, for example, a function, you have to add a comment just at the top of the function, like this:

    /**
     * @function example
     * @summary This is a short description of example
     * @author Whoever
     * @param {any} cb
     * @returns 
     */
    function example(cb){
        //Code
    }

If you want to know more block tags to use, please visit [usejsdoc.org][2]


  [1]: https://www.npmjs.com/package/gulp-jsdoc3/
  [2]: http://usejsdoc.org/

