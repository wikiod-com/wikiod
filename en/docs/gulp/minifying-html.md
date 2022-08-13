---
title: "Minifying HTML"
slug: "minifying-html"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Minify HTML using gulp-htmlmin
First, Install `gulp` and `gulp-htmlmin` to project directory locally

    npm install --save-dev gulp gulp-htmlmin

Then add the `minify-html` task to your `gulpfile.js`

    var gulp = require('gulp');
    var htmlmin = require('gulp-htmlmin');
    
    // Task to minify HTML
    gulp.task('minify-html', function() {
    return gulp.src('source/*.html')
    .pipe(htmlmin())
    .pipe(gulp.dest('public/'));
    });
    
    gulp.task('watch', function (){
        gulp.watch('source/*.html', ['minify-html']);
    // other tasks
    });
    
    gulp.task('default', ['minify-html', 'watch']);


This task finds all files in the `source` directory with a `.html` extension, minifies them and then outputs the resulting files to the `public` directory.

The task in the code is added as a dependency for the `'default'` task so every time `default` will run, `minify-html` will run before it.

You  can also call the `minify-html` task manually by running the command:

    gulp minify-html

