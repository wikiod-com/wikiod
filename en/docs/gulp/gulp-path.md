---
title: "Gulp Path"
slug: "gulp-path"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Creating Simple Gulp Path to the app
Before running gulp you need to install `Node.JS` and `npm`

The head of the Gulp.js is:

    var gulp = require('gulp');
        clean = require('gulp-clean'); // A gulp plugin for removing files and folders.
        imagemin = require('gulp-imagemin'); // Minify PNG, JPEG, GIF and SVG images

Then assign path of the gulp files

    var bases = {
     app: 'app/',        // path to your app
     dist: 'dist/',      // path to the compiled application
    };

    var paths = {
     scripts: ['scripts/**/*.js', '!scripts/libs/**/*.js'],
     libs: ['scripts/libs/jquery/dist/jquery.js', 'scripts/libs/underscore/underscore.js', 'scripts/backbone/backbone.js'],
     styles: ['styles/**/*.css'],
     html: ['index.html', '404.html'],
     images: ['images/**/*.png'],
     extras: ['crossdomain.xml', 'humans.txt', 'manifest.appcache', 'robots.txt', 'favicon.ico'],
    };

Example 1:

    // Copy all other files to dist directly
        gulp.task('copy', ['clean'], function() {
     // Copy html
           gulp.src(index.html)
             .pipe(gulp.dest(bases.dist)); // same as .pipe(gulp.dest('dist'));
        });

Example 2:

    // Imagemin images and ouput them in dist
    gulp.task('imagemin', ['clean'], function() {
     gulp.src(paths.images, {cwd: bases.app})
     .pipe(imagemin())
     .pipe(gulp.dest(bases.dist + 'images/')); // same as .pipe(gulp.dest('dist/images/'));
    });

Default watch

    // Define the default task as a sequence of the above tasks
    gulp.task('default', ['clean', 'imagemin', 'copy']);

