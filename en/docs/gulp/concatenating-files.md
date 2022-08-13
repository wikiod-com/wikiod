---
title: "Concatenating files"
slug: "concatenating-files"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Concat and Uglify JS and CSS files
Remember to npm install all the files into devDependencies first. E.g.

    npm install --save-dev gulp gulp-concat gulp-rename gulp-uglify gulp-uglifycss

**Gulpfile.js**

    var gulp = require('gulp');
    var gulp_concat = require('gulp-concat');
    var gulp_rename = require('gulp-rename');
    var gulp_uglify = require('gulp-uglify');
    var uglifycss = require('gulp-uglifycss');

    var destDir = './public/assets/dist/'; //or any folder inside your public asset folder
    var tempDir = './public/assets/temp/'; //any place where you want to store the concatenated, but unuglified/beautified files
    //To concat and Uglify All JS files in a particular folder
    gulp.task('js-uglify', function(){
        return gulp.src(['./public/js/**/*.js','./public/assets/js/*.js']) //Use wildcards to select all files in a particular folder or be specific
            .pipe(gulp_concat('concat.js')) //this will concat all the files into concat.js
            .pipe(gulp.dest(tempDir)) //this will save concat.js in a temp directory defined above
            .pipe(gulp_rename('uglify.js')) //this will rename concat.js to uglify.js
            .pipe(gulp_uglify()) //this will uglify/minify uglify.js
            .pipe(gulp.dest(destDir)); //this will save uglify.js into destination Directory defined above
    });
    //To Concat and Uglify all CSS files in a particular folder
    gulp.task('css-uglify', function () {
      gulp.src('./public/assets/css/*.css') //Use wildcards to select all files in a particular folder or be specific
      .pipe(gulp_concat('concat.css')) //this will concat all the source files into concat.css
            .pipe(gulp.dest(tempDir)) //this will save concat.css into a temp Directory
            .pipe(gulp_rename('uglify.css')) //this will rename concat.css into uglify.css, but will not replace it yet.
        .pipe(uglifycss({
          "maxLineLen": 80,
          "uglyComments": true
        })) //uglify uglify.css file
        .pipe(gulp.dest(destDir)); //save uglify.css
    });


Run them by following commands

    gulp js-uglify
    gulp css-uglify



## Concat all css files into one using gulp-concat


