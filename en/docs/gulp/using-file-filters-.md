---
title: "Using file filters."
slug: "using-file-filters"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Creating Images, JS, and CSS(SASS) rule for ordering files
# Installing Gulp and His Tasks

    $ npm install gulp --save-dev
    $ npm install gulp-sass --save-dev
    $ npm install gulp-uglify --save-dev
    $ npm install gulp-imagemin --save-dev
 
# Determining Folder Structure
In this structure, we will use the app folder for development purposes, while the dist folder is used to contain optimized files for the production site.

    |-app
        |-css/
        |-images/
        |-index.html
        |-js/
        |-scss/
    |- dist/
    |- gulpfile.js
    |- node_modules/
    |- package.json

# Gulp Preprocessing

    // Requires the gulp-sass plugin
    var gulp = require('gulp'); 
        sass = require('gulp-sass');
        uglify = require('gulp-uglify');
        imagemin = require('gulp-imagemin');

# Gulp Task

    gulp.task('sass', function(){
      return gulp.src('app/scss/**/*.scss') //selection all files in this derectory
        .pipe(sass()) // Using gulp-sass
        .pipe(gulp.dest('dist'))
    });
    gulp.task('gulp-uglify', function(){
      return gulp.src('app/js/*.js')
        // Minifies only if it's a JavaScript file
        .pipe(uglify())
        .pipe(gulp.dest('dist'))
    });
    gulp.task('images', function(){
      return gulp.src('app/images/**/*.+(png|jpg|gif|svg)')
      .pipe(imagemin())
      .pipe(gulp.dest('dist/images'))
    });

# Gulp Watch

    gulp.task('watch', function(){
      gulp.watch('app/js/**/*.js', ['gulp-uglify']); 
        gulp.watch('app/scss/**/*.scss', ['sass']);
        gulp.watch('app/images/**/*.*', ['images']);  
      // Other watchers
    });
    gulp.task('build', [`sass`, `gulp-uglify`, `images`], function (){
      console.log('Building files');
    });
    gulp.task('default', function() {});

