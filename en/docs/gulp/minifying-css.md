---
title: "Minifying CSS"
slug: "minifying-css"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Using gulp-clean-css and gulp-rename
First, Install `gulp`, [`gulp-clean-css`](https://www.npmjs.com/package/gulp-clean-css) and [`gulp-rename`](https://www.npmjs.com/package/gulp-rename) to project directory localy

    npm install --save-dev gulp gulp-clean-css gulp-rename

Than add following `minify-css` task to your `gulpfile.js`

    var gulp = require('gulp');
    var cleanCSS = require('gulp-clean-css');
    var rename = require('gulp-rename');

    gulp.task('minify-css', function() {
        return gulp.src('css/dist/dist.css')
            .pipe(cleanCSS())
            .pipe(rename('dist.min.css'))
            .pipe(gulp.dest('css/dist'));
    });

    gulp.task('watch', function(){
      gulp.watch('css/dist/**/*.css', ['minify-css']); 
      // Other watchers
    });

    gulp.task('default', ['minify-css', 'watch']);

Here `.pipe(cleanCSS())` executes minification of your `css/dist/dist.css` file and `.pipe(rename('concat.min.css'))` renames it to `dist.min.css`


## Sass and Css - Preprocessing with Gulp
Before starting gulp we need to install `node.js` and `npm`.
Then install gulp-sacc
    
    $ npm i gulp-sass --save-dev // i = install

Gulp Head 

    var gulp = require('gulp');
    // Requires the gulp-sass plugin
    var sass = require('gulp-sass');

Gulp Body

    gulp.task('sass', function(){
      return gulp.src('app/scss/*.scss') // searching for sass files
        .pipe(sass()) // Converts Sass to CSS with gulp-sass
        .pipe(gulp.dest('app/css')) // destination of the css file
    });

Gulp watch

    gulp.task('watch', function(){
      gulp.watch('app/scss/**/*.scss', ['sass']); 
      // Other watchers
    })

