---
title: "Image lossless compression (with gulp-imagemin)"
slug: "image-lossless-compression-with-gulp-imagemin"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
 1. imagemin([plugins], {options})

## Parameters
| Argument | Description |
| ------ | ------ |
| `sourcePath`   | Images' source directory (for example: `/assets/images`)   |
| `buildPath` | Destination path (for example: `/static/dist/`) |


First argument to `imagemin` constructor is plugin array. By default, following plugins are used: `[imagemin.gifsicle(), imagemin.jpegtran(), imagemin.optipng(), imagemin.svgo()]`

Second argument are options. In the above example following options are used:

    {
      progressive: true,
      interlaced: true,
      svgoPlugins: [{removeUnknownsAndDefaults: false}, {cleanupIDs: false}]
    }

Those are completely optional.

<a href="https://www.npmjs.com/package/imagemin-jpegtran#progressive">`progressive`</a> is used by `imagemin-jpegtran`. 

<a href="https://www.npmjs.com/package/imagemin-gifsicle#interlaced">`interlaced`</a> is used by `imagemin-gifsicle`. 

<a href="https://github.com/svg/svgo#what-it-can-do">`removeUnknownsAndDefaults`</a> and <a href="https://github.com/svg/svgo#what-it-can-do">`cleanupIDs`</a> are used by `imagemin-svgo`. 

## Installation and usage
**Dependency installation** ( https://www.npmjs.com/package/gulp-imagemin )

    $ npm install --save-dev gulp-imagemin

**Usage**

    /*
     * Your other dependencies.
     */

    var imagemin = require('gulp-imagemin');

    /*
     * `gulp images` - Run lossless compression on all the images.
     */
    gulp.task('images', function() {
      return gulp.src(sourcePath) // e.g. /assets/images
        .pipe(imagemin({
          progressive: true,
          interlaced: true,
          svgoPlugins: [{removeUnknownsAndDefaults: false}, {cleanupIDs: false}]
        }))
        .pipe(gulp.dest(buildPath + 'images')); // e.g. /static/dist/
    });


