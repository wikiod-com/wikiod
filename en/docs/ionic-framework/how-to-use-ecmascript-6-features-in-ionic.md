---
title: "How to use EcmaScript 6 features in Ionic?"
slug: "how-to-use-ecmascript-6-features-in-ionic"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## By using gulp-babel and gulp-plumber
Ionic uses Gulp, so install gulp-babel and gulp-plumber.

    npm install --save-dev gulp-babel gulp-plumber

Add babel to `gulpfile.js` like so:

    //...
    var babel = require("gulp-babel");
    var plumber = require("gulp-plumber");
     
    var paths = {
      es6: ['./src/es6/*.js'],
      sass: ['./scss/**/*.scss']
    };
      
    gulp.task('default', ['babel', 'sass']);
     
    gulp.task("babel", function () {
      return gulp.src(paths.es6)
        .pipe(plumber())
        .pipe(babel())
        .pipe(gulp.dest("www/js"));
    });
      
    //...
      
    gulp.task('watch', function() {
      gulp.watch(paths.es6, ['babel']);
      gulp.watch(paths.sass, ['sass']);
    });
    //...

Edit `ionic.project`:

    "gulpStartupTasks": [
        "babel",
        "sass",
        "watch"
     ],

Now when you run `ionic serve`, code will be transpiled for you.

