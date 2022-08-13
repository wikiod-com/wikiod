---
title: "Delete Files Using Gulp"
slug: "delete-files-using-gulp"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Note on using the globbing pattern (`**`):

The globbing pattern matches all `children` **and** the `parent`. In order to avoid that we add `'!public'` to our del task so that the `public` directory itself doesn't get deleted

## Delete files using del
First, Install `gulp` and `del` to project directory locally

    npm install --save-dev gulp del

Then add the `clean` task to your `gulpfile.js`

    var gulp = require('gulp');
    var del = require('del');
    
    gulp.task('default', function() {
    });
    
    // Task to delete target build folder
    gulp.task('clean', function() {
      return del(['public/**', '!public']);
    });
    
    gulp.task('default', ['clean']);

This task deletes all files in the public directory

The task in the code is added as a dependency for the `'default'` task so every time `default` will run, `clean` will run before it.

You  can also call the `clean` task manually by running the command:

    gulp clean


