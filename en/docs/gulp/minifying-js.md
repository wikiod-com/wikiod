---
title: "Minifying JS"
slug: "minifying-js"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
 - `ext` An object that specifies output source and minified file extensions.
 - `source` The suffix string of the filenames that output source files ends with.
 - `min` When string: The suffix string of the filenames that output minified files ends with.
 - When Array: The regex expressions to be replaced with input filenames. For example: [/\.(.*)-source\.js$/, '$1.js']
 - `exclude` Will not minify files in the dirs.
 - `noSource` Will not output the source code in the dest dirs.
 - `ignoreFiles` Will not minify files which matches the pattern.
 - `mangle` Pass `false` to skip mangling names.
 - `output` Pass an object if you wish to specify additional `output options`. The defaults are optimized for best compression.
 - `compress` Pass an object to specify custom `compressor options`. Pass false to skip compression completely.
 - `preserveComments` A convenience option for options.output.comments. Defaults to preserving no comments.
 - `all` Preserve all comments in code blocks
 - `some`Preserve comments that start with a bang `(!)` or include a Closure Compiler directive `(@preserve, @license, @cc_on)`
 - `function` Specify your own comment preservation function. You will be passed the current node and the current comment and are expected to return either `true` or `false`.

[Useful Links to gulp-minify][1]


  [1]: https://www.npmjs.com/package/gulp-minify

## Minify JS using gulp-minify
First, Install `gulp` and `gulp-minify` to project directory locally

    npm install --save-dev gulp gulp-minify

Then add following `min-js` task to your `gulpfile.js`

    var gulp = require('gulp');
    var minify = require('gulp-minify');
    
    gulp.task('min-js', function() {
        return gulp.src('lib/*.js')
            .pipe(minify({
                ext: {
                    min: '.min.js'
                },
                ignoreFiles: ['-min.js']
            }))
            .pipe(gulp.dest('lib'))
    });
    
    gulp.task('watch', function(){
      gulp.watch('lib/*.js', ['min-js']); 
      // Other watchers
    });

    gulp.task('default', ['min-js', 'watch']);


This task find all js files in `lib` directory, minfy it and save to `lib` directory with `.min.js` suffix. For example, after minify `lib/app.js` file will be created a `lib/app.min.js` file

Besides running as a dependency for the `'default'` gulp task, this task can be run manually by typing the following command:

    gulp min-js

