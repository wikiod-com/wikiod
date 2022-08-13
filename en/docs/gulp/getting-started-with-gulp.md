---
title: "Getting started with gulp"
slug: "getting-started-with-gulp"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Task dependency
You can run tasks in series, by passing a second parameter to `gulp.task()`.

This parameters is an array of tasks to be executed and completed before your task will run:

    var gulp = require('gulp');

    gulp.task('one', function() {
       // compile sass css
    });

    gulp.task('two', function() {
       // compile coffeescript
    });

    // task three will execute only after tasks one and two have been completed
    // note that task one and two run in parallel and order is not guaranteed
    gulp.task('three', ['one', 'two'], function() {
       // concat css and js
    });

    // task four will execute only after task three is completed
    gulp.task('four', ['three'], function() {
       // save bundle to dist folder
    });

You can also omit the function if you only want to run a bundle of dependency tasks:

    gulp.task('build', ['array', 'of', 'task', 'names']);

**Note:** The tasks will run in parallel (all at once), so don't assume that the tasks will start/finish in order. [Starting gulp v4](https://github.com/gulpjs/gulp/blob/4.0/CHANGELOG.md), `gulp.series()` should be used if the order of execution of dependency tasks is important.

## Installation or Setup
## 1. Install Node.js and NPM: ##

Gulp requires [Node.js](https://www.wikiod.com/node-js) and NPM, Node's package manager. Most installers include NPM with Node.js. [Refer to the installation documentation](https://www.wikiod.com/node-js/installing-nodejs) or confirm it is already installed by running the following command in your terminal,

    npm -v
    // will return NPM version or error saying command not found

## 2. Install gulp globally: ##

If you have previously installed a version of gulp globally, please run `npm rm --global gulp` to make sure your old version doesn't collide with **gulp-cli**.

    $ npm install --global gulp-cli

## 3. Initialize your project directory: ##


    $ npm init

## 4. Install gulp in your project devDependencies: ##


    $ npm install --save-dev gulp
## 5. Create a gulpfile.js at the root of your project: ##


    var gulp = require('gulp');

    gulp.task('default', function() {
      // place code for your default task here
    });

## 6. Run gulp: ##


    $ gulp

The default task will run and do nothing.

To run individual tasks, use `gulp <task> <othertask>`.

## Concat js file in sub folder using gulp
    var gulp = require('gulp'); 

    // include plug-ins
    var uglify = require('gulp-uglify'),
        concat = require('gulp-concat');

    // Minified file
    gulp.task('packjsMin', function() {
       return gulp.src('node_modules/angular/*.js')
          .pipe(concat('script.js'))
          .pipe(uglify())
          .pipe(gulp.dest('Scripts'));
    });

    //Not minified file
    gulp.task('packjs', function () {
        return gulp.src('node_modules/angular/*.js')
          .pipe(concat('allPackages.js'))
          .pipe(gulp.dest('Scripts'));
    });

## gulp CLI docs
### **Flags**

gulp has very few flags to know about. All other flags are for tasks to use if needed.

- `-v` or `--version` will display the global and local gulp versions
- `--require <module path>` will require a module before running the gulpfile. This is useful for transpilers but also has other applications. You can use multiple `--require` flags
- `--gulpfile <gulpfile path>` will manually set path of gulpfile. Useful if you have multiple gulpfiles. This will set the CWD to the gulpfile directory as well
- `--cwd <dir path>` will manually set the CWD. The search for the gulpfile, as well as the relativity of all requires will be from here
- `-T` or `--tasks` will display the task dependency tree for the loaded gulpfile
- `--tasks-simple` will display a plaintext list of tasks for the loaded gulpfile
- `--color` will force gulp and gulp plugins to display colors even when no color support is detected
- `--no-color` will force gulp and gulp plugins to not display colors even when color support is detected
- `--silent` will disable all gulp logging

The CLI adds process.env.INIT_CWD which is the original cwd it was launched from.

### **Task specific flags**

Refer to this [StackOverflow](http://stackoverflow.com/questions/23023650/is-it-possible-to-pass-a-flag-to-gulp-to-have-it-run-tasks-in-different-ways) link for how to add task specific flags

### **Tasks**

Tasks can be executed by running `gulp <task> <othertask>`. Just running `gulp` will execute the task you registered called `default`. If there is no `default` task gulp will error.

### **Compilers**

You can find a list of supported languages at [interpret](https://github.com/tkellen/node-interpret#jsvariants). If you would like to add support for a new language send pull request/open issues there.

