---
title: "Comprehensive Guide to a Front-end Workflow Automation with Gulpjs -1 of 2"
slug: "comprehensive-guide-to-a-front-end-workflow-automation-with-gulpjs--1-of-2"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
 - npm install [plugin-name] --save-dev
 - npm install [plugin-name] --save
 - 
    Function ```<function-name>``` (glob) { $.src(glob).pipe([plugin 1]).pipe([plugin 2])....pipe([plugin n]).pipe( $.dest(```<destination-name>```) }
 - ```$.task(<taskname> , [dependencies] , <body>);```



[Continue Reading - Creating a default task and setting up browser-sync][1]

References

- [Gulp-Clean-Css][2]
- [Gulp-Uglify][3]
- [Gulp-Autoprefixer - for Css Prefixes that maintain browser compatibility][4]
- [Gulp-Cached][5]
- [Gulp-Remember][6]
- [Gulp-Jshint][7]

[Step-By-Step Guide to Gulp Workflow Automation for absolute beginners that I documented iny my blog][8]


  [1]: https://www.wikiod.com/gulp
  [2]: https://www.npmjs.com/package/gulp-clean-css
  [3]: https://www.npmjs.com/package/gulp-uglify
  [4]: https://www.npmjs.com/package/gulp-autoprefixer
  [5]: https://github.com/contra/gulp-cached
  [6]: https://www.npmjs.com/package/gulp-remember
  [7]: https://www.npmjs.com/package/gulp-jshint
  [8]: http://blog.nitinprabhakar.com/gulp-workflow-automation-1/

## Loading All The Plugins from Package.JSON
Assuming , you have a grasp of how to install gulp, let us dive right down to requiring all the gulp-<plugin-name> dependencies from package.json under your projects root folder.

In case you do not have a gulpfile yet , please create an empty file with the name


***gulpfile.js***

First , we require gulp. like so:

```
var $ = require('gulp');

```

Next up , let us load all our plugins, into our gulpfile , by the below snippet

Note
====

Please read through the comments in all of the snippets we will be including in this read, they provide more insight into every functionality

```
/*
require gulp-load-plugins, and call it
gulp-load-plugins will require individually,
all the plugins installed in package.json at the root directory
these required plugins, are lazy loaded
that is, gulp-load-plugins will only require them when they are referenced in this file
plugins are available for perusal, via camelcased names, minus gulp
eg: gulp-clean-css is accessed by $$.cleanCss
*/
var $$ = require('gulp-load-plugins')();
```


----------

NOTE
----

Throughout the many examples we will have in the article , we alias 

1. gulp as $ and
2. gulp-load-plugins as $$

 purely for ease of typing.

## Installing Plugins for Responsive images|Css Minification|Js minification
Our goal , is to

 1. Make your Images conform to widths and scale appropriately, by generating a battery of images of varied widths, and to minify them
 3. Lint your Javascript
 4. Minimize your assets - JS/CSS/HTML , thus enabling you to host a lighter than lightest code
5. Watch the css/js/image files for change , and rebuild optimizations
6. Synching your changes while in development, on to a browser serving your site.

We need a number of plugins, so let us install all of them. Please run all these in the project's root folder.

Image processing plugins
------------------------

```
bash $ npm install --save-dev gulp-responsive 
bash $ npm install --save-dev gulp-imagemin 
bash $ npm install --save-dev imagemin
bash $ npm install --save-dev imagemin-jpeg-recompress
bash $ npm install --save-dev imagemin-pngquant 

```

Asset optimizer plugins
-----------------------

```
bash $ npm install --save-dev gulp-clean-css
bash $ npm install --save-dev gulp-uglify
bash $ npm install --save-dev gulp-minify-html
bash $ npm install --save-dev gulp-jshint
bash $ npm install --save-dev gulp-concat
bash $ npm install --save-dev gulp-autoprefixer
```

## Anatomy of a gulp function
```
[Function <name>] (glob) {

$.src(glob)

.pipe([plugin 1])
.pipe([plugin 2])
.
.
.
.pipe([plugin n])
.pipe( $.dest(<destination-name>)

}
```

Note
----

> pipe is a method that streams all the files matching the glob input , to our plugins( minifyhtml in this case) .
> 
> It is simple to picture it like so:
> 
> $.src is what builds the stream and pipe  pipes out each individual
> file matching the glob downwards to each plugin in the pipeline.Each
> plugin the file is passed to , modifies its contents in memory only
> until $.dest  is reached , which then updates/creates files streamed
> by  $.src

**Where** ***,***

$ - > gulp
----------

$$ - > gulp-load-plugins
------------------------

## Asset Optimization and Minification

---------------

So, Before writing out optimiser functions , we need to install a couple of caching plugins.

```
bash $ npm install --save-dev gulp-cached
bash $ npm install --save-dev gulp-remember
```

You might wonder why two caches eh!. **gulp-cached** , passes only modified or new content down the pipeline to other plugins.
So, Since we want files without change to be used for concatenating into a single file per asset( css | js ) as well, we need **gulp-remember** in addition to gulp-cached

First we use **gulp-cached** to build a list of files that have changed

Second , we need **gulp-remember** to keep a track of all files that are passed through by that list in memory.

First Run : No files are cached , so gulp-cached will pass them all to gulp-remember

Subsequent runs : Only modified or new files are piped down by gulp-cached.
Since the content of the current file has changed , gulp-remember updates its cache.

Cool , Let us write our first optimizer

Style Optimizer
---------------

```

// Goal

/*

1. cache existing files on the first run
2. each file , 
       a. Is autoprefixed with cross browser compatible prefixes for any css property ( justify-content for e.g)
       b. Is concatenated into app.css
3. app.css is minified
4. On subsequent runs , the above process is implemented on file modifications/additions only

*/

/*
*@src - input a glob pattern - a string eg 'images/**/*' or '**/*.css' or, an array eg ['glob1','glob2']
*/
var optimizeStyles = function(src) {

return $.src(src).
pipe($$.cached('styles')).
pipe($$.autoprefixer({
browsers: ['last 2 versions']
})).
pipe($$.remember('auto-prefixed-stylesheets')).
pipe($$.concat('app.css')).
pipe($.dest('build/css')).
pipe($$.cleanCss()).
pipe($$.rename({
suffix: '.min'
})).
pipe($.dest('build/css'))
}
```


Note
----

> 
> $ - > gulp
> 
> $$ - > gulp-load-plugins
> 
> $.src - > builds file streams matching the glob passed as src
> 
> $.dest - > saves the manipulated file in the path specified

Script Optimiser
----------------

```

// Goal

/*

1. cache existing files on the first run
2. each file , 
       a. Is linted with jshint 
       b. Is concatenated into app.js
3. app.js is minified
4. On subsequent runs , the above process is implemented on file modifications/additions only

*/

/*
*@src - input a glob pattern - a string eg 'js/**/*' or '**/*.js' or, an array eg ['glob1','glob2']
*/

var optimizeScripts = function(src) {

    return $.src(src).
    pipe($$.cached('scripts')).
    pipe($$.jshint()).
    pipe($$.jshint.reporter('default')).
    pipe($$.jshint.reporter('fail')).
    pipe($$.remember('linted-scripts')).
    pipe($$.concat('app.js')).
    pipe($.dest('build/js')).
    pipe($$.uglify()).
    pipe($$.rename({
        suffix: '.min'
    })).
    pipe($.dest('build/js'))


}

```
Note
----

> 
> $ - > gulp
> 
> $$ - > gulp-load-plugins
> 
> $.src - > builds file streams matching the glob passed as src
> 
> $.dest - > saves the manipulated file in the path specified

Generate Responsive Images
--------------------------

Let us now move on to image processing. So, the aim , is to have an array of sizes for each image you are going to serve.

Why?

>    Well , to understand why we need a battery of images with a range of widths , we need to ponder over the fact , that there are probably
> zillions of devices with varied resolutions.
> We need an image to scale without much pixelation. At the same time , we need to improve page load times , by downloading just the
> one image , which fits the width it is contained by , and is also with
> the smallest possible dimension , to do so. There are scholarly blogs
> like the one Eric Portis wrote , which highlights the ineffectiveness
> of just media queries and serves as a comprehensive guide to
> understanding concepts like srcsets and sizes.

You can refer to [Eric Portis' epic write up here][1]


Now, Our function , needs to take a glob , and a width as inputs, and do its magic and push the file each run generates , to a destination and minify the responsified image.

We have installed two image compression plugins [in our first example][2]


Since these plugins **DO NOT** start with a "gulp-" prefixed, we need to manually load them onto our gulpfile.

SO Let us require them manually , after the gulp-load-plugins declaration at the top of our gulpfile.

like so:

```
var compressJpg = require('imagemin-jpeg-recompress');
var pngquant = require('imagemin-pngquant');
```

> It is worthwhile to note , that gulp-responsive comes with the sharp
> image processor , which is better than imagemagick BY FAAAAR!. Sharp
> is what is used by gulp-responsive , to crop your images to desired
> widths.
> 
> you might look at gulp-responsive-configuration-options,for a
> comprehensive list of configuration params. I have only used
> 
>    - width - to crop our images to a width w, passed as a parameter
>    - rename - to add a suffix to the image name, so that it remains unique 
> 
> in my configuration function below.
> so our function , will crop the image to the width passed as input ,
> for all matching images deciphered via the glob input.
> then, each image is compressed using jpeg-recompress or pngquant 
> and saved inside build/images.

With that in mind, our function would be like so:

```
/*
@generateResponsiveImages
*@Description:takes in a src of globs, to stream matching image files , a width,
*to resize the matching image to, and a dest to write the resized and minified files to
*@src - input a glob pattern - a string eg 'images/** /*' or 'images/*' or, an array
eg ['glob1','glob2']
*@return returns a stream
*/
var generateResponsiveImages = function(src, width, dest) {

    //declare a default destination
    if (!dest)
        dest = 'build/images';
    //case 1: src glob -  images/**/*
    // the base is the directory immediately preceeding the glob - images/ in this case
    //case 2: images/fixed/flourish.png : the base here is images/fixed/ - we are overriding
    // that by setting base to images.This is done so that, the path beginning after images/
    // - is the path under our destination - without us setting the base, dest would be,
    //build/images/flourish.png.But with us setting the base, the destination would be
    // build/images/fixed/flourish.png
    return $.src(src, {
        base: 'images'
    })

    //generate resized images according to the width passed
    .pipe($$.responsive({
            //match all pngs within the src stream
            '**/*.png': [{
                width: width,
                rename: {
                    suffix: '-' + width
                },
                withoutEnlargement: false,
            }],
            //match all jpgs within the src stream
            '**/*.jpg': [{
                width: width,
                rename: {
                    suffix: '-' + width
                },
                progressive: true,
                withoutEnlargement: false,
            }]
        }, {

            errorOnEnlargement: false,
            errorOnUnusedConfig: false,
            errorOnUnusedImage: false

        }))
        //once the file is resized to width, minify it using the plugins available per format
        .pipe($$.if('*.jpg', compressJpg({
            min: 30,
            max: 90,
            target: 0.5
        })()))
        //use file based cache gulp-cache and it will minify only changed or new files
        //if it is not a new file and if the contents havent changed, the file is served from cache
        .pipe($$.cache($$.imagemin({
            verbose: true
        })))


    //write to destination - dest + path from base
    .pipe($.dest(dest));
}

```
Note
----

> 
> $ - > gulp
> 
> $$ - > gulp-load-plugins
> 
> $.src - > builds file streams matching the glob passed as src
> 
> $.dest - > saves the manipulated file in the path specified

Further references
==================

- [Gulp-Responsive][3]
- [Gulp-Imagemin][4]


  [1]: http://ericportis.com/posts/2014/srcset-sizes/
  [2]: https://www.wikiod.com/gulp
  [3]: https://www.npmjs.com/package/gulp-responsive
  [4]: https://www.npmjs.com/package/gulp-imagemin

HTML Minifier
-------------

```
*
 *@minifyHtml
 *Description:takes in a glob src, and minifies all '.html' files matched by the glob
 *@src - input a glob pattern - a string eg '/**/*.html /*' or '*.html' or, an array eg ['glob1','glob2']
 *@dest=file.base means, the modified html file will be in the same directory as the src file being minified
 *often means, the process is just a modification on the existing src file
 *@return returns a stream
 */
var minifyHtml = function(src) {
    return $.src(src)
        .pipe($$.minifyHtml())
        .pipe($.dest(function(file) {
            //file is provided to a dest callback -
            // Refer here https://github.com/gulpjs/gulp/blob/master/docs/API.md#path
            return file.base;
        }));
}
```

## Anatomy of a gulp task
The anatomy of a task definition is like so:

```$.task(<taskname> , [dependencies] , <body>);```

dependencies , is an array of tasks that HAVE to finish before the current task you are defining , runs. More like forcing a synchronous execution instead of the default Asynchronous functionality.

## Adding Gulp Tasks
So, we now have 
- A function defined Above to optimise Styles
- A function defined Above to optimise scripts
- A function defined Above to optimise HTML
- A function to generate multiple images per image Above

All we need to do now, is to invoke them when needed.

Let us write our tasks according to the syntax we defined earlier

```
/*
* $.task('name','dependency array',function)
results in building a task object as below
task:{
'name':name,
'dep':[array of dependencies],
'fn':function
}
*/


//*@return returns a stream to notify on task completion
$.task('optimizeHtml', function() {
    var src = ['**/*.html', '!{node_modules}/**/*.html'];
    return minifyHtml(src);
});

//*@return returns a stream to notify on task completion
$.task('optimizeScripts', function() {
    var src = ['js/**/*.js'];
    return optimizeScripts(src);
});

//*@return returns a stream to notify on task completion
$.task('optimizeStyles', function() {
    var src = ['css/**/*.css', 'fonts/google/**/*.css'];
    return optimizeStyles(src);
});

//Take in a callback to ensure notifying the gulp engine, that the task is done
//required since, you are not returning a stream in this task
$.task('generateResponsiveImages', function(callback) {
    var src = ['images/**/*.{jpg,png}'];
    for (var i = widths.length - 1; i >= 0; i--) {
        generateResponsiveImages(src, widths[i]);
    }
    callback();

});
```

