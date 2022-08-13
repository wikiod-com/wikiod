---
title: "Comprehensive Guide to a Front end Workflow with Gulpjs 2 of 2"
slug: "comprehensive-guide-to-a-front-end-workflow-with-gulpjs-2-of-2"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Cool. So we are all done with our workflow automation.

We now have a gulp file , that

 - Responsifies and minifies images
 - cleans,autoprefixes,concatenates and minifies Css
 - Concatenates and minifies JS
 - Watches for changes to assets be it HTML | CSS | JS and triggers associated tasks
 - Creates a build directory , and stores all processed deployment ready code inside it. And all that , in the background, while you just develop your app.

References
==========
[Run-Sequence][1]
[Browser-sync][2]



  [1]: https://www.npmjs.com/package/run-sequence
  [2]: https://www.npmjs.com/package/browser-sync

## Setting up Browser sync And Configuring Watchers for Style and Script
NOTE
----

> **This page illustrates use of gulp plugins like browser-sync ,**
> **gulp-watch and run-sequence , and continues discussing**
> **gulp-workflow-automation from where we left off  at
> Gulpjs-workflow-automation-1 of 2. In case you landed here , consider**
> going through that post first.

- Default Task
- Watchdog task  - to continually build your deployment ready assets on the fly , whenever anything image | JS | css changes on the course of development.

Let us begin with browser-sync.


Gulp Watchdog Task
------------------


Let us begin with the watchdog task.

The goal , is to watch  for changes you make while developing. Any change , should trigger the corresponding gulp task.

Also , we need a functionality that syncs your changes on the browser.

Browser sync

So , we need to install Browser Sync.

```
bash $ npm install browser-sync --save-dev
```

With that premise, let us open our gulpfile.js and add the watch functionality. Let us require browser sync and define some variables to use its functionality.

At the top of the gulpfile , add the below snippet. Place it just below the image compression declarations.

like so:

```
//Browser-sync

var sync = require('browser-sync').create();
var reload = sync.reload;

```

Having browser sync  sync your development on to the browser , is a simple configuration. Let us create a task called watchdog.

like so:

```
$.task('watchdog', function() {


})
```

Now , If we browse through browser sync options[ here ][1] , and search for the server setting ,  we can see how easy it is .

[1]: https://www.browsersync.io/docs/options/

We just need to place the below inside of our watchdog task

Snippet - 1 - inside watchdog boilerplate
=========================================

```
/*
 Initiate Browser sync
 @documentation - https://www.browsersync.io/docs/options/
 */
 sync.init({
  server: {
    baseDir: "./"
 },
 port: 8000 //change it as required
 });
```
Insert the above inside of your watchdog boilerplate above.

The next snippet , is to define a watcher for styles, with a goal to reprocess changed css files or new ones , and trigger a browser reload automatically.

snippet - 2 - inside watchdog boilerplate
=========================================

```
$.watch(['css/**/*', 'fonts/google/**/*.css'], reload).on('change', function(event) {
console.log(event.type + ':' + event.path)
if (event.type === 'deleted') {
uncache('styles', event.path);
$$.remember.forget('auto-prefixed-stylesheets', event.path);
}
sequence('optimizeStyles')
});

```
Insert the above inside of your watchdog boilerplate above.

So we are monitoring``` " [fonts/google/**/*.css , /**/*.css ]"``` i.e,

all css files under css/
all css files under fonts/google/
When anything changes, or a new file is added , it triggers the reload method, which is defined at the top of our gulpfile ,  in the browsersync declaration.

>Note : You might notice , that we have a **.on** event handler attached to the watcher.

```
$.watch(['css/**/*', 'fonts/google/**/*.css'], reload).on('change', function(event) 
```
Basically , anything CUD(Create| Update | Delete) triggers the reload function , and passes an event Object as a parameter to the
callback function.

The callback is a vital function , where we can achieve operations like uncache on asset deletion.Now the event object has parameters like

- path
- type - Create/Update/Delete


If an asset is deleted , we need to make sure the caches we built in our earlier minification functions , via gulp-cached and gulp-remember , need updation.

we are handling that in the snippet below , which is inside the callback on change.
```
if (event.type === 'deleted') { 
uncache('styles', event.path);
$$.remember.forget('auto-prefixed-stylesheets', event.path);  
}
```

Note
----
> $ - >alias for gulp
>
> $$ - > alias for gulp-load-plugins

you might also notice , that I have a ```sequence('optimizeStyles');``` after I wrote the uncache invocation

The sequence method , ensures , synchronous method runs in an asynchronous by default javascript.

installing it is simple

DO 

```
bash $ npm install run-sequence
```


then , declare it in your gulpfile just below the browser sync declaration.


```
var sequence = require('run-sequence');

```
So with that understanding , the watcher for scripts is an easy one to write. just different globs!

So, add this snippet below the style watcher inside the watchdog boilerplate.

Snippet - 3 - inside Watchdog task boilerplate
==============================================

```
/*
on addition or change or deletion of a file in the watched directories
the change event is triggered. An event object with properties like
path,
event-type
is available for perusal passed to the callback

*/
$.watch('js/**/*', reload).on('change', function(event) {
console.log(event.type + ':' + event.path)
if (event.type === 'deleted') {
uncache('scripts', event.path);
$$.remember.forget('linted-scripts', event.path);
}
sequence('optimizeScripts');
});

```

Note
----
>We used two functions in our snippets above.
> - uncache
> - $$.remember.forget
> Note:
>
> $-> Alias for gulp
>
> $$-> Alias for gulp-load-plugins

Let us define the function uncache somewhere in our gulpfile.js , before it is invoked.

```
/*
Deletes a cache entry
*/
var uncache = function(cacheName, cacheKey) {
        var cache = $$.cached;
        if (cache.caches[cacheName] && cache.caches[cacheName][cacheKey])
            return delete cache.caches[cacheName][cacheKey];
        return false;
    }
    /*
    logs current cache created via gulp-cached
    */
var viewCache = function() {
    console.log($$.cached.caches)
}
```






  [1]: https://www.browsersync.io/docs/options/

## Defining a Default Task
So now, let us finish the gulpfile code , by defining a Default task.

the default task is the one that runs, when you just say

```
gulp
```
on a command prompt under the root of your project.

```
$.task('default', ['generateResponsiveImages'], function() {
 $.start('watchdog');
 console.log('Starting Incremental Build');
});

```


