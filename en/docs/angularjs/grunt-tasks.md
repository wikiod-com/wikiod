---
title: "Grunt tasks"
slug: "grunt-tasks"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Run application locally
> Following example requires that [node.js][1] is installed and [npm][2] is available.<br>
> Full working code can be forked from GitHub @ https://github.com/mikkoviitala/angular-grunt-run-local


Usually one of the first things you want to do when developing new web application is to make it run locally. 

Below you'll find complete example achieving just that, using [grunt][3] (javascript task runner), [npm][2] (node package manager) and [bower][4] (yet another package manager).

*Beside your actual application files* you'll need to install few 3rd party dependencies using tools mentioned above. In your project directory, **preferably root**, you'll need three (3) files.

 * package.json (dependencies managed by npm)
 * bower.json (dependencies managed by bower)
 * gruntfile.js (grunt tasks)

So your project directory looks like so:

[![enter image description here][5]][5]

<hr>

**package.json**

We'll be installing **grunt** itself, **matchdep** to make our life easier allowing us to filter dependencies by name, **grunt-express** used to start express web server via grunt and **grunt-open** to open urls/files from a grunt task.

So these packages are all about "infrastructure" and helpers we'll be building our application on.

    {
      "name": "app",
      "version": "1.0.0",
      "dependencies": {},
      "devDependencies": {
        "grunt": "~0.4.1",
        "matchdep": "~0.1.2",
        "grunt-express": "~1.0.0-beta2",
        "grunt-open": "~0.2.1"
      },
      "scripts": {
        "postinstall": "bower install"
      }
    }

**bower.json**

Bower is (or at least should be) all about front-end and we'll be using it to install **angular**.

    {
      "name": "app",
      "version": "1.0.0",
      "dependencies": {
        "angular": "~1.3.x"
      },
      "devDependencies": {}
    }

**gruntfile.js**

Inside gruntfile.js we'll have the actual "running application locally" magic, which opens our application in new browser window, running on http://localhost:9000/

    'use strict';
    
    // see http://rhumaric.com/2013/07/renewing-the-grunt-livereload-magic/
    
    module.exports = function(grunt) {
      require('matchdep').filterDev('grunt-*').forEach(grunt.loadNpmTasks);
     
      grunt.initConfig({
        express: {
          all: {
            options: {
              port: 9000,
              hostname: 'localhost',
              bases: [__dirname]
            }
          }
        },
     
        open: {
          all: {
            path: 'http://localhost:<%= express.all.options.port%>'
          }
        }
      });
     
      grunt.registerTask('app', [
        'express',
        'open',
        'express-keepalive'
      ]);
    };


**Usage**

To get your application up & running from scratch, save above files to your project's root directory (any empty folder will do). Then fire up console/command line and type in the following to install all required dependencies.

    npm install -g grunt-cli bower
    npm install

And then run your application using
    
    grunt app

<hr>

Note that yes, you'll be needing your actual application files, too.<br>
For almost-minimal example browse [GitHub repository][6] mentioned in beginning of this example.

There structure ain't that different. There's just `index.html` template, angular code in `app.js` and few styles in `app.css`. Other files are for Git and editor configuration and some generic stuff. Give it a try!

[![enter image description here][7]][7]

[![enter image description here][8]][8]


  [1]: https://nodejs.org
  [2]: https://www.npmjs.com
  [3]: http://gruntjs.com/
  [4]: https://bower.io
  [5]: http://i.stack.imgur.com/GlnAc.png
  [6]: https://github.com/mikkoviitala/angular-grunt-run-local
  [7]: http://i.stack.imgur.com/58e4t.png
  [8]: http://i.stack.imgur.com/M1649.png

