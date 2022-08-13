---
title: "Getting started with ember.js"
slug: "getting-started-with-emberjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Getting started with Ember is easy. Ember projects are created and managed
through our command line build tool Ember CLI.
This tool provides:

* Modern application asset management (including concatenation, minification, and versioning).
* Generators to help create components, routes, and more.
* A conventional project layout, making existing Ember applications easy to approach.
* Support for ES2015/ES6 JavaScript via the [Babel](http://babeljs.io/docs/learn-es2015/) project. This includes support for [JavaScript modules](http://exploringjs.com/es6/ch_modules.html), which are used throughout this guide.
* A complete [QUnit](https://qunitjs.com/) test harness.
* The ability to consume a growing ecosystem of [Ember Addons](https://emberobserver.com/).

## Dependencies

### Node.js and npm

Ember CLI is built with JavaScript, and expects the [Node.js](https://nodejs.org/)
runtime. It also requires dependencies fetched via [npm](https://www.npmjs.com/). npm is packaged with Node.js, so if your computer has Node.js
installed you are ready to go.

Ember requires Node.js 0.12 or higher and npm 2.7 or higher.
If you're not sure whether you have Node.js or the right version, run this on your
command line:

```bash
node --version
npm --version
```

If you get a *"command not found"* error or an outdated version for Node:

* Windows or Mac users can download and run [this Node.js installer](http://nodejs.org/download/).
* Mac users often prefer to install Node using [Homebrew](http://brew.sh/). After
installing Homebrew, run `brew install node` to install Node.js.
* Linux users can use [this guide for Node.js installation on Linux](https://github.com/joyent/node/wiki/Installing-Node.js-via-package-manager).

If you get an outdated version of npm, run `npm install -g npm`.

### Git

Ember requires Git to manage many of its dependencies. Git comes with Mac OS
X and most Linux distributions. Windows users can
download and run [this Git installer](http://git-scm.com/download/win).

### Watchman (optional)

On Mac and Linux, you can improve file watching performance by installing [Watchman](https://facebook.github.io/watchman/docs/install.html).

### PhantomJS (optional)

You can run your tests from the command line with PhantomJS, without the
need for a browser to be open. Consult the [PhantomJS download instructions](http://phantomjs.org/download.html).

## Installation

Install Ember using npm:

```bash
npm install -g ember-cli
```

To verify that your installation was successful, run:

```bash
ember -v
```

If a version number is shown, you're ready to go.


## Assign localhost ports (esp. permissions/availability issues, running multiple ember sites simultaneously)
Occasionally it's useful to assign one or more ports manually vs using the defaults. Doing so can solve port availability/permissions issues or accommodate running more than one ember instance at a time.

----------

To have ember-cli attempt to identify and assign an available port, use:

    ember serve --port 0

*Per ember help: "Pass 0 to automatically pick an available port". (In a terminal, type ember help).*

----------

To run more than one ember site at the same time, each needs its own server and live-reload ports. A simple approach: in separate Terminal windows navigate to each instance and use the following to launch them with their own ports:

    ember serve --port 0 --live-reload-port 0

----------

If you get an error about availability or permission in any of these cases, enter the following python script at your Terminal prompt to identify an available port:

    python -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()'

Use the results to specify ports you now know to be available:

    ember serve --port <known_port_1> --live-reload-port <known_port_2>

## Creating App

Ember CLI allows you to use one of two options to generate a new app:

1. Create a folder and run `ember init` (generates application structure and sets up git and makes your first commit)
2. Run `ember new <app name>` (creates a folder with the specified name, steps into it and runs `ember init`)

Once the generation process is complete, boot a live-reload server within the app folder by running:

    ember server

or `ember s` for short. **Ta-da, now you have a running Ember app!*  [Official Docs][1]


  [1]: https://ember-cli.com/user-guide/#getting-started

**Creating your first template**

Let's create a new template using the `ember generate` command. 

    ember generate template application
The `application` template is always on screen when a user is visiting your application. In your editor of choice, open `app/templates/application.hbs` and add the following code:

    <h2>My first Ember application</h2>

    {{outlet}}
Now you should see the newly added text on the welcome page of your application. Also notice that Ember automatically detects the new file and reloads the page for you. Neat, right?

## Deploy App
To deploy an Ember application simply transfer the output from ember build to a web server. This can be done with standard Unix file transfer tools such as `rsync` or `scp`. There are also services that will let you deploy easily.

```
ember build
scp -r dist/* myserver.com:/var/www/public/
```

normally we would use `ember build --environment=production` which does more to make your code ready for production (gzip and minify code).

## How to work with JavaScript plugins
There are four ways to work with JavaScript plugins,

1. Ember add-on
1. Import JavaScript plugins globally
1. Consume named AMD plugins
1. Via `ember-browserify`

