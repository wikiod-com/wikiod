---
title: "Getting started with ember-cli"
slug: "getting-started-with-ember-cli"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Ember-cli first requires Node and NPM to be installed on the system. Either follow the installation instructions on [nodejs.org][1], or use a preferred package manager (such as [Homebrew][2] on OSX.) It's recommended to install latest version of each. 

Once its done, run the following commands to ensure installation was correct:

    node -v
    npm -v

Since [Yarn][3] package manager has been released recently (October 2016), it's possible to install dependencies with Yarn instead of NPM. Checking the guide on [yarn's website][3] for further details. 

Next, install Ember CLI globally:

    npm install -g ember-cli

OR

    yarn global add ember-cli 

This will grant access to the ember command-line runner.

**BOWER**

Globally install Bower, a package manager that keeps front-end dependencies up-to-date. (including jQuery, Ember, and QUnit)

    npm install -g bower

OR

    yarn global add bower

This will grant access to the bower command-line runner.

**PhantomJS**

With Ember CLI, use a preferred automated test runner. Most testing services recommend or require PhantomJS, which can be installed via npm or the PhantomJS website. (PhantomJS is the default test runner for Testem and Karma.)

To use PhantomJS for integration tests, it must be globally installed:

    npm install -g phantomjs-prebuilt

or 

    yarn global add phantomjs-prebuilt


**Watchman**

On OSX and UNIX-like operating systems, it is recommended to install Watchman version 4.x. This provides Ember CLI a more effective way for watching project changes.

File-watching on OSX is error-prone and Node’s built-in `NodeWatcher` has trouble observing large trees. [Watchman][4] solves these problems and performs well on extremely massive file trees.

On OSX, install Watchman using Homebrew:

    brew install watchman

For complete installation instructions, [refer to the docs on the Watchman website][4]. 

Do *not* use an NPM version of Watchman. The following command can be used to uninstall it:

    npm uninstall -g watchman

**Congratulations!** Now you are able to create your first project by running:

    ember new my-first-app

start Ember server by running :

    ember s

Navigate to `http://localhost:4200` to see the new app in action.

Navigate to `http://localhost:4200/tests` to see the test results in action.


  [1]: http://nodejs.org
  [2]: http://brew.sh/
  [3]: http://yarnpkg.org
  [4]: https://facebook.github.io/watchman/

