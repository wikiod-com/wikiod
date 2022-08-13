---
title: "Directory Structure"
slug: "directory-structure"
draft: false
images: []
weight: 9818
type: docs
toc: true
---

Before the release of Meteor 1.3, Meteor developers were frustrated with Meteor.js' handling of file dependencies and global variables. In response, Meteor set new standards for project structures in order to make the project dependency system more streamlined. This topic explains the standardized project structure and the principles behind it.

**client**  
All code in the client directory is run only in the client-side, or web browser.

**client/compatibility**  
The compatibility directory contains legacy or 3rd party code, such as jQuery libraries, etc.

**lib**  
The lib directory is loaded before other directories in your Meteor project, and is loaded on both the server and client. This is the preferred place to define data models, isomorphic libraries, and business logic.

**imports**  
The imports directory is a directory on the server that is available to both the server and client, but only before the client bundle gets shipped to the client.

**packages**  
The packages directory is where custom packages are stored during local development. When using the standard command `meteor add package:name` to add a package, Meteor will look first in this directory if  a local package has the corresponding description name in its `package.js` file. If not, it will poll Atmosphere as usual.

**private**  
The private directory contains static files that should only be available on the web server.

**public**  
The public directory contains static files that are only available on the application client. This may including branding assets, etc.

**server**  
The server directory contains server-side assets. This can include authentication logic, methods, and other code that may need security consideration.

**tests**  
The tests directory is omitted by default when your application is bundled and deployed.

As suggested [by Richard Silverton][1] it is a convenient idea to put not only the meteor project directory under version control, but also its parent directory. 

That way you can keep files under version control without having meteor to deal with it.


  [1]: https://blog.tableflip.io/large-meteor-projects-best-practices/

## Classic Directory Structures
The first thing you need to know when structuring your apps is that the Meteor tool has some directories that are hard-coded with specific logic. At a very basic level, the following directories are "baked in" the Meteor bundler.

<!-- language: lang-bash -->

```
client/                                  # client application code
client/compatibility/                    # legacy 3rd party javascript libraries
imports/                                 # for lazy loading feature 
lib/                                     # any common code for client/server.
packages/                                # place for all your atmosphere packages
private/                                 # static files that only the server knows about
public/                                  # static files that are available to the client
server/                                  # server code
tests/                                   # unit test files (won't be loaded on client or server)
```

Reference page: [Meteor Guide > Special directories][1]


  [1]: https://guide.meteor.com/structure.html#special-directories

## Directory load order
>HTML template files are always loaded before everything else
>
>Files beginning with *main.* are loaded last
>
>Files inside any lib/ directory are loaded next
>
>Files with deeper paths are loaded next
>
>Files are then loaded in alphabetical order of the entire path

[Reference Link][1]


  [1]: https://guide.meteor.com/structure.html

Reference page: [Meteor Guide > Application Structure > Default file load order][1]


  [1]: https://guide.meteor.com/structure.html#load-order

## Imports/Modules Directory Structure
The most recent versions of Meteor ship with support for `ecmascript`, aka ES6 or ES2015.  Instead of packages, Javascript now supports `import` statements and modules, which replaces the need for package-only applications.  The latest directory structure is similar to the package-only structure, but uses the `/imports` directory instead of `/packages`.

<!-- language: lang-bash -->

```
imports                                 #
imports/api                             # isomorphic methods 
imports/lib                             # any common code for client/server
imports/client                          # client application code
imports/server                          # server code
```


## Package-Only Directory Structure
Many people find themselves eventually supporting multiple applications, and desire to share code between apps.  This leads to the concept of microservice architecture, and all-package apps.  Essentially, the code from the entire classic directory structure is refactored out into packages.  

Even though there is no hard-coded logic for directories in packages, we find that it's a good practice to use the classic directory structure when creating packages. This creates a natural refactor path as features are prototyped in the app, and then extracted into packages to be published and shared.  The directory names are shared, so there's less confusion among team members.

<!-- language: lang-bash -->

```
client/                                  # client application code
packages/                                # place for all your atmosphere packages
packages/foo/client                      # client application code
packages/foo/lib                         # any common code for client/server
packages/foo/server                      # server code
packages/foo/tests                       # tests
server/                                  # server code
```

## Mixed-Mode Directory Structure
And, of course, you can mix these approaches, and use both packages and imports along side your application specific code.  A mix-mode structure is most common in three situations:  a franken-app, which is just sort of pulling a bit from here-and-there without any overall strategy; an app that's being actively refactored from either Classic or Package-Only structures to the Imports/Modules structure.

<!-- language: lang-bash -->

```
client/                                  # client application code
client/compatibility/                    # legacy 3rd party javascript libraries
imports                                  #
imports/api                              # isomorphic methods 
imports/lib                              # any common code for client/server
imports/client                           # client application code
imports/server                           # server code
lib/                                     # any common code for client/server.
packages/                                # place for all your atmosphere packages
packages/foo/client                      # client application code
packages/foo/lib                         # any common code for client/server
packages/foo/server                      # server code
packages/foo/tests                       # tests
private/                                 # static files that only the server knows about
public/                                  # static files that are available to the client
server/                                  # server code
tests/                                   # unit test files (won't be loaded on client or server)
```



