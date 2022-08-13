---
title: "Electrify - Compiling Meteor as a Locally Installable App"
slug: "electrify---compiling-meteor-as-a-locally-installable-app"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

## Installing Electrify for a Meteor application
Electron ports HTML web applications to native applications for a range of devices, including creating native desktop applications. It's also very easy to get started!

To begin, we must have `electron`, `nodejs`, `npm`, `git` and `meteor` installed. Familiarity with these tools is vital for working with Meteor, so make sure you know about these things first.

---

**Electron**

    npm install -g electrify

 - `electron` is what we're using! Read more [here][1].
 - `electrify` is a tool for packaging Meteor apps. Read mode [here](https://github.com/arboleya/electrify).

---

**Other requirements for installing and using Electrify with Meteor**

**Meteor**

    curl https://install.meteor.com/ | sh

There are many ways to install Meteor, see [here][2].

 - `meteor` is the JavaScript framework we'll be using for building our application. It provides us with a lot of coding simplifications for some rather conceptually hard problems in web applications; its simplicity has been noted as useful for prototypical projects. Read more [here][3].

**NodeJS**

    apt-get install nodejs build-essentials

There are many ways to install, depending on your OS. Find out which way you need [here][4].

 - `nodejs` is the package for Node.js, which is a Javascript environment for running JavaScript on the server side. Read more [here][5]. 

**npm**

`npm` should be bundled with the `nodejs` installation. Check it is by running the command `npm -v` after installing `nodejs`.

 - `npm` is the Node Package Manager. It's a huge collection of open source modules that you can easily add into your Node projects. Read more [here][6].


  [1]: http://electron.atom.io/
  [2]: https://www.meteor.com/install
  [3]: https://www.meteor.com/tutorials/blaze/creating-an-app
  [4]: https://nodejs.org/en/download/package-manager/
  [5]: https://nodejs.org/en/about/
  [6]: https://docs.npmjs.com/getting-started/what-is-npm

## Using Electrify on a Meteor Application
Let's download a Meteor Todos example project, using a Linux shell (command line) script, to test out Electrifying a project for the first time:

---

**Requirements for this section:**

**Git**

    apt-get install git-all

There are many ways to install Git. Check them out [here][1].

 - `git` is a version control system for files. They can be stored remotely (i.e., online) in public repositories (GitHub being a rather famous one) or private repositories (BitBucket provides limited free private repositories, as an example). Read more [here][5].

---

    #!/usr/bin/bash

    # Change this parameter to choose where to clone the repository to.
    TODOSPATH="/home/user/development/meteor-todos"

    # Download the repository to the $TODOSPATH location.
    git clone https://github.com/meteor/todos.git "$TODOSPATH"

    # Change directory (`cd`) into the Todos project folder.
    cd "$TODOSPATH"

We should now have a project folder named 'meteor-todos', at the location specified within the TODOSPATH parameter. We've also changed directory (`cd`) into the project folder, so let's add Electrify to this project!

    # It's really this simple.
    electrify

That's right - a single word command, and our project is ready. Permissions may cause errors for you when trying to run `electrify` as a command, in wihch case try `sudo electrify` to override the permissions. 

However, do attempt to resolve these permission issues - it is not good practice to unnecessarily `sudo` (which I'd elaborate upon, but I could write a whole other topic on why that is!)


  [1]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git

