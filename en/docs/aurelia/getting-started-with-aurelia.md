---
title: "Getting started with aurelia"
slug: "getting-started-with-aurelia"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World: Getting started with aurelia-cli
This example will show you how to quickly get a hello world Aurelia application up and running using the Aurelia CLI.

# Prerequisites

The Aurelia CLI is a Node.js based application, so make sure you install it first before proceeding. You will need [Node.js][1] 4.4.7 or later.

You will also need a Git client installed. Some great options include: [Github Desktop][2], [Sourcetree][3] and [Git SCM][4].

# Installing the CLI

Provided you installed Node.js and Npm correctly, open up a Command Prompt/PowerShell or Bash terminal and install the CLI globally using the following command:

    npm install aurelia-cli -g

Before proceeding, run `au -v` to make sure that the Aurelia CLI successfully installed. You should see a version number displayed.

# Creating your first Aurelia application

Now you have the CLI installed, to create a new project run the following command and following the informative on screen prompts:

    au new

You'll get a choice of different formats and loaders, to keep things simple just select the defaults. As you become more familiar with the CLI, you can configure these options to match your needs.

# Running your Aurelia application

To run your Aurelia application, from the same folder run: `au run` - you should now see a fully-functioning hello world application when you open up your application in a web browser. By default, the CLI dev server will be available at `http://localhost:9000`

# Conclusion

You have just successfully created a "hello world" Aurelia application using the CLI.

  [1]: https://nodejs.org/en/
  [2]: https://desktop.github.com/
  [3]: https://www.sourcetreeapp.com/
  [4]: https://git-scm.com/

