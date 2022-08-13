---
title: "Getting started with loopbackjs"
slug: "getting-started-with-loopbackjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation & Getting started
**Installation:**

To install the LoopBack command-line interface (CLI) tool:

    npm install -g loopback-cli

This installs the `lb` command-line tool for scaffolding and modifying LoopBack applications.


**Create new application:**

To create a new application:

    lb

The LoopBack application generator will greet you with some friendly ASCII art and prompt you for the name of the application. Simply answer questions asked by tool to create an application.



**Create first model:**

Go into your new application directory, then run: 

    lb model

Again, simply answer all questions asked by tool and create your first model.


**Run the application:**
From your project's root folder, run:

    node .

Open your browser to http://0.0.0.0:3000/ (on some systems, you may need to use http://localhost:3000 instead).  You’ll see the default application response that displays some JSON with some status information.






