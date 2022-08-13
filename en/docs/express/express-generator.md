---
title: "express-generator"
slug: "express-generator"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Parameters
Parameter | Definition
------ | ------
-h, --help   | output usage information
-V, --version   |  output the version number
-e, --ejs   | add pjs (Embedded JavaScript) templating engine support (defaults to jade, which has been renamed to Pug)
--hbs   | add handlebars templating engine support
-H, --hogan   | add hogan.js engine support
--git   | add .gitignore
-f, --force   | force on non-empty directory
-c &lt;engine>, --css &lt;engine> | add stylesheet &lt;engine> support (less, stylus ,compass, sass) (default is css)

Express generator is a great tool for getting a project up and rolling quickly. Once you understand the organization it implements, it's a real time saver.

## Installing Express Generator
`npm --install express-generator -g`

## Creating an App
`express my-app`

## Start App

Using start option

`npm start`

Using Nodemon 

    nodemon 

Using forever 

    forever start 'js file name'

To stop in forever 

    forever stop ''js file name'

To restart in forever   

    forever restart 'js filename'

List the server ruuning using forever 

    forever list

