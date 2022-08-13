---
title: "Getting started with semantic-ui"
slug: "getting-started-with-semantic-ui"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation via NodeJS - Recommended
Installation via NodeJS is the recommended method. It is preferred because you can use it to build the files selecting just the components you want. 

**Step 1**: Install Node ([Link][1])

**Step 2**: Install Gulp globally ( -g ) on your computer

    npm install -g gulp

> Semantic UI uses Gulp to provide command line tools for building
> themed versions of the library with just the components you need.

**Step 3**:  Install Semantic-UI locally for your project

    cd /path/to/your/project

    npm install semantic-ui --save

**Step 4**: Navigate to the folder where semantic-ui was saved and run the following gulp task (This may be automatically done)

    cd node_modules/semantic-ui    
    gulp install

This will launch the interactive installer. Just follow the instructions to select the SUI elements that you want and it will build the relevant files for you. 

- You will first have to choose the type of installation.First time users can choose the *Automatic Mode* to build SUI
- Then you will have to specify the project folder and where you would like to save SUI (default is `semantic/`) 
- In case of Custom installation, you will get the option to pick the components you want.

**Step 5**: Link to your HTML

Link the compiled CSS and JavaScript file in your HTML

    <link rel="stylesheet" type="text/css" href="semantic/dist/semantic.min.css">
    <script src="semantic/dist/semantic.min.js"></script>

All done!


  [1]: https://nodejs.org/

