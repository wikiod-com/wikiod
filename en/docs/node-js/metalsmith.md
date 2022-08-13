---
title: "metalsmith"
slug: "metalsmith"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Build a simple blog
Assuming that you have node and npm installed and available, create a project folder with a valid `package.json`. Install the necessary dependencies:

    npm install --save-dev metalsmith metalsmith-in-place handlebars

Create a file called `build.js` at the root of your project folder, containing the following:

    var metalsmith = require('metalsmith');
    var handlebars = require('handlebars');
    var inPlace = require('metalsmith-in-place');

    Metalsmith(__dirname)
      .use(inPlace('handlebars'))
      .build(function(err) {
        if (err) throw err;
        console.log('Build finished!');
      });

Create a folder called `src` at the root of your project folder. Create `index.html` in `src`, containing the following:

    ---
    title: My awesome blog
    ---
    <h1>{{ title }}</h1>

Running `node build.js` will now build all files in `src`. After running this command, you'll have `index.html` in your build folder, with the following contents:

    <h1>My awesome blog</h1>


