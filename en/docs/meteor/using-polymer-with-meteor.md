---
title: "Using Polymer with Meteor"
slug: "using-polymer-with-meteor"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Using differential:vulcanize
In the root of your project, make sure Bower is installed (`npm install -g bower`) and run `bower init`. This will create a `bower.json` file in your project's directory.

Create a new file called `.bowerrc` to your root directory. It should contain the following:
```
{
  "directory": "public/bower_components"
}
```
This lets Bower know that it should save components in the `bower_components` folder in your app's public directory.

Now add the Polymer components you wish to use with your app.

In your app's root directory bower-install each component you want to use.

```
bower install --save PolymerElements/paper-button#^1.0.0 PolymerElements/paper-checkbox#^1.0.0
```

Add [Vulcanize][1] to your project

    Meteor add differential:vulcanize

Create a new file called config.vulcanize in the root of your project. It should contain the following:

```
{
    "polyfill": "/bower_components/webcomponentsjs/webcomponents.min.js",
    "useShadowDom": true, // optional, defaults to shady dom (polymer default)
    "imports": [
        "/bower_components/paper-button/paper-button.html",
        "/bower_components/paper-checkbox/paper-checkbox.html"
    ]
}
```

`"imports"` should list each component you will use in your app.

You can now use components you have imported in your Blaze templates just as you would any other element:

```
<template name="example">
    <div>
        this is a material design button: <paper-button></paper-button>
        this is a material design checkbox: <paper-checkbox></paper-checkbox>
    </div>
</template>
```

  [1]: https://github.com/Differential/meteor-vulcanize

