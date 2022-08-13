---
title: "Aurelia CLI"
slug: "aurelia-cli"
draft: false
images: []
weight: 9865
type: docs
toc: true
---

## Adding Bootstrap To A CLI Application
A commonly used CSS/Javascript library is [Bootstrap](http://getbootstrap.com/). To install it into your Aurelia CLI driven application first you need to install it using Npm.

```
npm install bootstrap --save
```

Because Bootstrap has a hard dependency on jQuery, we need to make sure we also have jQuery installed:
```
npm install jquery --save
```

Now in your preferred IDE/code editor open up the following file in your project directory: ``aurelia_project/aurelia.json`` and scroll down to the `build.bundles` section towards the end of the file. We will want to add Bootstrap to one of the bundles. For this example, we will be adding both jQuery and Bootstrap into the vendor bundle.

```
"jquery",
{
  "name": "bootstrap",
  "path": "../node_modules/bootstrap/dist",
  "main": "js/bootstrap.min",
  "deps": ["jquery"],
  "exports": "$",
  "resources": [
    "css/bootstrap.css"
  ]
}
```

This will make Bootstrap accessible in your application and importable via `import 'bootstrap'` in your code (this is the `name` property defined above). Notice the reference to `jquery` in the `"deps": []` section of our bootstrap definition. We also specify that we want to bundle Bootstrap's CSS in our main `vendor-bundle.js` file using the `"resources":[]` property

Last and not least, to use our newly added Bootstrap dependency, we want to first import the Bootstrap library inside of our `main.js` file by putting the following at the beginning of the file.

```
import 'bootstrap';
```

We want to include the Bootstrap CSS from a place where it will be globally accessible to the whole application, so inside of `app.html` put the following between the `<template></template>` tags.

```
<require from="bootstrap/css/bootstrap.css"></require>
```

## Adding Lodash To A CLI Application
A commonly used utility library is [Lodash](https://lodash.com/). To install it into your Aurelia CLI driven application first you need to install it using Npm.

`npm install lodash --save`

Now in your preferred IDE/code editor open up the following file in your project directory: aurelia_project/aurelia.json and scroll down to the build.bundles section towards the end of the file. We will want to add Lodash to one of the bundles. For this example, we will be adding Lodash into the vendor bundle.

Because Lodash does follow the CommonJS conventions and exports one concatenated file we can easily register it by adding the following object ot the dependencies array:

```
{
  "name": "lodash",
  "path": "../node_modules/lodash/lodash.min"
}
```

> Note that we omited the .js file suffix as those will be automatically appended when resolving the dependency.

Now we can use Lodash by simply importing it, using the ES6 import syntax:

`import _ from 'lodash';`

## Installing The Aurelia-I18N Plugin
In order to get the official [I18N Plugin](https://github.com/aurelia/i18n) into your CLI Project we need to install it by following the next steps.

First you want to install the plugin via npm:

`npm install aurelia-i18n`

Since Aurelia-I18N is backed by i18next, you should install it and a backend plugin of your choice. As an example we're going to leverage the i18next-xhr-backend:

`npm install i18next i18next-xhr-backend`

After that we need to tell our CLI App about the new dependencies. To do so we're going to open the file *aurelia_project/aurelia.json* and scroll down to section named *dependencies*. In there add the following three entries:

```
{
  "name": "i18next",
  "path": "../node_modules/i18next/dist/umd",
  "main": "i18next"
},
{
  "name": "aurelia-i18n",
  "path": "../node_modules/aurelia-i18n/dist/amd",
  "main": "aurelia-i18n"
},
{
  "name": "i18next-xhr-backend",
  "path": "../node_modules/i18next-xhr-backend/dist/umd",
  "main": "i18nextXHRBackend"
}
```

Great, now following the official [Aurelia-I18N Guide](https://github.com/aurelia/i18n#how-to-install-this-plugin) we create a folder in the root of your app named *locales*.

> You have to put the folder into the root (on same level as *src*) as this is the hosted root of your app

In there add subfolders for each language you'd like to support, eg *en* and *de* for English and German language.

Inside of each of those folders create a file named *translation.json* with your translation keys and values. Follow the official guide for detailed info.

Last but not least it's time to wire up the plugin inside your app. Therefore go to your *src/main.js* file and configure it as follows.

```
/**********************************************/
/          add the necessary imports           /
/**********************************************/
import environment from './environment';
import Backend from 'i18next-xhr-backend';

//Configure Bluebird Promises.
//Note: You may want to use environment-specific configuration.
Promise.config({
  warnings: {
    wForgottenReturn: false
  }
});

export function configure(aurelia) {
  aurelia.use
    .standardConfiguration()
    .feature('resources');

  if (environment.debug) {
    aurelia.use.developmentLogging();
  }

  if (environment.testing) {
    aurelia.use.plugin('aurelia-testing');
  }

  /**********************************************/
  /             register the plugin              /
  /**********************************************/
  aurelia.use.plugin('aurelia-i18n', (instance) => {
    // register backend plugin
    instance.i18next.use(Backend);

    // adapt options to your needs (see http://i18next.com/docs/options/)
    // make sure to return the promise of the setup method, in order to guarantee proper loading
    return instance.setup({
      backend: {                                  // <-- configure backend settings
        loadPath: './locales/{{lng}}/{{ns}}.json', // <-- XHR settings for where to get the files from
      },
      lng : 'de',
      attributes : ['t','i18n'],
      fallbackLng : 'en',
      debug : false
    });
  });

  aurelia.start().then(() => aurelia.setRoot());
}
```

## Adding Aurelia Configuration to a CLI Application
Adding the aurelia-configuration to a cli application sometimes produces a build error. This is caused by a missing dependency so we simply add the dependency to the build bundle.

Try the following:

<pre><code>
npm install deep-extend --save  
npm install aurelia-configuration --save
</code></pre>

Now add the following to the aurelia.json file in the dependencies array:

<pre><code>
{
    "name": "deep-extend",
    "path": "../node_modules/deep-extend/lib/deep-extend"
  },
  {
    "name": "aurelia-configuration",
    "path": "../node_modules/aurelia-configuration/dist/amd",
    "main": "index"
  }
</code></pre>

