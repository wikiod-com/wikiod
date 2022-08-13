---
title: "How to update Ember, Ember Data and Ember CLI"
slug: "how-to-update-ember-ember-data-and-ember-cli"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

 - To find the latest stable version of **Ember**, [click here][1].
 - To find the latest stable version of **Ember Data**, [click here][2].
 - To find the latest stable version of **Ember CLI**, [click here][3].

All these steps were found on [Ember cli release note][4].


  [1]: https://github.com/emberjs/ember.js/releases/latest
  [2]: https://github.com/emberjs/data/releases/latest
  [3]: https://github.com/ember-cli/ember-cli/releases/latest
  [4]: https://github.com/ember-cli/ember-cli/releases

## Updating Ember CLI
Ember CLI is a normal npm package. To update it we have to uninstall it and then install the version we want.

As of writing this post the latest version is 2.13.2. From the command line run:

    npm uninstall -g ember-cli
    npm cache clean
    bower cache clean
    npm install -g ember-cli@2.13.2

To verify the proper version was installed run:

    ember -v

Then update your project. This will clear out the cache and update the Ember CLI version in `package.json`. The last part will run the new project blueprint on your projects directory. Just follow the prompts and review all the changes.

    rm -rf node_modules bower_components dist tmp
    npm install --save-dev ember-cli@2.13.2
    npm install
    bower install
    ember init

references 

## Updating Ember
In this example, `2.13.2` is the latest version. We install it via `bower`, specifying the particular version as `ember#2.13.2` and including the save flag to persist it to bower.json.

As of writing this post the latest version is `2.13.2`. From the command line, in the root of your app's directory, run:

    bower install ember#2.13.2 --save

You may be prompted to choose your version of Ember. If you are, prepend your answer with a `!` to make sure it's saved.


## Updating Ember Data
Since Ember Data is an Ember CLI add-on we can install it just like any other add-on by using `ember install`.

As of writing this post the latest version is `2.13.1`. From the command line, in the root of your app's directory, run:

    ember install ember-data@2.13.1


