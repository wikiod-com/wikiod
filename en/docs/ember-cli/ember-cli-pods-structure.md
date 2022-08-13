---
title: "Ember-cli Pods structure"
slug: "ember-cli-pods-structure"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
   - Ember g [blueprints.eg: route] [name] --pod
   - Ember g route foo --pod
   - Ember g component my-name --pod


## Parameters
| Generate | pods |
| ------ | ------ |
| g   | --pod   |

Just pass `--pod` to `ember generate` when generating new files.

If you would like to use the pods structure as the default for your project, you can set usePods in your `.ember-cli` config file to true (setting was previously named `usePodsByDefault`). To generate or destroy a blueprint in the classic type structure while `usePods` is `true`, use the `--classic` flag.

With the usePods set to true.

    // .ember-cli
    {
        "usePods": true
    }

The following would occur when generating a route:

    ember generate route taco
    
    installing
      create app/taco/route.js
      create app/taco/template.hbs
    installing
      create tests/unit/taco/route-test.js
    
    ember generate route taco --classic
    
    installing
      create app/routes/taco.js
      create app/templates/taco.hbs
    installing
      create tests/unit/routes/taco-test.js


There are some benefits to use this method, however, it's completely up to you.Firstly, it separates your application into more logical groupings, thus, you can keep your files neatly organized into resources.

This structure also makes our development's life easier. For instance, if I want to find the `myname` `controller` in the default structure, I need to preface what I actually want (myname) with the type (controllers). However, with pods, I can fuzzy-find the same controller by simply looking up “myname.”

## Organize with Pods
    app/controllers/myname.js
    app/templates/myname.hbs
    app/routes/myname.js
    app/models/myname.js

Using pods, the example above would translate into this:

    app/myname/controller.js
    app/myname/template.hbs
    app/myname/route.js
    app/myname/model.js

## podModulePrefix: app/pods
    ember generate route foo --pod
    
    installing
      create app/pods/foo/route.js
      create app/pods/foo/template.hbs
    installing
      create tests/unit/pods/foo/route-test.js

