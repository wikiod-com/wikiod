---
title: "ES2015 modules (Import & Export)"
slug: "es2015-modules-import--export"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

MDN documentation for imports: https://developer.mozilla.org/en/docs/web/javascript/reference/statements/import
MDN documentation for exports: https://developer.mozilla.org/en/docs/web/javascript/reference/statements/export
ExploringJS chapter on modules: http://exploringjs.com/es6/ch_modules.html

## Importing in app modules
Node modules

    import url from 'url';
    import moment from 'moment';

Meteor packages

    import { Meteor } from 'meteor/meteor';
    import { SimpleSchema } from 'meteor/aldeed:simple-schema';

## Importing in Meteor packages
In package.js:
<!-- language:js -->
    Npm.depends({
      moment: "2.8.3"
    });

In a package file:

<!-- language: js -->

    import moment from 'moment';

## Exporting variables from app modules
```
// Default export
export default {};

// Named export
export const SomeVariable = {};
```

## Exporting symbols from Meteor packages
In your [mainModule][1] file:

```
export const SomeVar = {};
```


  [1]: http://docs.meteor.com/#/full/pack_mainModule

