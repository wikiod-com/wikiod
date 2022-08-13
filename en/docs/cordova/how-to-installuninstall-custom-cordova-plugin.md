---
title: "How to installuninstall custom cordova plugin"
slug: "how-to-installuninstall-custom-cordova-plugin"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Use plugman command to install/uninstall cordova plugin
You can use plugman command to install/uninstall custom cordova plugins.

To install plugman

```s
 npm install -g plugman
```
Install plugin command syntax:
```
plugman <install|uninstall> --platform <ios|android|blackberry10|wp8> --project <directory> --plugin <name|url|path>
```

Example:

```
plugman install --platform ios --project platforms/ios/ --plugin plugins/cordova-plugin-test/
```
For more plugman options check [this link][1]


  [1]: https://cordova.apache.org/docs/en/latest/plugin_ref/plugman.html 

