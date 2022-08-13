---
title: "Getting started with coffeescript"
slug: "getting-started-with-coffeescript"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello Word (Linux and OS X)
CoffeeScript is a scripting language that compiles into JavaScript. Any code written in CoffeeScript can be translated into JavaScript with a one-to-one matching.

CoffeeScript can be easily installed with `npm`:

```
$ mkdir coffee && cd coffee
$ npm install -g coffee-script
```

The `-g` flag will install CoffeeScript globally, so it will always be available on your CLI. Don't use the `-g` flag if you want a local installation:

```
$ mkdir coffee && cd coffee
$ npm install coffee-script
```

When the package is installed, create a `helloword.coffee` file in the working directory and write some CoffeeScript code in it.

```
console.log 'Hello word!'
```

This code can be executed by calling the CoffeeScript binary. If you installed CoffeeScript globally, simply run:

```
$ coffee helloword.coffee
```

If you installed CoffeeScript locally, you will find the binary in the installation folder:

```
$ ./node_modules/coffee-script/bin/coffee helloword.coffee
```

In both cases,
the result will be printed in the console: `Hello word!`

