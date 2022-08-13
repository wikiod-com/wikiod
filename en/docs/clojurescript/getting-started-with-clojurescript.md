---
title: "Getting started with clojurescript"
slug: "getting-started-with-clojurescript"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
# Leiningen

**Note:** If you're going to use Leiningen, you first need to download and install JDK 6 or newer.

The easiest way to get started with Clojure is to download and install Leiningen, the de facto standard tool to manage Clojure projects.

## Linux:

```sh
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
export PATH=$PATH:~/bin
chmod 755 ~/bin/lein
lein
```

## OS X:
Follow Linux steps above or

Install with [Homebrew]:

```sh
brew install leiningen
```

## Windows:

See <https://github.com/technomancy/leiningen#installation>.

## Connecting to a REPL

Once you have `lein` installed, execute

```sh
lein repl
```

## Start a new ClojureScript Project


    lein new mies myproject
    cd myproject
    ./scripts/build

Now open `index.html` in a Web Browser. 

Press F12

Look at the Console, and observe the following: 

    Hello world!

# Acknowledgements

  * David Nolen's extraordinary work on ClojureScript. 





