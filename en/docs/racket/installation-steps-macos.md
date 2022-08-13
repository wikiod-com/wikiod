---
title: "Installation steps (macOS)"
slug: "installation-steps-macos"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Installation or Setup
Visit https://download.racket-lang.org and choose between the two available distributions:
- `Racket` is the main distribution, it comes with several additional packages like [math/number-theory][1] as well as the DrRacket IDE.
- `Minimal Racket` is far smaller and comes only with the needed packages.

Installation steps for macOS:
=============================

The installation is very simple. If you are used to this kind of thing, just go to https://download.racket-lang.org, then download and install the `.dmg` file. A more detailed step-by-step walkthrough is detailed afterwards, if you prefer.

Downloading
-----------

 1. Go to https://download.racket-lang.org .
 2. Select <kbd>Platform: Mac OS (Intel 32-bit)</kbd> if you have a 32-bit system, or <kbd>Platform: Mac OS (Intel 64-bit)</kbd> if you have a 64-bit system.
 3. Click the download button labeled <kbd>racket-6.9-x86_64-macosx.dmg (106M)</kbd> (the label may be slightly different depending on the version).

Starting the installer
----------------------

 4. `FIXME: If you have macOS, please fill in this section`

Setting up command-line tools
-----------------------------

On **Mac OS X**, you can visit the [Help menu][2] of DrRacket and use "Configure Command Line for Racket..." to set up racket tools for command line use. On **Windows** you will need to add the Racket installation folder to your `PATH` variable.


  [1]: https://docs.racket-lang.org/math/number-theory.html
  [2]: https://docs.racket-lang.org/drracket/Menus.html#%28part._.Help%29

Running your first program
--------------------------

To run a program, open DrRacket, enter the program starting with `#lang racket`, and click the `Run` button near the top-right corner. Here is a first example program:

```
#lang racket
(displayln "Hello Racket!")
```


