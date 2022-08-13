---
title: "Installation steps (Windows)"
slug: "installation-steps-windows"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Installation or Setup
Visit https://download.racket-lang.org and choose between the two available distributions:
- `Racket` is the main distribution, it comes with several additional packages like [math/number-theory][1] as well as the DrRacket IDE.
- `Minimal Racket` is far smaller and comes only with the needed packages.

  [1]: https://docs.racket-lang.org/math/number-theory.html

To run a program, open DrRacket, enter the program starting with `#lang racket`, and click the `Run` button near the top-right corner.

Installation steps for Windows:
===============================

The installation is very simple. If you are used to this kind of thing, just go to https://download.racket-lang.org, then download and run the installer. A more detailed step-by-step walkthrough is detailed afterwards, if you prefer.

Downloading
-----------

 1. Go to https://download.racket-lang.org .
 2. Select <kbd>Platform: Windows (x86, 32-bit)</kbd> if you have a 32-bit system, or <kbd>Platform: Windows (x64, 64-bit)</kbd> if you have a 64-bit system. If in doubt, choose the 32-bit version.
 3. Click the download button labeled <kbd>racket-6.9-i386-win32.exe (73M)</kbd> (the label may be slightly different depending on the version).

Starting the installer
----------------------

 4. Open the directory where the file was downloaded, and double-click on the `racket-….exe` file.
 5. Follow the installer's instructions.

Setting up command-line tools
-----------------------------

To set up the command-line tools, open DrRacket, click the Help menu, and click "Configure Command Line for Racket." This will install the [`racket`][command: racket] and [`raco`][command: raco] commands. (On Windows, the [`racket`][command: racket] command is `Racket.exe`).

  [command: racket]: http://docs.racket-lang.org/reference/running-sa.html
  [command: raco]: http://docs.racket-lang.org/raco/index.html

Running your first program
--------------------------

To run a program, open DrRacket, enter the program starting with `#lang racket`, and click the `Run` button near the top-right corner. Here is a first example program:

```
#lang racket
(displayln "Hello Racket!")
```


