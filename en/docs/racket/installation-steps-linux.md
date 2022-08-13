---
title: "Installation steps (Linux)"
slug: "installation-steps-linux"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Installation or Setup
Visit https://download.racket-lang.org and choose between the two available distributions:
- `Racket` is the main distribution, it comes with several additional packages like [math/number-theory][1] as well as the DrRacket IDE.
- `Minimal Racket` is far smaller and comes only with the needed packages.

Installation steps for Linux:
=============================

The installation is very simple. If you are used to this kind of thing, just follow these four steps. A more detailed step-by-step walkthrough is detailed afterwards, if you prefer.

 1. download it from https://download.racket-lang.org
 2. `chmod +x racket-6.6-x86_64-linux.sh`
 3. `./racket-6.6-x86_64-linux.sh`
 4. Answer the questions, and possibly update your `$PATH`.

For a more detailed step-by-step guide, see below.

Downloading
-----------

 1. Go to https://download.racket-lang.org .
 2. Select <kbd>Platform: Linux i386</kbd> if you have a 32-bit system, or <kbd>Platform: Linux x86_64</kbd>.
 3. Click the download button labeled <kbd>racket-6.9-x86_64-linux.sh (113M)</kbd> (the label may be slightly different depending on the version).

Starting the installer
----------------------

 4. Open a terminal.
 5. If you downloaded the file to the `/home/YOUR_USER_NAME/Downloads`, type the following command:

  <kbd>cd /home/YOUR_USER_NAME/Downloads</kbd>

  Be sure to replace `YOUR_USER_NAME` by your actual user name and `/Downloads` by the actual path to the folder to which you downloaded Racket.
 6. Type <kbd>chmod +x racket-6.6-x86_64-linux.sh</kbd> (change the version number and the `x86_64` to match the file you downloaded).
 7. If you want to install Racket system-wide, type <kbd>sudo ./racket-6.6-x86_64-linux.sh</kbd> (change the version number and the `x86_64` to match the file you downloaded).

  Otherwise, if you are not an administrator on the computer, simply type <kbd>./racket-6.6-x86_64-linux.sh</kbd> to install it in your own home directory (change the version number and the `x86_64` to match the file you downloaded).

Installing
----------

The installer will ask the following questions:

 8. <samp>`Do you want a Unix-style distribution?`</samp>

  Answer <kbd>no</kbd> (the default).

 9. <samp>`Where do you want to install the "racket-6.6.0.4" directory tree?`</samp>

   Select `/usr/racket` (type <kbd>1</kbd> <kbd>Enter ⏎</kbd>) or `/usr/local/racket` (type <kbd>2</kbd> <kbd>Enter ⏎</kbd>) if you are installing Racket system-wide. Otherwise, to install it in your own home directory (e.g. if you are not an administrator), select `~/racket (/home/YOUR_USER_NAME/racket)` (type <kbd>3</kbd> <kbd>Enter ⏎</kbd>).

 10. <samp>`If you want to install new system links within the "bin", "man" and "share/applications" subdirectories…`</samp>

   If you are doing a system-wide installation it is a good idea to type <kbd>/usr/local</kbd> or <kbd>/usr</kbd> here (to know which, check which one is present in your `PATH`, by typing <kbd>echo $PATH</kbd> in another terminal window). If you are installing it in your own home directory, leave the answer empty and just press <kbd>Enter ⏎</kbd>.

Starting DrRacket
-----------------

Depending on your answer to steps 9 and 10, you need to type one of the following commands in a terminal to start DrRacket:

* <kbd>drracket</kbd> (if step 10 was successful)
* <kbd>/usr/racket/bin/drracket</kbd>
* <kbd>/usr/local/racket/bin/drracket</kbd>
* <kbd>/home/YOUR_USER_NAME/racket/bin/drracket</kbd> (replace `YOUR_USER_NAME` by your actual username, or simply type <kbd>~/racket/bin/drracket</kbd>)

To avoid typing such a long command each time, you can add the following command to the file `~/.bashrc`, where `/path/to/the/containing/folder/` should be one of `/usr/racket/bin/`, `/usr/local/racket/bin/` or `/home/YOUR_USER_NAME/racket/bin/`:

    export PATH="/path/to/the/containing/folder/:$PATH"

Running your first program
--------------------------

To run a program, open DrRacket as explained above, enter the program starting with `#lang racket`, and click the `Run` button near the top-right corner. Here is a first example program:

```
#lang racket
(displayln "Hello Racket!")
```


  [1]: http://docs.racket-lang.org/math/number-theory.html

