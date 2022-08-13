---
title: "Getting started with smalltalk"
slug: "getting-started-with-smalltalk"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
The name Smalltalk usually refers to ANSI Smalltalk or Smalltalk 80 (of which the first is based on). While most implementations are close to the standard, they vary in different aspects (usually referred to as _dialects_).

Each implementation has it's own method of installation.

Well known FOSS implementations are:
==

[Pharo](http://pharo.org/) Started as a Squeak fork. (Windows/Linux/Mac OSX). Pharo has its own documentation entry at Stackoverflow Documentation, so please take a look [there][1]

[Squeak](http://squeak.org/) (Windows/Linux/Mac OSX)

[GNU Smalltalk](http://smalltalk.gnu.org/) (Windows/Linux/Mac OSX)

[Dolphin Smalltalk](http://www.object-arts.com/) Originally commercial, now free open source. (Windows only)

[Cuis Smalltalk](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev) A Squeak fork with a focus on reducing system complexity.

Commercial Smalltalks include:
==

[VisualWorks/Cincom Smalltalk](http://www.cincomsmalltalk.com/main/) Free trial available.

[VisualAge Smalltalk](http://www.instantiations.com/products/vasmalltalk/index.html) Originally by IBM, now Instatiations. Free trial available

[Smalltalk/x](https://www.exept.de/en/smalltalk-x.html) (Free for personal use?)

[GemStone/s](https://gemtalksystems.com/products/gs64/) Free community edition available.

Other Smalltalk dialects
==

[Amber Smalltalk](http://www.amber-lang.net/) A Smalltalk that lives in the browser.

[Redline Smalltalk](http://www.redline.st/) Smalltalk for the JVM.

[List of Smalltalk implementations on world.st](http://www.world.st/try/implementations)


  [1]: https://www.wikiod.com/pharo/getting-started-with-pharo

## Hello World in Smalltalk
    Transcript show: 'Hello World!'.
This will print `Hello World!` to the Transcript window in Smalltalk. `Transcript` is the class that allows you to print to the Transcript window by sending the message `show:` to that object. The colon indicates that this message requires a parameter which is in this case a string. Strings are represented by single quotes and single quotes only since double quotes are reserved for comments in Smalltalk.

