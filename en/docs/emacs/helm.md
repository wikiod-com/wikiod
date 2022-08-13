---
title: "Helm"
slug: "helm"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Installing helm via MELPA
From emacs 24.4 package.el is avalable, and one way to install helm is to do it via MELPA.  First, add the MELPA repository as package archive by putting following code somewhere in your `~/.emacs` (or, `~/.emacs.d/init.el`).

    (require 'package)
    
    ;; add the repository before the package-initialize.
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
    
    (package-initialize)

Next, enter <kbd>M-x list-packages</kbd> to see the avaiable package list.  Search for `helm` entry, put your cursor on the `helm` entry, press <kbd>RET</kbd>.  You'll see the package information buffer.  Put your cursor on `[Install]`, and press <kbd>RET</kbd>.  Helm will be installed.  The package list window and the package information window is shown in following image.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/GUEEN.png

