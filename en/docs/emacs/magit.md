---
title: "Magit"
slug: "magit"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Magit is an interface to the version control system Git, implemented as an Emacs package.
It allows you to interact with git in Emacs.

Magit is an interface to the version control system Git,
implemented as an Emacs package.  Magit aspires to be a complete
Git porcelain.  While we cannot (yet) claim, that Magit wraps and
improves upon each and every Git command, it is complete enough to
allow even experienced Git users to perform almost all of their
daily version control tasks directly from within Emacs.  While many
fine Git clients exist, only Magit and Git itself deserve to be
called porcelains.

Note that Magit can interface itself to Github (with [Magithub](https://github.com/vermiculus/magithub/), see also [Github integration in Emacs](http://wikemacs.org/wiki/Github)) and that Emacs also has packages to work with [Gitlab](http://wikemacs.org/wiki/Gitlab), Bitbucket and others.

## Installation
You can install Magit from MELPA with:

    M-x package-install RET magit RET


## Basic usage: commit unstaged edits within an existing repo
    M-x magit-status
    s RET <file-to-stage> RET
    c c <commit message>
    C-c C-c
    q


