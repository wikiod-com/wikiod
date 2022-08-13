---
title: "How to Compile Vim"
slug: "how-to-compile-vim"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Compiling on Ubuntu
To build vim from source on Ubuntu:

 1. Get a copy of the source code by downloading from the [official Vim repository on GitHub][1].
 1. Get the dependencies by running `$ sudo apt-get build-dep vim-gnome` or similar.
 1. Go to the directory of the Vim source code: `cd vim/src`
 1. Run `$ ./configure`. You can customize the build (and enable Perl, Python, etc. language integrations) by passing configuration options. See `src/INSTALL` for an overview.
 1. Run `$ make`.
 1. Finish the installation by running `$ sudo make install`. As your self-compiled Vim is not managed by the package manager, it will be placed in `/usr/local/bin/vim`, instead of `/usr/bin/vim`. So, to run it, you either need to specify the full path, or ensure that `/usr/local/bin` is before `/usr/bin` in your `PATH` (it usually is).
 1. (Optional) Remove the distribution-provided version of Vim if you had it installed already: `$ sudo apt-get remove vim vim-runtime gvim`.

To verify the installation, you can run `$ which vim` which should return something like `/usr/local/bin/vim` if the installation was successful.

  [1]: https://github.com/vim/vim


