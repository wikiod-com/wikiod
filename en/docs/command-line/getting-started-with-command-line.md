---
title: "Getting started with command-line"
slug: "getting-started-with-command-line"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing ZSH as default shell on macOS
The simplest way is to use `brew`:

    brew install zsh
After installation, you may want to set it as your default shell by doing:

    sudo echo '/usr/local/bin/zsh' >> /etc/shells
    chsh -s /usr/local/bin/zsh

If you have git, and required command line tools installed you can compile and install the latest version (`5.2` as of this edit) as follows:

    # clone the source
    git clone git://git.code.sf.net/p/zsh/code zsh
    
    # checkout the required version, say, zsh-5.2
    cd zsh && git checkout zsh-5.2

    # check the documentation for help on configuration options
    ./Util/preconfig
    ./configure --prefix=/usr/local \
      --enable-fndir=/usr/local/share/zsh/functions \
      --enable-scriptdir=/usr/local/share/zsh/scripts \
      --enable-site-fndir=/usr/local/share/zsh/site-functions \
      --enable-site-scriptdir=/usr/local/share/zsh/site-scripts \
      --enable-runhelpdir=/usr/local/share/zsh/help \
      --enable-etcdir=/etc \
      --mandir=/usr/local/share/man \
      --infodir=/usr/local/share/info \
      --enable-cap \
      --enable-maildir-support \
      --enable-multibyte \
      --enable-pcre \
      --enable-zsh-secure-free \
      --with-tcsetpgrp
   
    # compile and check if compiled successfully
    make -j5 && make check
    # you should see results of successful test scripts

    sudo make install

Again, you can make `zsh` as your default shell by adding it to `/etc/shells` and using `chsh` as described above.

## macOS using homebrew
    brew install zsh
    sudo echo '/usr/local/bin/zsh' >> /etc/shells
    chsh -s /usr/local/bin/zsh

## Hello world
**In Unix/Posix systems:**

    >$ echo "Hello World!"

This simple command will print Hello World on the terminal.

