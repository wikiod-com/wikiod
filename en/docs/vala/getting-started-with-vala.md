---
title: "Getting started with vala"
slug: "getting-started-with-vala"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello world!
In `foo.vala`:

<!-- language: lang-vala -->

    void main (string[] args) {
        stdout.printf ("Hello world!");
    }

To compile the source into the `foo` binary:

```
valac foo.vala
```

To compile and run the source:

```
vala foo.vala
```

## Installation or Setup
The easiest way of installing Vala is to install your distribution-specific package.

On Ubuntu:

```bash
sudo apt install valac
```

On Fedora:

```
sudo dnf install vala
```

On Arch:

```
sudo pacman -S vala
```

On OS X, with Homebrew:

```
brew install vala
```

On Windows, you can get an installer for the latest version [here][1].

You can also build it from sources, but you'll need to install `pkg-config`, a C compiler, a standard C library and GLib 2 before:

```
wget https://download.gnome.org/sources/vala/0.34/vala-0.34.4.tar.xz
tar xvf vala-0.34.4.tar.xz
cd vala-0.34.4
./configure
make
sudo make install
```


  [1]: https://sourceforge.net/projects/valatowindows/

