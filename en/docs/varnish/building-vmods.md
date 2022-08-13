---
title: "Building vmods"
slug: "building-vmods"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

You don't necessarily have to compile vmods if the binaries for them are already available for your platform. Both CentOS 6 and 7 can leverage COPR builds by Ingvar in order to install a collection of extra modules by Varnish Software:
 https://copr.fedorainfracloud.org/coprs/ingvar/varnish51/

## Compile and install a vmod
Installation of a vmod requires an installed version of Varnish Cache, including the development files. Requirements can be found in the Varnish documentation.

Source code is built with autotools:

    sudo apt-get install libvarnishapi-dev || sudo yum install varnish-libs-devel
    ./bootstrap   # If running from git.
    ./configure
    make
    make check   # optional
    sudo make install

