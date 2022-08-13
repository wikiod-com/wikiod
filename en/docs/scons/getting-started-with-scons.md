---
title: "Getting started with SCons"
slug: "getting-started-with-scons"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting Started
Once you have [SCons running](https://www.wikiod.com/scons/getting-scons-running), create a file named `SConstruct`:

    print('..Building World')

Now run `scons`:

    $ scons
    scons: Reading SConscript files ...
    ..Building World
    scons: done reading SConscript files.
    scons: Building targets ...
    scons: `.' is up to date.
    scons: done building targets.

`SConstruct` is a Python script with additional SCons functions. 

    Zip('archive', ['SConstruct'])

The script above packs itself into `archive.zip` using `Zip()` function provided by SCons. `Zip` is a **Builder** - it builds **target** specified by first argument from multiple **sources**, which come as second argument to Builders by convention.

SCons **Builders** start with uppercase letter and operate on **Environment** object, which stores build configuration. SCons provides default **Environment**, but it can be created explicitly to separate build variables, choose different tools, etc.

    env = Environment()
    env.Zip('archive', ['SConstruct'])

Note that when you run the script for the second time, it doesn't build anything. SCons rebuilds targets only when source files change. Modify `SConstruct` and run `scons` again to see the difference.

SCons is designed to be extensible. You add your own Builder methods by attaching them to the Environment, which can be covered in later topics. 


