---
title: "SCons run phases"
slug: "scons-run-phases"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

SCons is a multi-step build system. First it reads all `SConstruct` and `SConscript` to execute Python code and create `build graph` with targets. Then it scans filesystem to detect which targets from the `build graph` should be updated, and after that it executes command to build outdated targets.

## Inspecting SCons phases
`scons` describes running phases itself. Running it over an empty `SConstruct` yields this:

    $ scons
    scons: Reading SConscript files ...
    scons: done reading SConscript files.
    scons: Building targets ...
    scons: `.' is up to date.
    scons: done building targets.

To suppress phase messages, add `-Q` option. `--tree=all` allows to see dependency tree for current target that `scons` constructed while building.

    $ scons -Q --tree=all
    scons: `.' is up to date.
    +-.
      +-SConstruct

`.` is default target, which means "build SConstruct in current directory". `SConstruct` is then a dependency for building the default target.

