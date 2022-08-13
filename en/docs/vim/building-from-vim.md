---
title: "Building from vim"
slug: "building-from-vim"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

## Starting a Build
`:mak[e][!] [arguments]` will start the program referred to by the `makeprg` option. By default, `makeprg` is set to "make," but can be configured to invoke any appropriate program.

All `[arguments]` (can be several) are passed to `makeprg` just as if it had been invoked with `:!{makeprg} [arguments]`.

The output of the invoked program is parsed for errors according to the `'errorformat'` option. If any errors are found, the quickfix window is opened to display them.

`:cnext` `:cprev` can be used to cycle between errors displayed in the quickfix window. `:cc` will jump to the error under the cursor.

It should be noted that on systems where gnumake is installed and properly configured, there is generally no need to define `&makeprg` to anything but its default value to compile mono-file projects. Thus, in C, C++, Fortran... just type `:make %<` to compile the current file. According the source file is in the current directory, `:!./%<` will execute it. Compilation options can be controlled through `$CFLAGS`, `$CXXFLAGS`, `$LDFLAGS`, etc. Consult the documentation of `make` regarding _implicit rules_.

