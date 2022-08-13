---
title: "Cabal"
slug: "cabal"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
- cabal \<command> where \<command> is one of:
- **[global]**
  - update
    - Updates list of known packages
  - install
    - Install packages
  - help
    - Help about commands
  - info
    - Display detailed information about a particular package
  - list
    - List packages matching a search string
  - fetch
    - Downloads packages for later installation
  - user-config
    - Display and update the user's global cabal configuration
- **[package]**
  - get
    - Download/Extract a package's source code (repository)
  - init
    - Create a new .cabal package file (interactively)
  - configure
    - Prepare to build the package
  - build
    - Compile all/specific components
  - clean
    - Clean up after a build
  - run
    - Builds and runs an executable
  - repl
    - Open an interpreter session for the given component
  - test
    - Run all/specific tests in the test suite
  - bench
    - Run all/specific benchmarks
  - check
    - Check the package for common mistakes
  - sdist
    - Generate a source distribution file (.tar.gz)
  - upload
    - Uploads source packages or documentation to Hackage
  - report
    - Upload build reports to a remote server
  - freeze
    - Freeze dependencies
  - gen-bounds
    - Generate dependency bounds
  - haddock
    - Generate Haddock HTML documentation
  - hscolour
    - Generate HsColour colourised code, in HTML format
  - copy
    - Copy the files into the install locations
  - register
    - Register this package with the compiler
- **[sandbox]**
  - sandbox
    - Create/modify/delete a sandbox
      - cabal sandbox init          [FLAGS]
      - cabal sandbox delete        [FLAGS]
      - cabal sandbox add-source    [FLAGS] PATHS
      - cabal sandbox delete-source [FLAGS] PATHS
      - cabal sandbox list-sources  [FLAGS]
      - cabal sandbox hc-pkg        [FLAGS] [--] COMMAND [--] [ARGS]
  - exec
    - Give a command access to the sandbox package repository
  - repl
    - Open interpreter with access to sandbox packages

## Working with sandboxes
A Haskell project can either use the system wide packages or use a sandbox. A sandbox is an isolated package database and can prevent dependency conflicts, e. g. if multiple Haskell projects use different versions of a package.

To initialize a sandbox for a Haskell package go to its directory and run:

    cabal sandbox init

Now packages can be installed by simply running `cabal install`.

Listing packages in a sandbox:

    cabal sandbox hc-pkg list

Deleting a sandbox:

    cabal sandbox delete

Add local dependency:

    cabal sandbox add-source /path/to/dependency

## Install packages
To install a new package, e.g. aeson:

    cabal install aeson

