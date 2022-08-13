---
title: "Packages"
slug: "packages"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
- Pkg.add(package)
- Pkg.checkout(package, branch="master")
- Pkg.clone(url)
- Pkg.dir(package)
- Pkg.pin(package, version)
- Pkg.rm(package)

## Parameters
| Parameter | Details |
| --------- | -------- |
| `Pkg.add(`_`package`_`)` | Download and install the given registered package. |
| `Pkg.checkout(`_`package`_`, `_`branch`_`)` | Check out the given branch for the given registered package. _`branch`_ is optional and defaults to `"master"`. |
| `Pkg.clone(`_`url`_`)`  | Clone the Git repository at the given URL as a package. |
| `Pkg.dir(`_`package`_`)` | Get the location on disk for the given package. |
| `Pkg.pin(`_`package`_`, `_`version`_`)` | Force the package to remain at the given version. _`version`_ is optional and defaults to the current version of the package. |
| `Pkg.rm(`_`package`_`)` | Remove the given package from the list of required packages. |

## Install, use, and remove a registered package
After finding an official Julia package, it is straightforward to download and install the package. Firstly, it's recommended to refresh the local copy of METADATA:

    julia> Pkg.update()

This will ensure that you get the latest versions of all packages.

Suppose that the package we want to install is named [`Currencies.jl`][1]. The command to run to install this package would be:

    julia> Pkg.add("Currencies")

This command will install not only the package itself, but also all of its dependencies.

If the installation is successful, you can [test that the package works properly][2]:

    julia> Pkg.test("Currencies")

Then, to use the package, use

    julia> using Currencies

and proceed as described by the package's documentation, usually linked to or included from its README.md file.

To uninstall a package that is no longer needed, use the `Pkg.rm` function:

    julia> Pkg.rm("Currencies")

Note that this may not actually remove the package directory; instead it will merely mark the package as no longer required. Often, this is perfectly fine — it will save time in case you need the package again in the future. But if necessary, to remove the package physically, call the `rm` function, then call `Pkg.resolve`:

    julia> rm(Pkg.dir("Currencies"); recursive=true)

    julia> Pkg.resolve()


  [1]: https://github.com/JuliaFinance/Currencies.jl
  [2]: https://www.wikiod.com/julia-lang/unit-testing#Testing a Package

## Check out a different branch or version
Sometimes, the latest tagged version of a package is buggy or is missing some required features. Advanced users may wish to update to the latest development version of a package (sometimes referred to as the "master", named after the usual name for a development [branch][1] in Git). The benefits of this include:

- Developers contributing to a package should contribute to the latest development version.
- The latest development version may have useful features, bugfixes, or performance enhancements.
- Users reporting a bug may wish to check if a bug occurs on the latest development version.

However, there are many drawbacks to running the latest development version:

- The latest development version may be poorly-tested and have serious bugs.
- The latest development version can change frequently, breaking your code.

To check out the latest development branch of a package named [`JSON.jl`][2], for example, use

    Pkg.checkout("JSON")

To check out a different branch or tag (not named "master"), use

    Pkg.checkout("JSON", "v0.6.0")

However, if the tag represents a version, it's usually better to use

    Pkg.pin("JSON", v"0.6.0")

Note that a version literal is used here, not a plain string. The `Pkg.pin` version informs the package manager of the version constraint, allowing the package manager to offer feedback on what problems it might cause.

To return to the latest tagged version,

    Pkg.free("JSON")

  [1]: https://www.wikiod.com/git/tortoisegit#Branching
  [2]: https://github.com/JuliaLang/JSON.jl

## Install an unregistered package
Some experimental packages are not included in the METADATA package repository. These packages can be installed by directly cloning their Git repositories. Note that there may be dependencies of unregistered packages that are themselves unregistered; those dependencies cannot be resolved by the package manager and must be resolved manually. For example, to install the unregistered package [`OhMyREPL.jl`](https://github.com/KristofferC/OhMyREPL.jl):

    Pkg.clone("https://github.com/KristofferC/Tokenize.jl")
    Pkg.clone("https://github.com/KristofferC/OhMyREPL.jl")

Then, as is usual, use `using` to use the package:

    using OhMyREPL

