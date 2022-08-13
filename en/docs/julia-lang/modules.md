---
title: "Modules"
slug: "modules"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Syntax
- module Module; ...; end
- using Module
- import Module

## Wrap Code in a Module
The `module` keyword can be used to begin a module, which allows code to be organized and namespaced. Modules can define an external interface, typically consisting of `export`ed symbols. To support this external interface, modules can have unexported internal [functions][1] and [types][2] not intended for public use.

Some modules primarily exist to wrap a type and associated functions. Such modules, by convention, are usually named with the plural form of the type's name. For instance, if we have a module that provides a `Building` type, we can call such a module `Buildings`.

    module Buildings

    immutable Building
        name::String
        stories::Int
        height::Int  # in metres
    end

    name(b::Building) = b.name
    stories(b::Building) = b.stories
    height(b::Building) = b.height

    function Base.show(io::IO, b::Building)
        Base.print(stories(b), "-story ", name(b), " with height ", height(b), "m")
    end

    export Building, name, stories, height

    end

The module can then be used with the `using` statement:

    julia> using Buildings
    
    julia> Building("Burj Khalifa", 163, 830)
    163-story Burj Khalifa with height 830m
    
    julia> height(ans)
    830


  [1]: https://www.wikiod.com/julia-lang/functions
  [2]: https://www.wikiod.com/julia-lang/types

## Using Modules to Organize Packages
Typically, [packages][1] consist of one or more modules. As packages grow, it may be useful to organize the main module of the package into smaller modules. A common idiom is to define those modules as submodules of the main module:

    module RootModule

    module SubModule1

    ...

    end

    module SubModule2

    ...

    end

    end

Initially, neither root module nor submodules have access to each others' exported symbols. However, relative imports are supported to address this issue:

    module RootModule

    module SubModule1

    const x = 10
    export x

    end

    module SubModule2

    # import submodule of parent module
    using ..SubModule1
    const y = 2x
    export y

    end

    # import submodule of current module
    using .SubModule1
    using .SubModule2
    const z = x + y

    end

In this example, the value of `RootModule.z` is `30`.

  [1]: https://www.wikiod.com/julia-lang/packages

