---
title: "Modules"
slug: "modules"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Syntax
 - module Name where  -- export all names declared in this file
   
 - module Name (functionOne, Type (..)) where  -- export only functionOne, Type, and Type's constructors

 - import Module  -- import all of Module's exported names

 - import qualified Module as MN  -- qualified import

 - import Module (justThisFunction)  -- import only certain names from a module

 - import Module hiding (functionName, Type)  -- import all names from a module except functionName and Type


Haskell has support for modules:

 * a module can export all, or a subset of its member types & functions

 * a module can "re-export" names it imported from other modules

On the consumer end of a module, one can:

 * import all, or a subset of module members

 * hide imports of a particular member or set of members

[haskell.org][1] has a great chapter on module definition.


  [1]: https://www.haskell.org/tutorial/modules.html

## Defining Your Own Module
If we have a file called `Business.hs`, we can define a `Business` module that can be `import`-ed, like so:

    module Business (
        Person (..), -- ^ Export the Person type and all its constructors and field names
        employees   -- ^ Export the employees function
    ) where
    -- begin types, function definitions, etc

A deeper hierarchy is of course possible; see the [Hierarchical module names][1] example.


  [1]: https://www.wikiod.com/haskell/modules#Hierarchical module names

## Exporting Constructors
To export the type and all its constructors, one must use the following syntax:

    module X (Person (..)) where

So, for the following top-level definitions in a file called `People.hs`:

    data Person = Friend String | Foe deriving (Show, Eq, Ord)
    
    isFoe Foe = True
    isFoe _   = False

This module declaration at the top:

    module People (Person (..)) where

would only export `Person` and its constructors `Friend` and `Foe`.

If the export list following the module keyword is omitted, all of the names bound at the top level of the module would be exported:

    module People where

would export `Person`, its constructors, and the `isFoe` function.



## Importing Specific Members of a Module
Haskell supports importing a subset of items from a module.

    import qualified Data.Stream (map) as D
    
would only import `map` from `Data.Stream`, and calls to this function would require `D.`:

    D.map odd [1..]

otherwise the compiler will try to use `Prelude`'s `map` function.

## Hiding Imports
Prelude often defines functions whose names are used elsewhere. Not hiding such imports (or using qualified imports where clashes occur) will cause compilation errors.

[`Data.Stream`][1] defines functions named `map`, `head` and `tail` which normally clashes with those defined in Prelude. We can hide those imports from Prelude using `hiding`:

    import Data.Stream -- everything from Data.Stream
    import Prelude hiding (map, head, tail, scan, foldl, foldr, filter, dropWhile, take) -- etc

In reality, it would require too much code to hide Prelude clashes like this, so you would in fact use a `qualified` import of `Data.Stream` instead.


  [1]: https://hackage.haskell.org/package/Stream-0.4.7.2/docs/Data-Stream.html

## Qualifying Imports
When multiple modules define the same functions by name, the compiler will complain. In such cases (or to improve readability), we can use a `qualified` import:

    import qualified Data.Stream as D

Now we can prevent ambiguity compiler errors when we use `map`, which is defined in `Prelude` and `Data.Stream`:

    map (== 1) [1,2,3] -- will use Prelude.map
    D.map (odd) (fromList [1..]) -- will use Data.Stream.map

It is also possible to import a module with only the clashing names being qualified via `import Data.Text as T`, which allows one to have `Text` instead of `T.Text` etc.

## Hierarchical module names
The names of modules follow the filesystem's hierarchical structure. With the following file layout:

    Foo/
    ├── Baz/
    │   └── Quux.hs
    └── Bar.hs
    Foo.hs
    Bar.hs

the module headers would look like this:

    -- file Foo.hs
    module Foo where

    -- file Bar.hs
    module Bar where

    -- file Foo/Bar.hs
    module Foo.Bar where

    -- file Foo/Baz/Quux.hs
    module Foo.Baz.Quux where

Note that:

  * the module name is based on the path of the file declaring the module
  * Folders may share a name with a module, which gives a naturally hierarchical naming structure to modules

