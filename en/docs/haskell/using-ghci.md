---
title: "Using GHCi"
slug: "using-ghci"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

GHCI is the interactive REPL that comes bundled with GHC.

## Breakpoints with GHCi


## Starting GHCi
Type `ghci` at a shell prompt to start GHCI.

    $ ghci
    GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
    Prelude> 

## Changing the GHCi default prompt
By default, GHCI's prompt shows all the modules you have loaded into your interactive session. If you have many modules loaded this can get long:

    Prelude Data.List Control.Monad> -- etc

The `:set prompt` command changes the prompt for this interactive session.

    Prelude Data.List Control.Monad> :set prompt "foo> "
    foo> 

To change the prompt permanently, add `:set prompt "foo> "` to [the GHCi config file](https://www.wikiod.com/haskell/using-ghci#The GHCi configuration file).

## The GHCi configuration file
GHCi uses a configuration file in `~/.ghci`. A configuration file consists of a sequence of commands which GHCi will execute on startup.

    $ echo ":set prompt \"foo> \"" > ~/.ghci
    $ ghci
    GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
    Loaded GHCi configuration from ~/.ghci
    foo> 



## Loading a file
The `:l` or `:load` command type-checks and loads a file.

    $ echo "f = putStrLn \"example\"" > example.hs
    $ ghci
    GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
    ghci> :l example.hs
    [1 of 1] Compiling Main               ( example.hs, interpreted )
    Ok, modules loaded: Main.
    ghci> f
    example

## Quitting GHCi
You can quit GHCi simply with `:q` or `:quit`

    ghci> :q
    Leaving GHCi.

    ghci> :quit
    Leaving GHCi.

Alternatively, the shortcut <kbd>CTRL</kbd>+<kbd>D</kbd> (<kbd>Cmd</kbd>+<kbd>D</kbd> for OSX) has the same effect as `:q`.

## Reloading a already loaded file
If you have loaded a file into GHCi (e.g. using `:l filename.hs`) and you have changed the file in an editor outside of GHCi you must reload the file with `:r` or `:reload` in order to make use of the changes, hence you don't need to type again the filename.
    
    ghci> :r
    OK, modules loaded: Main.

    ghci> :reload
    OK, modules loaded: Main.

## Multi-line statements
The `:{` instruction begins _multi-line mode_ and `:}` ends it. In multi-line mode GHCi will interpret newlines as semicolons, not as the end of an instruction.

    ghci> :{
    ghci| myFoldr f z [] = z
    ghci| myFoldr f z (y:ys) = f y (myFoldr f z ys)
    ghci| :}
    ghci> :t myFoldr
    myFoldr :: (a -> b -> b) -> b -> [a] -> b

