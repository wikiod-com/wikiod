---
title: "Whitespace"
slug: "whitespace"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Here is how you can clean up whitespace.

See [vimcast 4 transcript][1]

  [1]: http://vimcasts.org/transcripts/4/en/

## Delete trailing spaces in a file
You can delete trailing spaces with the following command.

    :%s/\s\+$//e

This command is explained as follows:

 - enter Command mode with `:`
 - do this to the entire file with `%` (default would be for the current line)
 - substitute action `s`
 - `/` start of the search pattern
 - `\s` whitespace character
 - `\+` escaped  + sign, one or more spaces should be matched
 - before the line end `$`
 - `/` end of the search pattern, beginning of replacement pattern
 - `/` end of the replacement pattern.  Basically, replace with nothing.
 - `e` suppress error messages if no match found


## Convert tabs to spaces and spaces to tabs
You can convert tabs to spaces by doing the following:

First check that [expandtab][1] is switched off

    :set noexpandtab

 Then 

    :retab!
which replaces spaces of a certain length with tabs

If you enable [expandtab][1] again `:set expandtab` then and run the `:retab!` command then all the tabs becomes spaces. 

If you want to do this for selected text then first select the text in [visual mode][2].


  [1]: https://www.wikiod.com/vim/indentation
  [2]: https://www.wikiod.com/vim/modes---insert-normal-visual-ex

## Delete blank lines in a file
You can delete all blank lines in a file with the following command:
    :g/^$/d

This command is explained as follows:

 - enter Command mode with `:`
 - `g` is a global command that should occur on the entire file
 - `/` start of the search pattern
 - the search pattern of blank line is `^g`
 - `/`end of the search pattern
 - Ex command `d` deletes a line

