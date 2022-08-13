---
title: "Substitution"
slug: "substitution"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Syntax
- `s/<pattern>/<pattern>/optional-flags`  
- `<pattern>` is a Regex

## Parameters
| Flag | Meaning                                                   |
| ---- | --------------------------------------------------------- |
| &    | Keep the flags from the previous substitute.              |
| c    | Prompt to confirm each substitution.                      |
| e    | Do not report errors.                                     |
| g    | Replace all occurrences in the line.                      |
| i    | Case-insensitive matching.                                |
| I    | Case-sensitive matching.                                  |
| n    | Report the number of matches, do not actually substitute. |


Use `set gdefault` to avoid having to specify the 'g' flag on every substitute.

# Example

When `gdefault` is set, running `:s/foo/bar` on the line `foo baz foo` will yield `bar baz bar` instead of `bar baz foo`. 

## Quickly refactor the word under the cursor
1. <kbd>*</kbd> on the word you want to substitute.

2.  `:%s//replacement/g`, leaving the *find* pattern empty.

## Simple replacement
`:s/foo/bar` Replace the **first** instance of *foo* with *bar* on the current line.

`:s/foo/bar/g` Replace every instance of *foo* with *bar* on the current line.

`:%s/foo/bar/g` Replace *foo* with *bar* throughout the entire file.



## Replacement with interactive approval
`:s/foo/bar/c` Marks the first instance of *foo* on the line and asks for confirmation for substitution with *bar* 

`:%s/foo/bar/gc` Marks consecutively every match of *foo* in the file and asks for confirmation for substitution with *bar*

## Keyboard short-cut to replace currenlty highlighted word
For example, with following `nmap`:

    nmap <expr> <S-F6> ':%s/' . @/ . '//gc<LEFT><LEFT><LEFT>'

select a word with <kbd>*</kbd>, type <kbd>Shift</kbd>-<kbd>F6</kbd>, type in a replacement and hit <kbd>Enter</kbd> to rename all occurrences interactively.

