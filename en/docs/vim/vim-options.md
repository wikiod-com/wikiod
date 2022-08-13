---
title: "Vim Options"
slug: "vim-options"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Syntax
- `:set [no](option|shortcut)`
- `:set (option|shortcut)=value`
- `:set (option|shortcut)(?|&)`
- do not use `:` in the vimrc file


See [vimcast 1 video][1]

See [vimcast 1 transcript][2]

  [1]: http://vimcasts.org/episodes/show-invisibles/
  [2]: http://vimcasts.org/transcripts/1/en/

## Indentation
# Width
To make indentations 4 spaces wide:

    :set shiftwidth=4
# Spaces
To use spaces as indents, 4 spaces wide:

    :set expandtab
    :set softtabstop=4
`softtabstop` and `sts` are equivalent:

    :set sts=4

# Tabs
To use tabs as indents, 4 spaces wide:
    
    :set noexpandtab
    :set tabstop=4
`tabstop` and `ts` are equivalent:

    :set ts=4

# Automatic Indentation

    :set autoindent
# Instruction descriptions
| Instruction | Description | Default |
| ------ | ------ | ------ |
| tabstop | width of tab character | 8 |
| expandtab| causes spaces to be use instead of tab character | off |
| softabstop | tune the whitespace | 0 |
| shiftwidth | determines whitespace amount when in [normal][1] mode | 8


  [1]: https://www.wikiod.com/vim/modes---insert-normal-visual-ex#The basics about modes

## Set
To set the options - use `:set` instruction. Example:

    :set ts=4
    :set shiftwidth=4
    :set expandtab
    :set autoindent
    
To view the current value of the option - type `:set {option}?`. Example:

    :set ts?

To reset the value of the option to its default - type `:set {option}&`. Example:

    :set ts&


## Invisible characters
# Show or hide invisible characters

To show invisible characters:

    :set list 

To hide invisible characters:

    :set nolist

To toggle between showing and hiding invisible characters:

    :set list!

# Default symbol characters
| Symbol| Character |
| :------: | ------ |
| **^I** | Tab   |
| **$** | New Line |

# Customize symbols
To set the tab character to **> ** and the new line character to **¬**

    set listchars=tab:>\ ,eol:¬

To set the spaces to **_**

    set listchars=spaces

To see a list of character options

    :help listchars


