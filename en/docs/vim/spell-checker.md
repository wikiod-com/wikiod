---
title: "Spell checker"
slug: "spell-checker"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Spell Checking
To turn on the vim spell checker run `:set spell`.
To turn it off run `:set nospell`.
If you always want the spell checker to be on, add `set spell` to your vimrc.
You can turn spelling on only for certain filetypes using an auto command.

Once the spell checker is on, misspelled words will be highlighted.
Type `]s` to move to the next misspelled word and `[s` to move to the previous one.
To  see a list of corrected spellings, place the cursor on a misspelled word and type `z=`.
You can type the number of the word you wish to replace the misspelled word with and hit `<enter>` to replace it, or you can just hit enter to leave the word unchanged.

With the cursor on a misspelled word, you can also type `<number>z=` to change to the `<number>`th correction without viewing the list.
Typically you will use `1z=` if you think `vim`'s first guess is likely to be the correct word.

## Set Word List
To set the word list that `vim` will use for spell checking set the `spelllang` option.
For example

    :set spelllang=en        # set to English, usually this is the default
    :set spelllang=en_us     # set to U.S. English 
    :set spelllang=en_uk     # set to U.K. English spellings
    :set spelllang=es        # set to Spanish

If you want to set the `spelllang` and turn on spell checking in one command, you can do:

    :setlocal spell spelllang=en

