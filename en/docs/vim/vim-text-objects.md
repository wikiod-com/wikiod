---
title: "Vim Text Objects"
slug: "vim-text-objects"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Select a word without surrounding white space
Suppose we want to select a word without surrounding white spaces, use the text object `iw` for inner word using visual mode:

1. Got to normal mode by pressing `ESC`
2. Type `viw` at the beginning of a word
3. This will select the inner word

   [![viw][1]][1]
    


  [1]: http://i.stack.imgur.com/hQf4X.png

## Select a word with surrounding white space
Suppose we want to select a word with a surrounding white space, use the text object `aw` for around a word using visual mode:

 1. Got to normal mode by pressing ESC
 2. Type `vaw` at the beginning of a word
 3. This will select the word with white space

    [![vaw][1]][1]

  [1]: http://i.stack.imgur.com/UUq8i.png

## Select text inside a tag
We can select a text within an `html` or `xml` tag by using visual selection `v` and text object `it` .


 1. Go to normal mode py pressing `ESC`
 2. Type `vit` from anywhere within the `html` or `xml` section
 3. This will visually select all text inside the `tag`

[![enter image description here][1]][1]

All other text objects can also be used to operate on the text inside the tag

 1. `cit` - delete text inside the tag and place in `insert` mode
 2. `dit` - delete text inside the tag and remain in `normal` mode
 3. `cat` - delete around tag and place in `insert` mode
 4. `dat` - delete text around the tag and remain in `normal` mode

  [1]: https://i.stack.imgur.com/dZtfw.png

