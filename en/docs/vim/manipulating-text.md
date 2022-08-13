---
title: "Manipulating text"
slug: "manipulating-text"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

To increment and decrement things like `11:59AM`, `3rd`, and `XVIII`, use the plugin [vim-speeddating](https://github.com/tpope/vim-speeddating)

## Converting text case
## In normal mode:

* `~` inverts the case of the character under the cursor,
* `gu{motion}` lowercases the text covered by `{motion}`,
* `gU{motion}` uppercases the text covered by `{motion}`

Example (`^` marks the cursor position):

    Lorem ipsum dolor sit amet.
            ^
    Lorem ipSum dolor sit amet.    ~
    Lorem IPSUM DOLOR sit amet.    gU2w
    Lorem IPsum DOLOR sit amet.    gue

## In visual mode:
 
* `~` inverts the case of the selected text,
* `u` lowercases the selected text,
* `U` uppercases the selected text

Example (`^^^` marks the visual selection):

    Lorem ipsum dolor sit amet.
            ^^^^^^^^^^^^^
    Lorem ipSUM DOLOR SIT amet.    ~
    Lorem ipSUM DOLOR SIT amet.    U
    Lorem ipsum dolor sit amet.    u

## Incrementing and decrementing numbers and alphabetical characters
In normal mode, we can increment the nearest number on the line at or after the cursor with `<C-a>` and decrement it with `<C-x>`. In the following examples, the cursor position is indicated by `^`.

### Incrementing and decrementing numbers

    for i in range(11):
          ^

`<C-x>` decrements the number:

    for i in range(10):
                    ^

`10<C-a>` increments it by `10`:

    for i in range(20):
                    ^
### Incrementing and decrementing alphabetical characters

To make increment and decrement also work with letters, either use the ex command `:set nrformats+=alpha` or add `set nrformats+=alpha` to your `.vimrc`.

Increment example:

    AAD
     ^ 

`<C-a>` increments it to `B`:

    ABD
     ^ 

Decrement example:

    ABD
      ^ 

`<C-x>` decrements `D` to `C`:

    ABC
      ^ 

### Incrementing and decrementing numbers when alphabetical increment/decrement is enabled

Notice that enabling increment/decrement to work with alphabetical characters means that you have to be careful not to modify them when you really want to just modify numbers. You can either turn off alphabetical increment/decrement by using the ex command `:set nrformats-=alpha` or you can just be aware of it and be sure [to move][1] to the number before increment or decrement. Here is the "`for i in range(11):`" example from above redone to work while alphabetical increment/decrement is set:

Let's say you want to decrease `11` to `10` and alphabetical increment/decrement is active.

    for i in range(11):
          ^

Since alphabetical increment/decrement is active, to avoid modifying the character under the cursor, first move forward to the first `1` using the *normal mode* movement command `f1` (that is lowercase `f` followed by the number `1`, not to be confused with a function key):

    for i in range(11):
                   ^
Now, since the cursor is on the number, you can decrement it with `<C-x>`. Upon decrement, the cursor is repositioned to the last digit of the numeral:

    for i in range(10):
                    ^


  [1]: https://www.wikiod.com/vim/movement#Basic Motion

## Formatting Code
In normal mode:

`gg` go to top

`=` then `G`


## Using "verbs" and "nouns" for text editing
One of the ways to think about the commands that should be executed, to edit a text in a certain manner, is as entire sentences.

A command is an action performed on an object. Therefore it has a verb:

    :normal i    " insert
    :normal a    " append
    :normal c    " overwrite
    :normal y    " yank (copy)
    :normal d    " delete
Some of these words work with an object like `d`, `c`, `y`. Such objects can be **word, line, sentence, paragraph, tag**. One can use these in combination:

    :normal dw    " deletes the text from the position of the cursor to the end of the next word
    :normal cw    " deletes the text from the cursor to the end of the next word and
                  " enters insert mode
Also one could use a **modifier** to specify precisely where should the action be executed:

    :normal diw    " delete inside word. I.e. delete the word in which is the cursor.
    :normal ciw    " removes the word, the cursor points at and enters insert mode
    :normal ci"    " removes everything between the opening and closing quotes and
                   " enters insert mode
    :normal cap    " change the current paragraph
    :normal ct8    " remove everything until the next number 8 and enter insert mode
    :normal cf8    " like above but remove also the number
    :normal c/goal " remove everything until the word 'goal' and enter insert mode
    :normal ci{    " change everything inside the curly braces

**More resources:**

[Learn to speak vim — verbs, nouns, and modifiers!][1]

[Learning Vim in 2014: Vim as Language][2]

[VimSpeak editing using Speech Grammar][3]


  [1]: https://yanpritzker.com/learn-to-speak-vim-verbs-nouns-and-modifiers-d7bfed1f6b2d#.c50ws56cu
  [2]: https://benmccormick.org/2014/07/02/learning-vim-in-2014-vim-as-language/
  [3]: https://www.youtube.com/watch?v=TEBMlXRjhZY


