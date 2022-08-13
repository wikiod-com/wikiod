---
title: "Normal mode commands (Editing)"
slug: "normal-mode-commands-editing"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Copy, Cut and Paste
In Vim, these operations are handled differently from what you might be used to in almost any other modern editor or word processor (<kbd>Ctrl-C</kbd>, <kbd>Ctrl-X</kbd>, <kbd>Ctrl-V</kbd>). To understand, you need to know a little about registers and motions.

_Note: this section will not cover Visual Mode copying and cutting or range yanking as these are beyond the scope of both Normal Mode and basic editing._


Registers
---------
Vim uses the concept of _registers_ to handle moving text around within the program itself. Windows has a single clipboard for this purpose, which is analogous to a single register in Vim. When copying, cutting, and pasting in Vim, there are ways to use a similarly simple editing workflow (where you don't have to think about registers), but there are also much more complex possibilities.

A register is targeted for the input/output of a command by prefixing the command with <kbd>"</kbd> and a lowercase letter name.


Motions
-------
A motion in Vim is any command that moves the cursor position elsewhere. When copying, cutting, and pasting in Normal Mode, the possibilities of text selection for movement are only limited by your knowledge of motions. A few will be illustrated below.


Copying and Cutting
-------------------

The basic commands copy and cut operations are built on are <kbd>y</kbd> ('yank', for copy) and <kbd>d</kbd> ('delete', for cut).  You'll see the similarities in the following table.

| Command              | <kbd>:</kbd>   | Description                                                                                                                 |                      |
| ------               | ------         | ------                                                                                                                      |
| <kbd>y{motion}</kbd> |                | Copy ('yank') text indicated by the motion into the default register
| <kbd>yy</kbd>        |                | Copy the current line into the default register, _linewise_
| <kbd>Y</kbd>         |                | Copy the current line into the default register (synonym for <kbd>yy</kbd>)
| <kbd>"ayiw</kbd>     |                | Copy the word the cursor is on into register 'a'
| <kbd>20"byy</kbd>    |                | Copy twenty lines, beginning from the cursor, into register 'b'
| <kbd>d{motion}</kbd> |                | Cut ('delete') text indicated by the motion into the default register 
| <kbd>dd</kbd>        |                | Cut the current line into the default register, _linewise_
| <kbd>D</kbd>         |                | Cut from the cursor to end of line into the default register (NOT a synonym for <kbd>dd</kbd>)
| <kbd>"adiw</kbd>     |                | Cut the word the cursor is on into register 'a'
| <kbd>20"bdd</kbd>    |                | Cut twenty lines, beginning from the cursor, into register 'b'

Note: when something is copied or cut _linewise_, the paste behavior shown below will place text either before or after the current _line_ (rather than the cursor).  Examples follow to clarify.

Pasting
-------

There are several ways to paste in Vim, depending on what you are trying to accomplish.

| Command              | <kbd>:</kbd>   | Description                                                                                                                 |                      |
| ------               | ------         | ------                                                                                                                      |
| <kbd>p</kbd>         |                | Paste whatever is in the default register _after_ the cursor
| <kbd>P</kbd>         |                | Paste whatever is in the default register _before_ the cursor
| <kbd>"ap</kbd>       |                | Paste the contents of register 'a' after the cursor
| <kbd>"cP</kbd>       |                | Paste the contents of register 'c' before the cursor

So, How Do I Perform A Really Simple Cut and Paste?
---------------------------------------------------

If I have the following text:

    1 This line should be second
    2 This line should be first

I can do the simplest cut-and-paste by placing my cursor somewhere on line 1 and typing <kbd>ddp</kbd>.  Here are the results:

    1 This line should be first
    2 This line should be second

What happened? <kbd>dd</kbd> 'Cuts' the first line (linewise) into the default register - which will only contain one thing at a time, like the Windows clipboard - and <kbd>p</kbd> pastes the line after the current one, which has just changed due to the <kbd>dd</kbd> command.

Here's a not-quite-as-simple example. I need to move a couple of words around. (This is contrived and unnecessary, but you can apply this principle to larger chunks of code.)

    1 These words order out are of

I can repeat <kbd>w</kbd> to get to the 'o' at the front of 'order' (or <kbd>b</kbd> if I just typed it and realized my mistake).

Then <kbd>"adaw</kbd> to put 'order ' in register 'a'.

Then <kbd>w</kbd> to get to the 'a' in 'are'.

Following this, I would type <kbd>"bdaw</kbd> to put 'are ' into register 'b'.  Now I have this displayed:

    1 These words out of

To be clear, now 'order ' is in register 'a' and 'are ' is in register 'b', like two separate clipboards.

To arrange the words correctly, I type <kbd>b</kbd> to get to the 'o' in 'out', and then <kbd>"bP</kbd> to put 'are ' from register 'b' in front of 'out':

    1 These words are out of

Now I type <kbd>A</kbd> to get to the end of the line, followed by <kbd>Space</kbd><kbd>Esc</kbd> (assuming there was no space after 'of') and <kbd>"ap</kbd> to put 'order' where it belongs.

    1 These words are out of order

## Basic Undo and Redo
Undo
----

| Command          | <kbd>:</kbd>    | Description                                                                                                                 |                  |
| ------           | ------          | ------                                                                                                                      |
| <kbd>u</kbd>     |`u`,`undo`       | Undo the most recent change
| <kbd>5u</kbd>    |                 | Undo the _five_ most recent changes (use any number)

Please be aware that in Vim, the 'most recent change' varies according to the mode you are in. If you enter Insert Mode (<kbd>i</kbd>) and type out an entire paragraph before dropping back to Normal Mode (<kbd>Esc</kbd>), _that entire paragraph_ is considered the most recent change.


Redo
----
| Command           | <kbd>:</kbd>   | Description                                                                                                                 |                   |
| ------            | ------         | ------                                                                                                                      |
| <kbd>Ctrl-R</kbd> |`red`,`redo`    | Redo the most recent undone change
| <kbd>2Ctrl-R</kbd>|                | Redo the _two_ most recent undone changes (use any number)

There is one other way to undo and redo changes in Vim that is handled a bit differently. When you undo a change with <kbd>u</kbd>, you traverse back up the nodes on a 'tree' of your changes, and pressing <kbd>Ctrl-R</kbd> walks back down those nodes in order.  (The undo tree is a separate topic and is too complex to cover here.)

You can _also_ use <kbd>U</kbd> (that is, uppercase) to remove all the latest changes on a single line (the line where your last changes were made).  This _does not_ traverse the nodes of the tree in the same way as <kbd>u</kbd>.  Using <kbd>U</kbd> actually counts as a change itself - another node _forward_ on the tree - so that if you press <kbd>U</kbd> a second time immediately after the first it will act as a Redo command.

Each has its uses, but <kbd>u</kbd> / <kbd>:</kbd>`undo` should cover most simple cases.

## Introduction - Quick Note on Normal Mode
In Normal Mode, commands can be entered by direct key combinations (typing <kbd>u</kbd> to undo the last change, for example). These commands often have equivalents in 'ex' mode, accessed by typing a colon <kbd>:</kbd>, which drops you into a single-line buffer at the bottom of the Vim window.

In 'ex' mode, after typing the colon you type a command name or its abbreviation followed by <kbd>Enter</kbd> to execute the command.  So, <kbd>:</kbd>`undo`<kbd>Enter</kbd> accomplishes the same thing as directly typing <kbd>u</kbd> in Normal Mode.

You can see that the direct commands will often be faster (once learned) than the 'ex' commands for simple editing, but for completeness, wherever possible in the documentation that follows, if both are available for use then both will be shown.

Most of these commands can also be preceded with a _count_ by prefixing or interspersing a number - typing <kbd>3dd</kbd> in Normal Mode, for example, deletes three lines (beginning from the current cursor position).

## Repeat the Last Change
The Repeat command, executed with the dot or period key (<kbd>.</kbd>), is more useful than it first appears. Once learned, you will find yourself using it often.

| Command           | <kbd>:</kbd>   | Description                                                                                                                 |                   |
| ------            | ------         | ------                                                                                                                      |
| <kbd>.</kbd>      |                | Repeat the last change
| <kbd>10.</kbd>    |                | Repeat the last change 10 times

So then, for a very simple example, if you make a change to line 1 by typing <kbd>i</kbd>`I`</kbd>Esc</kbd>, with the following result:

    1 I made a mistake
    2  made a mistake
    3  made a mistake

Your cursor will be at position 1 of line 1, and all you need to do to fix the next two lines is press <kbd>j.</kbd> twice - that is, <kbd>j</kbd> to move down a line and <kbd>.</kbd> to repeat the last change, which was the addition of the `I`. No need to jump back into Insert Mode twice to fix those lines.

It becomes much more powerful when used to repeat [macros][1].


  [1]: https://www.wikiod.com/vim/macros

## Completion
Completion can be used to match words used in a document. When typing a word, <kbd>Ctrl</kbd><kbd>p</kbd> or <kbd>Ctrl</kbd><kbd>n</kbd> will match previous or next similar words in the document.

This can even be combined with <kbd>Ctrl-X</kbd> mode to complete entire lines. For instance type something like:

    This is an example sentence.

then go to the next line and begin typing the same sentence:

    Thi

and then hit <kbd>Ctrl</kbd><kbd>p</kbd> which will result in:

    This

Now still in insert mode, hit <kbd>Ctrl</kbd><kbd>x</kbd> <kbd>Ctrl</kbd><kbd>p</kbd> and then next word will be completed resulting in:

    This is

Continue hitting <kbd>Ctrl</kbd><kbd>x</kbd> <kbd>Ctrl</kbd><kbd>p</kbd> until the entire line is completed.

If you know you want to complete an entire line type this:

    This is an example sentence.

then on the next line type:

    Thi

and hit <kbd>x</kbd> <kbd>Ctrl</kbd><kbd>l</kbd> to complete the line.

If the completion being done is a filename <kbd>Ctrl</kbd><kbd>x</kbd> <kbd>Ctrl</kbd><kbd>f</kbd> can be used to complete that directory. Type:

    ~/Deskt

then hit <kbd>Ctrl</kbd><kbd>x</kbd> <kbd>Ctrl</kbd><kbd>f</kbd> and:

    ~/Desktop

will be completed (if at that location). <kbd>Ctrl</kbd><kbd>x</kbd> <kbd>Ctrl</kbd><kbd>f</kbd> can then be repeatedly used to list the files in the Desktop.

