---
title: "Movement"
slug: "movement"
draft: false
images: []
weight: 9855
type: docs
toc: true
---

## Basic Motion
# Remarks
- Every motion can be used after an operator command, so the command operates on the text comprised by the movement's reach.
- Just like operator commands, motions can include a count, so you can move by <kbd>2w</kbd>ords, for example.

# Arrows

In Vim, normal arrow/cursor keys (<kbd>←</kbd><kbd>↓</kbd><kbd>↑</kbd><kbd>→</kbd>) work as expected. However, for touch-typers, it's easier to use the <kbd>h</kbd><kbd>j</kbd><kbd>k</kbd><kbd>l</kbd> alternative keys. On a typical keyboard, they're located next to each other on the same row, and easily accessible using right hand. The mnemonic technique to remember which is which among them goes like this:

* <kbd>h</kbd>/<kbd>l</kbd> &mdash; those are located "most to the left/right" among the four letters on the keyboard, so they are equivalent to "going left/right" respectively;
* <kbd>j</kbd> &mdash; lowercase "j" has its tail going "down" below typical letters, like a small arrow - so it's equivalent to "going down";
* <kbd>k</kbd> &mdash; conversely, lowercase "k" has its "ascender" going "up" above typical letters, like a small pointer - so it's equivalent to "going up".

# Basic motions
All commands below should be done in **normal mode**.

| Command                            | Description                                                                                                                 |
| ------                             | ------                                                                                                                      |
| <kbd>h</kbd> or <kbd>left</kbd>  | go <kbd>[count]</kbd> characters to the left                                                                                |
| <kbd>j</kbd> or <kbd>down</kbd>  | go <kbd>[count]</kbd> characters below                                                                                      |
| <kbd>k</kbd> or <kbd>up</kbd>    | go <kbd>[count]</kbd> characters above                                                                                      |
| <kbd>l</kbd> or <kbd>right</kbd> | go <kbd>[count]</kbd> characters to the right                                                                               |
| <kbd>gg</kbd>                   | go the first line, or <kbd>[count]</kbd>'th line, if given|
|<kbd>H</kbd>                     | go to the first line in the visible screen
|<kbd>M</kbd>                     | go to the middle line in the visible screen
|<kbd>L</kbd>                     | go to the last line in the visible screen
| <kbd>G</kbd>                    | go the last line, or <kbd>[count]</kbd>'th line, if given|
| <kbd>Home</kbd> or <kbd>0</kbd>                 | go to first character of the line                                                                                           |
| <kbd>^</kbd>                       | go to first non-blank character of the line                                                                                 
| <kbd>+</kbd>                      | go down one line to first non-blank character
| <kbd>-</kbd>                      | go up one line to first non-blank character                                                                                 
| <kbd>$</kbd> or <kbd>End</kbd>   | go to the end of the line (if <kbd>[count]</kbd> is given, go <kbd>[count - 1]</kbd> lines down)                            |
| <kbd>&#124;</kbd>                | go to the <kbd>[count]</kbd>'th character or go to the beginning of the line if `count` not specified                                                                                  |
| <kbd>f{char}</kbd>                 | go to <kbd>[count]</kbd>'th occurrence of <kbd>{char}</kbd> to the right *inclusive*                                        |
| <kbd>F{char}</kbd>                 | go to <kbd>[count]</kbd>'th occurrence of <kbd>{char}</kbd> to the left *inclusive*                                         |
| <kbd>t{char}</kbd>                 | go to <kbd>[count]</kbd>'th occurrence of <kbd>{char}</kbd> to the right *exclusive*                                        |
| <kbd>T{char}</kbd>                 | go to <kbd>[count]</kbd>'th occurrence of <kbd>{char}</kbd> to the left *exclusive*                                         |
| <kbd>;</kbd>                       | repeat latest <kbd>f</kbd>, <kbd>t</kbd>, <kbd>F</kbd> or <kbd>T</kbd> <kbd>[count]</kbd> times                             |
| <kbd>,</kbd>                       | repeat latest <kbd>f</kbd>, <kbd>t</kbd>, <kbd>F</kbd> or <kbd>T</kbd>, in the opposite direction, <kbd>[count]</kbd> times |
| <kbd>w</kbd>                       | go to the beginning of the next word |
| <kbd>b</kbd>                       | go to the beginning of the previous word |
| <kbd>e</kbd>                       | go to the ending of the next word |
| <kbd>ge</kbd>                      | go to the ending of the previous word |
| <kbd>%</kbd>                       | go to matching pairs, e.g `(), [], {}`, `/* */` or `#if, #ifdef, #else, #elif, #endif` |
|<kbd>{</kbd>  <kbd>}</kbd>| previous/next paragraph|
|<kbd>[{</kbd><kbd>]}</kbd>| beginning/ending of block|
|<kbd>'{char}</kbd> | Go to mark (mark with <kbd>m{char}</kbd>)|
|<kbd>\<C-B\></kbd><kbd>\<C-F\></kbd> | previous/next page |
|<kbd>\<C-O\></kbd><kbd>\<C-I\></kbd>| Go back or foward in the "jump list" (requires `jumplist` feature, see `:help jumps`) |



Note: <kbd>b</kbd>, <kbd>e</kbd>, and <kbd>w</kbd> consider a word to be letters, numbers, and underscores by default (this can be configured with the `iskeyword` setting). Each of these can also be capitalized, causing them to skip over anything that isn't whitespace as well.

Note: Vim recognizes two kinds of movement: operator movement (`:help movement`) and jumps (`:help jumplist`). Movements like those executed with `g` (`gg`, `G`, `g,`) count as jumps, as do changes. Changes get their own jumplist, which is navigable as mentioned above via `g,` and `g;` (see `:help changelist`). Jumps are not treated as motion commands by Vim

When moving up or down across lines, the cursor retains its column as would be expected. If the new line is too short the cursor moves to the end of the new line. If the column is beyond the end of the line, the cursor is displayed at the end of the line. The initial column number is still retained until an action is taken to alter it (such as editing text or explicitly moving column). 

If a line's length exceeds the width of the screen, the text is wrapped (under default settings, this behaviour can be configured). To move through lines as displayed on screen, rather than lines within the file, add <kbd>g</kbd> in front of the usual command. For example, <kbd>gj</kbd> will move the cursor to the position displayed one line below its current position, even if this is in the same line of the file.

## Searching
Jumping to characters
---------------------

<kbd>f</kbd><kbd>{char}</kbd> - move to the next occurrence of <kbd>{char}</kbd> to the right of the cursor on the same line

<kbd>F</kbd><kbd>{char}</kbd> - move to the next occurrence of <kbd>{char}</kbd> to the left of the cursor on the same line

<kbd>t</kbd><kbd>{char}</kbd> - move to the left of the next occurrence of <kbd>{char}</kbd> to the right of the cursor on the same line

<kbd>T</kbd><kbd>{char}</kbd> - move to the right of next occurrence of <kbd>{char}</kbd> to the left of the cursor on the same line

Jump forward / backward between the 'results' via <kbd>;</kbd> and <kbd>,</kbd>.

Further you can search for whole words via `/<searchterm>`<kbd>Enter</kbd>.


----------


Searching for strings
---------------------

<kbd>*</kbd> - move to the next occurrence of the word under the cursor

<kbd>#</kbd> - move to the previous occurrence of the word under the cursor

<kbd>/</kbd>`searchterm`<kbd>Enter</kbd> brings you to next match (forward-search). If you use <kbd>?</kbd> instead of <kbd>/</kbd>, searching goes backwards. 

Jump between the matches via <kbd>n</kbd> (next) and <kbd>N</kbd> (previous).

To view/edit your previous searches, type <kbd>/</kbd> and hit the <kbd>up</kbd> arrow key.

Helpful are also these settings: (note `:se` is equal to `:set`)

 - `:se hls` HighLightSearch, highlights all search matches; use `:noh` for temporarily turning off the search/mark highlighting (`:set noh` or `:set nohls` turns off.)
 - `:se is` or `:set incs` turns Incremental Search on, cursor jumps to the next match automatically. (`:se nois` turns off.)
 - `:se ic` IgnoreCase, turns case sensitivity off. (`:se noic` turns on again.)
 - `:se scs` SmartCaSe, can be used when IgnoreCase is set; makes case (in)sensitivity **smart**! e.g. `/the` will search for `the`, `The`, `ThE`, etc. while `/The` only will look for `The`.

## Navigating to the beginning of a specific word
When editing text, a common task is to navigate to a particular word on the screen. In these examples we explore how we can navigate to the word `updated`. For the sake of consistency across the examples, we aim to land on the first letter of the word.

----------

**Mid-screen jump**  
<kbd>M</kbd><kbd>$</kbd><kbd>B</kbd>  
      
[![enter image description here][1]][1]
       
This approach is quick, using only 3 keystrokes. The disadvantage however, is that it is not very general, as it's not common for our target line to happen to lie right on the middle of the screen. Still, it is a useful motion when making less granular movements.

----------

**Using a count**  
<kbd>3j</kbd><kbd>f</kbd><kbd>u</kbd><kbd>;</kbd><kbd>;</kbd>

[![enter image description here][2]][2]
    
At first glance, this may appear to be a step back from the first approach because of the number of keystrokes. But since we use a count here instead of <kbd>M</kbd>, it is more flexible. We can quickly identify the correct count to use if [relativenumber][3] is enabled. To move to the target word, using <kbd>f</kbd> in combination with <kbd>;</kbd> can be surprisingly effective - and certainly better than repeatedly pressing <kbd>w</kbd>. If you overshoot your target with <kbd>;</kbd>, you can go backwards with <kbd>,</kbd>.


----------

**Explicit search**  
<kbd>/</kbd><kbd>up</kbd><kbd>Enter</kbd><kbd>n</kbd><kbd>n</kbd>  

[![enter image description here][4]][4]

Navigating via <kbd>/</kbd> can be very powerful. We can often jump directly to our target word by typing it out. Here we type out only the first two characters in the hope that it uniquely matches our word. Unfortunately, there are multiple matches, but we can quickly jump to the next match with <kbd>n</kbd>. 

----------

**Implicit search**  
<kbd>/</kbd><kbd>y</kbd><kbd>Space</kbd><kbd>Enter</kbd><kbd>w</kbd>  

[![enter image description here][5]][5]

In some cases, it may be more efficient to jump *near* our target rather than aiming to go directly to it. Here we observe that there is an infrequently occurring letter, `y`, right next to the target. We can add a <kbd>Space</kbd> to our search term to decrease the chances that we hit some other `y` character along the way. This can also be used to great effect with <kbd>f{char}</kbd>, as in the example *Using a count*.

  [1]: http://i.stack.imgur.com/O095L.gif
  [2]: http://i.stack.imgur.com/GmzyL.gif
  [3]: https://www.wikiod.com/vim/tips-and-tricks-to-boost-productivity#Turn On Relative Line Numbers
  [4]: http://i.stack.imgur.com/Qq1Xa.gif
  [5]: http://i.stack.imgur.com/EarRy.gif

## Searching For Pattern
Vim supports the use of regular expressions when searching through a file.

The character to indicate that you wish to perform a search is `/`.

The simplest search you can perform is the following

    /if

This will search the entire file for all instances of `if`. However, our search `if` is actually a regular expression that will match any occurrence of the word `if` including those inside of other words.

For instance, our search would say all of the following words match our search: `if`, `spiffy`, `endif`, etc.

We can do more complicated searches by using more complicated regular expressions.

If our search was:

    /\<if\>

then our search would only return exact matches to the full word `if`. The above `spiffy` and `endif` would not be returned by the search, only `if`.

We can also use ranges. Given a file:

    hello1
    hello2
    hello3
    hello4

If we want to search for those lines containing "hello" followed by a digit between 1 and 3 we would say:

    /hello[1-3]

Another example:

    /(?:\d*\.)?\d+

would find all of the integer and decimals numbers in the file.

## Using Marks to Move Around
Marks are like bookmarks; they help you find places you've already been.

# TLDR 
Set them in normal mode with `m{a-zA-Z}`, and jump to them in normal or visual mode with `'{a-zA-Z}` (single quote) or <code>\`{a-zA-Z}</code> (backtick). Lowercase letters are for marks within a buffer, and capital letters and digits are global. See your currently set marks with `:marks`, and for more info see `:help mark`.

# Set a mark

Vim's built-in help says:  

    m{a-zA-Z}               Set mark {a-zA-Z} at cursor position (does not move
                            the cursor, this is not a motion command).

The mark will keep track of which line and column it was placed at. There is no visual confirmation that a mark was set, or if a mark had a previous value and has been overwritten.

# Jump to a mark

Vim's built-in help says:  

    Jumping to a mark can be done in two ways:  
    1. With ` (backtick):     The cursor is positioned at the specified location
                              and the motion is exclusive.
    2. With ' (single quote): The cursor is positioned on the first non-blank
                              character in the line of the specified location and
                              the motion is linewise.

Backtick uses the column position, while Single-quote does not. The difference between simply allows you to ignore the column position of your mark if you want.

You can jump between non-global marks in visual mode in addition to normal mode, to allow for selecting text based on marks.

# Global Marks

Global marks (capital letters) allow for jumping between files. What that means is if, for example, mark `A` is set in `foo.txt`, then from `bar.txt` (anywhere in my filesystem), if I jump to mark `A`, my current buffer will be replaced with `foo.txt`. Vim will prompt to save changes.

Jumping to a mark in another file is __not__ considered to be a movement, and visual selections (among other things) will not work like jumping to marks within a buffer.

To go back to the previous file (`bar.txt` in this case), use `:b[uffer] #` (that is, `:b#` or `:buffer#`).

Note: 

# Special marks

There are certain marks that Vim sets automatically (which you are able to overwrite yourself, but probably won't need to).

For example (paraphrased from Vim's help):

    `[` and `]`: jump to the first or last character of the previously changed or 
                 yanked text.  {not in Vi}

    `<` and `>`: jump to the first or last line (with `'`) or character (with 
                 <code>`</code>) of the last selected Visual area in the current 
                 buffer.  For block mode it may also be the last character in the 
                 first line (to be able to define the block).  {not in Vi}.

More, from Vim's built-in help:


    ''  ``              To the position before the latest jump, or where the
                        last "m'" or "m`" command was given.  Not set when the
                        :keepjumps command modifier was used.
                        Also see restore-position.


    '"  `"              To the cursor position when last exiting the current
                        buffer.  Defaults to the first character of the first
                        line.  See last-position-jump for how to use this
                        for each opened file.
                        Only one position is remembered per buffer, not one
                        for each window.  As long as the buffer is visible in
                        a window the position won't be changed.
                        {not in Vi}.

    '.  `.              To the position where the last change was made.  The
                        position is at or near where the change started.
                        Sometimes a command is executed as several changes,
                        then the position can be near the end of what the
                        command changed.  For example when inserting a word,
                        the position will be on the last character.
                        {not in Vi}

    '"  `"              To the cursor position when last exiting the current
                        buffer.  Defaults to the first character of the first
                        line.  See last-position-jump for how to use this
                        for each opened file.
                        Only one position is remembered per buffer, not one
                        for each window.  As long as the buffer is visible in
                        a window the position won't be changed.
                        {not in Vi}.

    '^  `^              To the position where the cursor was the last time
                        when Insert mode was stopped.  This is used by the
                        gi command.  Not set when the :keepjumps command
                        modifier was used.  {not in Vi}

Additionally, the characters `(`,`)`,`{`, and `}` are marks which jump to the same position as would their normal-mode commands – that is, `'}` does the same thing in normal mode as `}`.

## Jump to specific line
To jump to a specific line with colon number. To jump to the first line of a file use

    :1
To jump to line 23

    :23

