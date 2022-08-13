---
title: "Macros"
slug: "macros"
draft: false
images: []
weight: 9880
type: docs
toc: true
---

## Recording a macro
One way to create a macro is to *record* it.

Start recording a macro and save it to a register (in this example, we'll use `a`, but it can be any register you could normally yank text to):

    qa

Then run the commands you want to record in the macro (here, we'll surround the contents of a line with `<li>` tags):

    I<li><ESC>A</li>

When we're finished with the commands we want to record in the macro, stop the recording:

    q

Now, any time we want to execute the recorded sequence of commands stored in `a`, use:

    @a

and vim will repeat the recorded sequence.

Next time you would like to repeat the last macro that was used you can double type `@`:

    @@

And as a extra bonus it is good to remember that if you put a number before a command it will repeat it that many times. So, you repeat the macro saved in register `a` 20 times with:

    20@a

## Editing a vim macro
Sometimes you will make a mistake with a lengthy macro, but would rather edit it than re-record it entirely. You can do this using the following process:

1. Put the macro on an empty line with `"<register>p`.

   If your macro is saved in register `a`, the command is `"ap`.

2. Edit the macro as needed.

3. Yank the macro into the correct register by moving the cursor to the beginning of the line and using `"<register>y$`.

   You can re-use the original register or use another one. If you want to use register `b`, the command is `"by$`. or by using `"<register>d$` (deletes the unused line) 

## What is a macro?
A macro is a series of keystrokes meant to be "played back" by Vim without any delay. Macros can be stored in registers or variables, bound to keys, or executed on the command line.

Here is a simple macro that uppercases the third `word` on a line:

    0wwgUiw

That macro could be *recorded* into register `q`:

    qq         start recording into register q
    0wwgUiw    
    q          stop recording

or saved directly into register `q`:

    :let @q = '0wwgUiw'

to be played back with:

    @q

But it could also be typed directly in the command-line:

    :normal 0wwgUiw

for instant playback via the `:normal` command.

Or put into a variable:

    :let myvar = '0wwgUiw'

to be played back with:

    @=myvar

Or saved as a mapping:

    nnoremap <key> 0wwgUiw

to be played back by pressing `<key>`.

If you want to store a macro for later reuse you can type in insert mode:

    <C-r>q

This inserts the macro in register `q` (in this example: `0wwgUiw`). You can use this 
output e.g. to define the macro in your `vimrc`:

    let @q='0wwgUiw'

Doing so the register `q` is initialized with this macro every time you start vim.

## Recursive Macros
Vim macros can also be recursive. This is useful for when you need to act on every line (or other text object) till the end of the file.

To record a recursive macro, start with an empty register. (A register can be emptied using `q<register>q`.) 

Choose a consistent starting point on each line to start and finish. 

Before finishing recording, invoke the macro itself as the last command. (This is why the register must be empty: so it'll do nothing, as the macro doesn't exist yet).

Example, given the text:

    line 1
    line 2
    line 3
    foo bar
    more random text
    .
    .
    .
    line ???

In normal mode, with the cursor on the first line and a empty register `a`, one could record this macro:

    qaI"<Esc>A"<Esc>j@aq

Then with a single invocation of `@a`, all the lines of the file would be now inside double quotes.

## Record and replay action (macros)
with `q` command we could simplify a lot of tedious work in vim.

example 1. generate array sequence (1 to 20).


**STEP 1.** 
press `i` to enter insert mode, input `1`

```
1
```

**STEP 2.** **Record** following action: "append the last number to the next line, and increment the number"

 1. type `esc` to exit input mode 
 2. type `qa` to enter record mode, using buffer `a`
 3. type `yy` and `p` to copy current line and paste it as the next line
 4. type `ctrl` + `a` to increment number
 5. type `q` again to finish record

```
1
2
```

**STEP 3.** **Replay** action 18 times.

type `18@a` to replay action 3 and action 4 in step 2.

```
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
``` 


