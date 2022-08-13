---
title: "Tips and tricks to boost productivity"
slug: "tips-and-tricks-to-boost-productivity"
draft: false
images: []
weight: 9631
type: docs
toc: true
---

## Syntax
- :set relativenumber
- :set number
- :set nonumber / :set nonu
- :pwd


This automatic reload will only happen if you edit your `vimrc` in a version full version of vim which supports `autocmd`.

## Turn On Relative Line Numbers
To delete some lines of text when you don't know exact number of lines to delete, you try `10dd` , `5dd` , `3dd` until you remove all the lines.

Relative line numbers solves this problem, suppose we have a file containing :

    sometimes, you see a block of 
    text. You want to remove 
    it but you 
    cannot directly get the 
    exact number of 
    lines to delete 
    so you try 
    10d , 5d 
    3d until 
    you
    remove all the block.

Enter NORMAL mode: `Esc`

Now, execute `:set relativenumber`. Once done the file will be displayed as:

<pre>
<b>3</b> sometimes, you see a block of
<b>2</b> text. You want to remove
<b>1</b> it but you
<b>0</b> cannot directly get the
<b>1</b> exact number of
<b>2</b> lines to delete
<b>3</b> so you try
<b>4</b> 10d , 5d
<b>5</b> 3d until
<b>6</b> you
<b>7</b> remove all the block.
</pre>

where `0` is the line number for the current line and it also shows the real line number in front of relative number, so now you don't have to estimate the numbers of lines from the current line to cut or delete or worse count them one by one.

You can now execute your usual command like `6dd` and you are sure about the number of lines.

You can also use the short form of the same command `:set rnu` to turn on relative numbers and `:set nornu` to turn off the same.

If you also `:set number` or have `:set number` already on, you'll get the line number of the line in which the cursor is at. 

<pre>
<b>3</b> sometimes, you see a block of 
<b>2</b> text. You want to remove 
<b>1</b> it but you 
<b>4</b> cannot directly get the 
<b>1</b> exact number of 
<b>2</b> lines to delete 
<b>3</b> so you try 
<b>4</b> 10d , 5d 
<b>5</b> 3d until 
<b>6</b> you 
<b>7</b> remove all the block. 
</pre>

## Mappings for exiting Insert mode
A lot of Vim users find the <kbd>Esc</kbd> too hard to reach, and end up finding another mapping that's easy to reach from the home row. Note that <kbd>Ctrl</kbd>-<kbd>[</kbd> may be equivalent to <kbd>Esc</kbd> on an English keyboard, and is much easier to reach.

# <kbd>j</kbd><kbd>k</kbd>
    inoremap jk <ESC> 
   
This one is really easy to trigger; just smash your first two fingers on the home row at the same time. It's also hard to trigger accidentally since "jk" never appears in any English word, and if you're in normal mode it doesn't do anything. If you don't type "blackjack" too much, then consider also adding `inoremap kj <ESC>` so you don't need to worry about timing of the two keys. 

# Caps Lock

## Linux
On Linux, you can use `xmodmap` to make <kbd>Caps Lock</kbd> do the same thing as <kbd>Esc</kbd>. Put this in a file:

    !! No clear Lock 
    clear lock
    !! make caps lock an escape key
    keycode 0x42 = Escape

Then run `xmodmap file`. This remaps <kbd>Caps Lock</kbd> to <kbd>Esc</kbd>.

## Windows

On Windows you can use [SharpKey] or [AutoHotkey].

  [sharpkey]: https://sharpkeys.codeplex.com/
  [autohotkey]: https://www.autohotkey.com/

## macOS

If you have macOS 10.12.1 or later, you can remap Caps Lock to Escape using System Preferences. Select Keyboard, go to the Keyboard tab, and click Modifier Keys. 

[![Keyboard Preferences][1]][1]

  [1]: https://i.stack.imgur.com/JsGip.png

## Using the path completion feature inside Vim
This is very common, you memorize a path to a file or folder, you open up Vim and try to write what you've just memorized, but you are not 100% sure it's correct, so you close the editor and start over.

When you want the path completion feature,  and you have a file `/home/ubuntu/my_folder/my_file` and you are editing another file referencing to the path of the previous one:

Enter insert mode: 
<kbd>insert</kbd> or do it the way you want.
Next, write `/h`. When the cursor is under `h`,
press <kbd>Ctrl</kbd><kbd>x</kbd> and then <kbd>Ctrl</kbd><kbd>f</kbd> so the editor will complete it to `/home/` because the pattern `/h` is unique

Now, suppose you have two folders inside `/home/ubuntu/` called `my_folder_1` `my_folder_2`

and you want the path `/home/ubuntu/my_folder_2`

as usual:

Enter insert mode

write `/h` and press <kbd>Ctrl</kbd><kbd>x</kbd> and then <kbd>Ctrl</kbd><kbd>f</kbd> . Now you have `/home/`
Next add u after /home/ and press <kbd>Ctrl</kbd><kbd>x</kbd> and then <kbd>Ctrl</kbd><kbd>f</kbd> . Now, you have `/home/ubuntu/` because that path is unique.
Now, write `my` after `/home/ubuntu/` and press <kbd>Ctrl</kbd><kbd>x</kbd> and then <kbd>Ctrl</kbd><kbd>f</kbd> . The editor will complete your word until `my_folder_` and you will see the directory tree so use the arrow keys to choose the one you want.

## Viewing line numbers
To view line numbers from Default view enter

    :set number

To hide line numbers

    :set nonumber

There is also a shortcut for above. `nu` is same as `number`.

    :set nonu

To hide line numbers, we can also use
    
    :set nu!

## Write a file if you forget to `sudo` before starting vim
This command will save the open file with sudo rights

    :w !sudo tee % >/dev/null

You can also map `w!!` to write out a file as root

    :cnoremap w!! w !sudo tee % >/dev/null


## Quick "throwaway" macros
Add this to your vimrc:

    nnoremap Q @q

To start recording the "throwaway" macro, use `qq`. To finish recording hit `q` (in normal mode for both).

To execute the recorded macro, use `Q`.

This is useful for macros that you need to repeat many times in a row but won't be likely to use again afterward.

## How to know the directory and/or the path of the file you are editing
To know the path of the directory your file is in you can use:

<kbd>Esc</kbd> to enter command mode

    :pwd

This will print the path to the directory at the bottom of the editor, like this

    I'm a ninja
    ~
    ~
    ~
    ~
    ~
    /home/ubuntu/myfolder                                                1,5         All


Now, if you want to know the file name you are editing relatively to the vim working directory, you can use:

<kbd>Esc</kbd> to enter command mode
<kbd>CTRL</kbd><kbd>G</kbd>


    I'm a ninja
    ~
    ~
    ~
    ~
    ~
    "myfile"[Modified][New file] 1 line --100%--                         1,5         All

Finally to get the absolute path to the file you are editing, use

<kbd>Esc</kbd> to enter command mode,

<kbd>1</kbd> and then <kbd>CTRL</kbd><kbd>G</kbd>


    I'm a ninja
    ~
    ~
    ~
    ~
    ~
    "~/myfolder/myfile"[Modified][New file] 1 line --100%--              1,5         All


## Automatically reload vimrc upon save
To automatically reload `vimrc` upon save, add the following to your `vimrc`:

    if has('autocmd') " ignore this section if your vim does not support autocommands
        augroup reload_vimrc
            autocmd!
            autocmd! BufWritePost $MYVIMRC,$MYGVIMRC nested source %
        augroup END
    endif

and then for the last time, type:

    :so $MYVIMRC

The next time you save your `vimrc`, it will be automatically reloaded.

`nested` is useful if you're using vim-airline. The process of loading airline triggers some autocommands, but since you're in the process of executing an autocommand they get skipped. `nested` allows triggering nested autocommands and allows airline to load properly.


## Command line completion
set `wildmenu` to turn on completion suggestions for command line.  
Execute the following

    set wildmenu
    set wildmode=list:longest,full

Now if you do say, `:color `<kbd>tab</kbd>,

You'll get

>     256-jungle  Benokai  BlackSea  C64  CandyPaper  Chasing_Logic  ChocolateLiquor  
>     :color 0x7A69_dark

## Search within a function block
To search for text `foo` within a `{}` block surrounding the cursor use the following command (`<ESC>` - escape key, `<CR>` - enter key) :

    vi{<ESC>/\%Vfoo<CR>

now you can jump between the matches within the block by pressing `n` and `p`. If you have `hlsearch` option enabled this will highlight all the matches.
`\%V` is a special search pattern part, that tells vim to search only in the visually selected area. You can also make a mapping like this:

    :vnoremap g/ <ESC>/\%V

After this the above command is shortened to the following:

    vi{g/foo<CR>

Another useful trick is to print all the lines containing the pattern: 

    vi{
    :'<,'>g/foo/#

The `'<,'>` range is inserted automatically.

See `:help range` and `:help :g`.

## Copy, move or delete found line
A lot of users find themselves in a situation where they just want to copy, move or delete a line quickly and return to where they were.

Usually, if you'd want to move a line which contains the word `lot` below the current line you'd type something like:

    /lot<Esc>dd<C-o>p

But to boost productivity you can use this shortcut in these cases:

    " It's recommended to turn on incremental search when doing so
    set incsearch

    " copy the found line
    cnoremap $t <CR>:t''<CR>
    " move the found line
    cnoremap $m <CR>:m''<CR>
    " delete the found line
    cnoremap $d <CR>:d<CR>``

So a search like this:

    /lot$m

would move the line which contains `lot` below the line your cursor was on when you started the search.



