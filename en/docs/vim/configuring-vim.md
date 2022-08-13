---
title: "Configuring Vim"
slug: "configuring-vim"
draft: false
images: []
weight: 9869
type: docs
toc: true
---

## Files and directories
Whatever you do to customize Vim, it should NEVER happen outside of `$HOME`:

- on Linux, BSD and Cygwin, `$HOME` is usually `/home/username/`,
- on Mac OS X, `$HOME` is `/Users/username/`,
- on Windows, `$HOME` is usually `C:\Users\username\`.

The canonical location for your `vimrc` and your `vim` directory is at
the root of that `$HOME` directory:

- on Unix-like systems

      $HOME/.vimrc       <-- the file
      $HOME/.vim/        <-- the directory

- on Windows

      $HOME\_vimrc       <-- the file
      $HOME\vimfiles\    <-- the directory

The layout above is guaranteed to work, now and in the future.

Vim 7.4 made it possible to keep your lovely `vimrc` **inside** your `vim`
directory. It is really a good idea, if only because it makes it easier to
move your config around.

If you use 7.4 exclusively, the following will be enough:

- on Unix-like systems

      $HOME/.vim/vimrc

- on Windows

      $HOME\vimfiles\vimrc

If you want the benefits of a self-contained `vim/` but use both 7.4 and an older version, or only an older version, the simplest, future-proof, solution is to put this
line *and only this line:*

    runtime vimrc

in this file:

- on Unix-like systems

      $HOME/.vimrc

- on Windows

      $HOME\_vimrc

and do your configuration in `$HOME/.vim/vimrc` or `$HOME\vimfiles\vimrc`.

## Mappings
- Don't put comments after mappings, it will break things.
- Use `:map <F6>` to see what is mapped to `<F6>` and in which mode.
- Use `:verbose map <F6>` to also see where it was last mapped.
- `:map` and `:map!` are too generic. Use `:n[nore]map` for normal mode mappings, `:i[nore]map` for insert mode, `:x[nore]map` for visual mode, etc.

## Recursive mappings

Use *recursive* mappings **only** if you intend to use other mappings in your mappings:

    nnoremap b     B
    nmap     <key> db

In this example, `b` is made to work like `B` in normal mode. Since we use `b` in a *recursive* mapping, pressing `<key>` will work like `dB`, not like `db`.

## Non-recursive mappings

Use *non-recursive* mappings **only** if you intend to use default commands in your mappings, which is almost always what you want:

    nnoremap <key> db

In this example, we use `b` in a *non-recursive* mapping so pressing key will always work like `db`, whether we remapped `b` or not.

## Executing a command from a mapping

    nnoremap <key> :MyCommand<CR>

## Executing multiple commands from a mapping

    nnoremap <key> :MyCommand <bar> MyOtherCommand <bar> SomeCommand<CR>

## Calling a function from a mapping

    nnoremap <key> :call SomeFunction()<CR>

## Mapping a `<Plug>`mapping

    map <key> <Plug>name_of_mapping

See `:help map-commands`, `:help key-notation` and `:help <plug>`.

see https://www.wikiod.com/vim/key-mappings-in-vim for futher read 

## Toggle line enumerating
To enable - type:

`:set number` or `:set nu`.

To disable - type:

`:set nonumber` or `:set nonu`.

To enable enumerating *relative* to the cursor location - type:

`:set relativenumber`.

To disable enumerating *relative* to the cursor location - type:

`:set norelativenumber`.

*Note:* To change whether the current line shows the actual line number or `0`, use the `:set number` and `:set nonumber` commands while the `relativenumber` attribute is active.

## The vimrc file
The `.vimrc` file (pronounced Vim-wreck) is a Vim configuration file. It holds commands that will be executed by Vim every time it starts.

By default the file is empty or non-existent; you can use it to customize your Vim environment.

To find out where Vim expects the vimrc file to be stored, open Vim and run:

    :echo $MYVIMRC

**Unix:** on a Unix system such as Mac or Linux your vimrc will be called `.vimrc` and usually be located in your home directory (`$HOME/.vimrc`).

**Windows:** on Windows it will be called `_vimrc` and located in your home directory (`%HOMEPATH%/_vimrc`).

On startup, Vim will search in multiple places for a vimrc file. The first that exists is used, the others are ignored. For a full reference see the `:h $MYVIMRC` documentation article.

## Which options can I use?
If you don't know which options you should use, you may be interested in the `:options` command.

This will open a split with all Vim options listed and with their current value displayed. There are 26 sections to display all options you can try.

e.g.

    4 displaying text
    
    scroll    number of lines to scroll for CTRL-U and CTRL-D
        (local to window)
         set scr=20
    scrolloff    number of screen lines to show around the cursor
         set so=5
    wrap    long lines wrap
         set nowrap    wrap

    ...


On a value line (e.g. `set nowrap`) you can press <kbd>CR</kbd> to toggle the value (if it's a binary value).
On an option line (e.g. `wrap long line wrap`) you can press <kbd>CR</kbd> to access the documentation for this option.

## Functions
- Don't forget the bang to allow Vim to overwrite that function next time you reload the script where the function is defined.
- Custom functions must start either with an uppercase character (global functions), or with `s:` (script local functions), or they must be prefixed with the name associated to the autoload plugin where they are defined (e.g. in `{&rtp}/autoload/foo/bar.vim` we could define `foo#bar#functionname()`).
- To be able to use the parameters in the function, use `a:parameter_name`. Variadic functions can be defined with the ellipsis `...`, to access the parameters use `a:000` (list of all parameters), or `a:0` (number of parameters equal to `len(a:000)`), `a:1` first unnamed parameters, and so on.
- Functions can be called like so: `:call MyFunction(param1, param2)`
- Every line in a function implicitly begins with a `:`, thus all the commands are colon commands
- To prevent the function from continuing its execution in case of error, it's best to annotate the function signature with `abort`

## Example

    function! MyFunction(foo, bar, ... ) abort
        return a:foo . a:bar . (a:0 > 0 ? a:1 : '')
    endfunction

## Script functions

If you only plan on using your function in the file where it's defined (either because you've broken a bigger function in smaller parts, or because you'll use it in a command, a mapping, ...), you can prefix it with `s:`, avoiding littering your global namespace with useless internal functions:

    function! s:my_private_function() " note we don't need to capitalize the first letter this time
        echo "Hi!"
    endfunction

### Using s:functions from mappings

If your script local function is going to be used in a mapping, you need to reference it using the special `<SID>` prefix:

    nnoremap <your-mapping-key> :call <SID>my_private_function()<CR>

See `:help user-functions`.

Note however, that since Vim 7, it's considered a best practice to define mappings abbreviations, commands and menus in (ft)plugins, and defining functions in autoload plugins -- except the functions the plugins need to use when they're loaded. This means that nowadays the need to call scripts local functions from mappings is not as pertinent as what it used to be.

## Color Schemes
Vim comes with several pre-installed color schemes. In Linux, the color schemes that come with Vim are stored in `/usr/share/vim/vim74/colors/` (where 74 is your version number, sans periods); MacVim stores them in `/Applications/MacVim.app/Contents/Resources/vim/runtime/colors`.

Changing Color Schemes
====

The `colorscheme` command switches the current color scheme. 

For instance, to set the color scheme to "robokai":

    :colorscheme robokai

The default color scheme is creatively named `default`, so, to return to it use

    :colorscheme default

To view all of the currently installed color schemes, type `:colorscheme` followed by <kbd>space</kbd> and then either <kbd>tab</kbd> or <kbd>ctrl</kbd><kbd>d</kbd>. 

Installing Color Schemes
===
 User-installed color schemes can be placed in `~/.vim/colors/`.  Once a color scheme is added to this directory, it will appear as an option to the `colorscheme` command.

To find new color schemes, there are sites like [vimcolors](http://vimcolors.com/) which contain a variety of color schemes. There are also tools like [vim.ink](http://vim.ink/) and [Vivify](http://bytefluent.com/vivify/) to aid you in creating your own color schemes, or you can create them by hand.

## Setting Options
Commonly you would use `:set` to set options to your liking in your `.vimrc`. There are many options that can be changed.

For example, in order to use spaces for indentation:
   
    :set expandtab
    :set shiftwidth=4
    :set softtabstop=4

## Syntax Highlighting
Switch syntax highlighting on, when the terminal has colors

    if &t_Co > 2 || has("gui_running")
        syntax on
    end

Show trailing whitespace and tabs. Showing tabs can be especially useful when looking for errors in Makefiles.

    set list listchars=tab:\|_,trail:.
    highlight SpecialKey ctermfg=DarkGray

## Options
There are three kinds of options:

- boolean options,
- string options,
- number options.

To check the value of an option,

- use `:set option?` to check the value of an option,
- use `:verbose set option?` to also see where it was last set.

## Setting boolean options

    set booloption      " Set booloption.
    set nobooloption    " Unset booloption.

    set booloption!     " Toggle booloption.

    set booloption&     " Reset booloption to its default value.

## Setting string options

    set stroption=baz   " baz

    set stroption+=buzz " baz,buzz
    set stroption^=fizz " fizz,baz,buzz
    set stroption-=baz  " fizz,buzz

    set stroption=      " Unset stroption.

    set stroption&      " Reset stroption to its default value.

## Setting number options

    set numoption=1     " 1

    set numoption+=2    " 1 + 2 == 3
    set numoption-=1    " 3 - 1 == 2
    set numoption^=8    " 2 * 8 == 16

## Using an expression as value

- using concatenation:

      execute "set stroption=" . my_variable

- using `:let`:

      let &stroption = my_variable

See `:help :set` and `:help :let`.

## Variables
Like most scripting languages, vimscript has variables.

One can define a variable with the `:let` command:

    let variable = value

and delete it with `:unlet`:

    unlet variable

In Vim, variables can be scoped by prepending a single letter and a colon to their name. Plugin authors use that feature to mimic options:

    let g:plugin_variable = 1

See `:help internal-variables`.

## Commands
- Don't forget the bang to allow Vim to overwrite that command next time you reload your vimrc.
- Custom commands must start with an uppercase character.

## Examples

    command! MyCommand call SomeFunction()
    command! MyOtherCommand command | Command | command

- See `:help user-commands`.

## Autocommand groups
- Autocommand groups are good for organization but they can be useful for debugging too. Think of them as small namespaces that you can enable/disable at will.

## Example

    augroup MyGroup
        " Clear the autocmds of the current group to prevent them from piling
        " up each time you reload your vimrc.
        autocmd!

        " These autocmds are fired after the filetype of a buffer is defined to
        " 'foo'. Don't forget to use 'setlocal' (for options) and '<buffer>'
        " (for mappings) to prevent your settings to leak in other buffers with
        " a different filetype.
        autocmd FileType foo setlocal bar=baz
        autocmd FileType foo nnoremap <buffer> <key> :command<CR>

        " This autocmd calls 'MyFunction()' everytime Vim tries to create/edit
        " a buffer tied to a file in /'path/to/project/**/'.
        autocmd BufNew,BufEnter /path/to/project/**/* call MyFunction()
    augroup END

See `:help autocommand`.

## Conditionals
    if v:version >= 704
        " Do something if Vim is the right version.
    endif

    if has('patch666')
        " Do something if Vim has the right patch-level.
    endif

    if has('feature')
        " Do something if Vim is built with 'feature'.
    endif

See `:help has-patch` and `:help feature-list`.

## Plugins
Vim plugins are addons that can be used to change or enhance functionality of vim.

There is a good list of plugins at [vimawesome][1]


  [1]: http://vimawesome.com/

