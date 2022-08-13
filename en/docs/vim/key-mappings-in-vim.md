---
title: "Key Mappings in Vim"
slug: "key-mappings-in-vim"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

Updating Vim key mappings allows you to solve two kinds of problems: Re-assigning key commands to letters that are more memorable or accessible, and creating key commands for functions which have none.  Here you will learn about the various ways to [re]map key commands, and the context to which they apply *(i.e. vim modes)*

## Basic mapping
## map Overview

A key sequence can be re-mapped to another key sequence using one of the `map` variants.

As an example, the following typical `map` will exit *Insert mode* when you press <kbd>j</kbd><kbd>k</kbd> in quick sequence:

    :inoremap jk <Esc>

## map Operator

There are multiple variants of `:map` for different modes.

Commands                        | Modes
--------------------------------|-----------------------------------------
`:map`, `:noremap`, `:unmap`    | Normal, Visual and Operator-pending mode
`:map!`, `:noremap!`, `:unmap!` | Insert and Command-line mode
`:nmap`, `:nnoremap`, `:nunmap` | Normal mode
`:imap`, `:inoremap`, `:iunmap` | Insert and Replace mode
`:vmap`, `:vnoremap`, `:vunmap` | Visual and Select mode
`:xmap`, `:xnoremap`, `:xunmap` | Visual mode
`:smap`, `:snoremap`, `:sunmap` | Select mode
`:cmap`, `:cnoremap`, `:cunmap` | Command-line mode
`:omap`, `:onoremap`, `:ounmap` | Operator pending mode

Usually, [you should use the `:noremap` variants](http://learnvimscriptthehardway.stevelosh.com/chapters/05.html); it makes the mapping immune to remapping and recursion.
    
## map Command
    
* You can display all mappings using `:map` (or one of the variations above).
* To display the current mapping for a specific key sequence, use `:map <key>` where `<key`> is a sequence of keys
* Specials keys like <kbd>Esc</kbd> are mapped using special `<>` notation, like `<Esc>`. For the full list of key codes, see http://vimdoc.sourceforge.net/htmldoc/intro.html#keycodes
* `:nmapclear` - Clear all normal mode maps
* `:nunmap` - Unmap a normal mode map
* You can configure the maximum time between keys of a sequence by changing the `timeout` and `ttimeout` variables

## Examples

* `imap jk <Esc>`: typing `jk` in insert mode will bring you back to normal mode
* `nnoremap tt :tabnew<CR>`: typing `tt` in normal mode will open a new tab page
* `nnoremap <C-j> <C-w>j`: typing `<C-j>` in normal mode will make you jump to the window below and to the left
* `vmap <C-c> \cc`: typing `<C-c>` in visual mode will execute `\cc` (NERDCommenter command to comment the line). As this relies on a plugin mapping, you cannot use `:vnoremap` here!

> futher reading [here](http://vim.wikia.com/wiki/Mapping_keys_in_Vim_-_Tutorial_(Part_1))


## Map leader key combination
The leader key could be used as a way to create a mapping with a key-binding that can be overridden by the end user.

The leader is the <kbd>\\</kbd> key by default. In order to override it, the end-user would have to execute `:let g:mapleader='somekey(s)'` before defining the mapping.

In a typical scenario, the mapleader is set in the `.vimrc`, and plugins use `<Leader>` in the keybinding part of their mappings to have them customizable.

In the plugin, we would define mappings with:

    :nnoremap <Leader>a somecomplexaction

This would map the <kbd>somecomplexaction</kbd> action to the <kbd>\\</kbd>+<kbd>a</kbd> key combination.

The <kbd>a</kbd> action without the leader does not change.

It's also possible to use `<Plug>Mappings` to leave more room to customise plugins keybindings.

## Illustration of Basic mapping (Handy shortcuts).
In most text editors, the standard shortcut for saving the current document is <kbd>Ctrl</kbd>+<kbd>S</kbd> (or <kbd>Cmd</kbd>+<kbd>S</kbd> on macOS).

Vim doesn't have this feature by default but this can be mapped to make things easier. Adding the following lines in `.vimrc` file will do the job.

    nnoremap <c-s> :w<CR>
    inoremap <c-s> <c-o>:w<CR>

The `nnoremap` command maps <kbd>Ctrl</kbd>+<kbd>s</kbd> to `:w` (write current contents to file) command whereas the `inoremap` command maps the <kbd>Ctrl</kbd>+<kbd>S</kbd> to `:w` command and returns back to the insert mode (`<c-o>` goes into normal mode for one command and returns to insert mode afterwards, without altering cursor position which other solutions like `<esc>:w<cr>a` cannot ensure). 

Similarly,

    " This is commented, as Ctrl+Z is used in terminal emulators to suspend the ongoing program/process.
    " nnoremap <c-z> :u<CR>

    " Thus, Ctrl+Z can be used in Insert mode
    inoremap <c-z> <c-o>:u<CR>

    " Enable Ctrl+C for copying selected text in Visual mode
    vnoremap <c-c> <c-o>:y<CR>


PS: However it must be noted that <kbd>Ctrl</kbd>+<kbd>S</kbd> may not work as expected while using ssh (or PuTTY). The solution to this is not within the scope of this document, but can be found [Here][1].


  [1]: https://raamdev.com/2007/recovering-from-ctrls-in-putty/

