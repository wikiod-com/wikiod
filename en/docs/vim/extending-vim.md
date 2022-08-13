---
title: "Extending Vim"
slug: "extending-vim"
draft: false
images: []
weight: 9917
type: docs
toc: true
---

A plugin is a script or set of scripts that changes Vim's default behavior, either by adding non-existing features or by extending existing features.

Often added "non-existing features" include:

- commenting,
- indentation detection,
- autocompletion,
- fuzzy-matching,
- support for a specific language,
- etc.

Often extended "existing features" include:

- omni-completion,
- text-objects & motions,
- yanking & putting,
- status line,
- search & replace,
- buffer/window/tab page switching,
- folding,
- etc.

## Vundle
[Vundle](https://github.com/VundleVim/Vundle.vim) is a plugin manager for Vim.

# Installing Vundle
(Full installation details can be found in the [Vundle Quick Start](https://github.com/VundleVim/Vundle.vim%23quick-start))

1. Install [Git](http://git-scm.com/) and clone Vundle into `~/.vim/bundle/Vundle.vim`.
2. Configure plugins by adding the following to the top of your `.vimrc`, adding or removing plugins as necessary (the plugins in the list are merely for illustration purposes)

        set nocompatible              " be iMproved, required
        filetype off                  " required

        " set the runtime path to include Vundle and initialize
        set rtp+=~/.vim/bundle/Vundle.vim
        call vundle#begin()
        " alternatively, pass a path where Vundle should install plugins
        "call vundle#begin('~/some/path/here')

        " let Vundle manage Vundle, required
        Plugin 'VundleVim/Vundle.vim'

        " All of your Plugins must be added before the following line
        call vundle#end()            " required
        filetype plugin indent on    " required
        " To ignore plugin indent changes, instead use:
        "filetype plugin on

        "place non-Plugin stuff after this line

3. Install Plugins: by launching Vim and running `:PluginInstall`.


# Supported Plugin Formats
The following are examples of different formats supported. Keep Plugin commands between `vundle#begin` and `vundle#end`.

| Plugin Location | Usage |
| ----------- | ------------ |
| plugin on GitHub | `Plugin 'tpope/vim-fugitive'` |
| plugin from http://vim-scripts.org/vim/scripts.html | `Plugin 'L9'` |
| Git plugin not hosted on GitHub | `Plugin 'git://git.wincent.com/command-t.git'` |
| git repos on your local machine (i.e. when working on your own plugin) | `Plugin 'file:///home/gmarik/path/to/plugin'` |
|
| The sparkup vim script is in a subdirectory of this repo called vim. Pass the path to set the runtimepath properly. | `Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}` |
| Install L9 and avoid a Naming conflict if you've already installed a different version somewhere else. | `Plugin 'ascenator/L9', {'name': 'newL9'}` |

Working on a shared account, for example, on cluster head node can raise issues from the point of disk usage by `.vim` directory. There are a couple of packages which take considerable amount of disk space, for example [YCM](https://github.com/Valloric/YouCompleteMe). So please choose your `Vundle` plugin directory wisely, and its very easy to do so by setting `rtp`. And also if you are planning to install any vim plugin, don't directly do `git clone` in the `bundle` directory. Use the Vundle way. 

## Pathogen
[vim-pathogen][1] is a `runtimepath` manager created by Tim Pope to make it easy to install plugins and runtime files in their own private directories.

Installing Pathogen
===================

1. Put pathogen in `~/.vim/bundle` (here with Git, but it's not mandatory):
  
       git clone https://github.com/tpope/vim-pathogen.git
  
2. Add the following lines to the top of your `.vimrc`:

        " enable vim-pathogen
        runtime bundle/vim-pathogen/autoload/pathogen.vim
        execute pathogen#infect()

- the `runtime` directive specifies the path to the autoload script of `vim-pathogen`; 
- `execute pathogen#infect()` initiates it.

Once initiated, Pathogen will automatically start a sweep through the folders in `~/.vim/bundle` and load the plugin from each of them.

Using Pathogen
==============

1. Put the top-level directory of your plugin in `~/.vim/bundle/` to make it available next time you start Vim.
2. Run `:Helptags` to index your new plugin's documentation.

Benefits
========

- Each plugin resides in its own directory under `~/.vim/bundle/`.
- Your `.vimrc` stays clean from the configuration needed to load plugins.

The effort needed to "manage" a plugin is thus reduced to:

- **put** its top-level directory under `~/.vim/bundle/` to *install* it,
- **replace** its top-level directory to *update* it,
- **delete** its top-level directory to *uninstall* it.

How you perform those three actions (manually, via an automation tool, with Git/Svn/Hg/whatever…) is completely up to you.

  [1]: https://github.com/tpope/vim-pathogen

## How plugins work
A plugin could present itself as a single file containing 30 lines of vimscript or as 20MB of vimscript/python/ruby/whatever split into many files across a dozen of directories that depends on a number of external tools.

The former is obviously easy to install and manage but the latter could pause quite a challenge.

## The principle

The `'runtimepath'` option tells Vim where to look for runtime scripts. The default value makes Vim look for scripts into the following directories *in order*:

* on UNIX-like systems

  - **`$HOME/.vim/`**
  - `$VIM/vimfiles/`
  - `$VIMRUNTIME/`
  - `$VIM/vimfiles/after/`
  - **`$HOME/.vim/after/`**
                    
* on Windows

  - **`$HOME/vimfiles/`**
  - `$VIM/vimfiles/`
  - `$VIMRUNTIME/`
  - `$VIM/vimfiles/after/`
  - **`$HOME/vimfiles/after/`**

Of the directories above, only install plugins into the ones in bold. The others will cause instability for no good reason. Installing a plugin boils down to placing each of its components in the right directory under `$HOME/.vim/` or `$HOME/vimfiles/`.

## The manual method
Single file plugin
==================
 Put the file under `$HOME/.vim/plugin` or `$HOME/vimfiles/plugin`

 This would source the plugin on startup of Vim. Now the user could use everything defined in it.
If the plugin however needs activation, the user either has to execute the command themselves whenever they want to use it, or add the command to `.vimrc`

Bundle
=======
A bundle is a directory structure that the plugin uses. It consists of all the files of the plugin under the appropriate sub-directories.

To install such a plugin the sub-directories should be merged with their counterparts in `$HOME/.vim/plugin`. This approach however leads to mixing of the files of different plugins in the same directories and could possibly lead to namespace problems.

Another approach is to copy the entire directory into `$HOME/.vim/bundle`.

When using this approach there should be at least one  `.vim` file under the `$HOME/.vim/bundle/autoload` directory. These files would be sourced by vim on startup.

**Note:** Depending on the operating system of the user the prefix of all paths might be `$HOME/vimfiles`. For more details see [How plugins work][1]


  [1]: https://www.wikiod.com/vim/extending-vim#How plugins work

## VAM
https://github.com/MarcWeber/vim-addon-manager

## The future: packages
See `:help packages`.

