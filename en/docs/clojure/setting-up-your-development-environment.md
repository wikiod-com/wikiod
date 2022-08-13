---
title: "Setting up your development environment"
slug: "setting-up-your-development-environment"
draft: false
images: []
weight: 9913
type: docs
toc: true
---

## Emacs
To setup Emacs for working with Clojure, install [`clojure-mode`][1] and [`cider`](https://cider.readthedocs.io/en/latest/) package from melpa:

    M-x package-install [RET] clojure-mode [RET]
    M-x package-install [RET] cider [RET]

Now when you open a `.clj` file, run `M-x cider-jack-in` to connect to a REPL. Alternatively, you can use `C-u M-x` (cider-jack-in) to specify the name of a `lein` or `boot` project, without having to visit any file in it. You should now be able to evaluate expressions in your file using `C-x C-e`.

Editing code in lisp-like languages is much more comfortable with a paren-aware editing plugin. Emacs has several good options.

 - [`paredit`](https://www.emacswiki.org/emacs/ParEdit) A classic Lisp editing mode that has a steeper learning curve, but provides a lot of power once mastered.

    `M-x package-install [RET] paredit [RET]`


 - [`smartparens`](https://github.com/Fuco1/smartparens) A newer project with similar goals and usage to `paredit`, but also provides reduced capabilities with non-Lisp languages.

    `M-x package-install [RET] smartparens [RET]`


 - [`parinfer`](http://shaunlebron.github.io/parinfer/) A much simpler Lisp editing mode that operates mainly via inferring proper paren nesting from indentation.

    Installation is more involved, see the Github page for `parinfer-mode` for [setup instructions](https://github.com/edpaget/parinfer-mode).

To enable `paredit` in `clojure-mode`: 

    (add-hook 'clojure-mode-hook #'paredit-mode)

To enable `smartparens` in `clojure-mode`:

    (add-hook 'clojure-mode-hook #'smartparens-strict-mode)


  [1]: http://github.com/clojure-emacs/clojure-mode

## IntelliJ IDEA + Cursive
[Download][1] and install the latest IDEA version.

[Download][2] and install the latest version of the Cursive plugin.

After restarting IDEA, Cursive should be working out of the box. Follow the [user guide][3] to fine-tune appearance, keybindings, code style etc.

**Note:** Like [`IntelliJ`][1], [`Cursive`][1] is a commercial product, with a 30-day evaluation period. Unlike [`IntelliJ`][1], it doesn't have a community edition. Free non-commercial licences are available to individuals for non-commercial use, including personal hacking, open-source and student work. The licence is valid for 6 months and can be renewed.

  [1]: https://www.jetbrains.com/idea/
  [2]: https://cursive-ide.com/
  [3]: https://cursive-ide.com/userguide/

## Atom
Install Atom for your distribution [here](https://atom.io/).

After that run the following commands from a terminal:

```bash
apm install parinfer
apm install language-clojure
apm install proto-repl
```

## Spacemacs + CIDER
[Spacemacs][] is a distribution of emacs that comes with a lot of packages preconfigured and easily installed. Also, it is very friendly for those who are familiar with vim style of editing. Spacemacs provides a [CIDER-based Clojure layer][CIDER].

  [Spacemacs]: http://spacemacs.org/
  [CIDER]: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/clojure

To install and configure it for use with Clojure, first install emacs. Then make a backup of your previous configurations:

```
$ mv ~/.emacs.d ~/.emacs.d.backup
```

Then clone the spacemacs' repository:

```
$ git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
```

Now, open emacs. It will ask you some questions regarding your preferences. Then it downloads more packages and configures your emacs. After that the spacemacs is installed, you are ready to add Clojure support. Press `SPC f e d` to open your `.spacemacs` file for configuration. Find `dotspacemacs-configuration-layers` in the file, under it there is an open paren. Anywhere in between the parens in a new line type `clojure`.

    (defun dotspacemacs/layers ()
      (setq-default
       ;; ...
       dotspacemacs-configuration-layers
       '(clojure
         ;; ...
        )
       ;; ...
    ))

Press `SPC f e R` to save and install the clojure layer. Now, in any `.clj` file if you press `, s i` spacemacs will try to spawn a new clojure REPL connection to your project, and if it succeeds it will show in the statusbar, which afterwards you can press `, s s` to open a new REPL buffer to evaluate your codes.

For more information about spacemacs and cider contact their documentations. [Spacemacs docs][1], [Cider docs][2]

  [1]: http://spacemacs.org/doc/DOCUMENTATION.html
  [2]: http://cider.readthedocs.io/en/latest/


## Vim
Install the following plugins using your favourite plugin manager:

1. [fireplace.vim](https://github.com/tpope/vim-fireplace): Clojure REPL support
2. [vim-sexp](https://github.com/guns/vim-sexp): For taming [those hugs around your function calls](https://twitter.com/sckottie/status/481553390917279745)
3. [vim-sexp-mappings-for-regular-people](https://github.com/tpope/vim-sexp-mappings-for-regular-people): Modified sexp mappings that are a little easier to bear
4. [vim-surround](https://github.com/tpope/vim-surround): Easily delete, change, add "surroundings" in pair
5. [salve.vim](https://github.com/tpope/vim-salve): Static Vim support for Leiningen and Boot.
6. [rainbow_parentheses.vim](https://github.com/kien/rainbow_parentheses.vim): Better Rainbow Parentheses

and also relate to syntax highlighting, omni completion, advanced highlighting and so on:

1. [vim-clojure-static](https://github.com/guns/vim-clojure-static) (if you have a vim older than 7.3.803, newer versions ship with this)
2. [vim-clojure-highlight](https://github.com/guns/vim-clojure-highlight)

Other options in place of vim-sexp include [paredit.vim](https://github.com/vim-scripts/paredit.vim) and [vim-parinfer](https://github.com/bhurlow/vim-parinfer).

## Light Table
Light Table is a good editor to learn, experiment and run Clojure projects.<br>
You can also run lein/boot projects by opening `project.clj` file. It will load all project dependencies.

It supports inline evalution, plugins and much more, so there's no need to add print statements and check the output in the console. You can run individual lines or code blocska by pressing `ctrl + enter`. To run partial code, select the code and press `ctrl + enter`. check the following screenshot for how you can use Light Table to learn and experiment with Clojure code.

[![enter image description here][1]][1]

Pre-built binaries of Light Table can be found [here][2]. No further setup is required.

Light Table is able to automatically locate your Leiningen project and evaluate your code. If you don't have Leiningen installed, install it using the instructions [here][3].

Documentation: [docs.lighttable.com][4]


  [1]: http://i.stack.imgur.com/f2C4E.png
  [2]: http://lighttable.com/
  [3]: https://www.wikiod.com/clojure/getting-started-with-clojure#Installation and Setup
  [4]: http://docs.lighttable.com/

