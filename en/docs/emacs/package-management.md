---
title: "Package Management"
slug: "package-management"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## Automatic Package Installation with use-package
    ;; disable automatic loading of packages after the init file
    (setq package-enable-at-startup nil)
    ;; instead load them explicitly
    (package-initialize)
    ;; refresh package descriptions
    (unless package-archive-contents
       (package-refresh-contents))
    
    ;;; use-package initialization
    ;;; install use-package if not already done
    (if (not (package-installed-p 'use-package))
        (progn
          (package-refresh-contents)
          (package-install 'use-package)))
    ;;; use-package for all others
    (require 'use-package)
    
    ;; install your packages
    (use-package helm
      :ensure t)
    (use-package magit
      :ensure t)

## Automatic package installation on emacs start-up
<!-- language: lang-el -->

    ;; package.el is available since emacs 24
    (require 'package)
    
    ;; Add melpa package source when using package list
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
    
    ;; Load emacs packages and activate them
    ;; This must come before configurations of installed packages.
    ;; Don't delete this line.
    (package-initialize)
    ;; `package-initialize' call is required before any of the below
    ;; can happen
    
    ;; If you do not put the "(package-initialize)" in your ~/.emacs.d/init.el (or
    ;; ~/.emacs), package.el will do it for you starting emacs 25.1.
    
    ;; Below manual maintenance of packages should not be required starting emacs
    ;; 25.1 with the introduction of `package-selected-packages' variable. This
    ;; variable is automatically updated by emacs each time you install or delete a
    ;; package. After this variable is synced across multiple machines, you can
    ;; install the missing packages using the new
    ;; `package-install-selected-packages' command in emacs 25.1.
    
    ;; To clarify, below technique is useful on emacs 24.5 and older versions.
    ;; Request some packages:
    (defconst my-package-list '()
      "List of my favorite packages")
    
    (defvar my-missing-packages '()
      "List populated at each startup that contains the list of packages that need
    to be installed.")
    
    (dolist (p my-package-list)
      (when (not (package-installed-p p))
        (add-to-list 'my-missing-packages p)))
    
    (when my-missing-packages
      (message "Emacs is now refreshing its package database...")
      (package-refresh-contents)
      ;; Install the missing packages
      (dolist (p my-missing-packages)
        (message "Installing `%s' .." p)
        (package-install p))
      (setq my-missing-packages '()))   

---

# References

- [Comparison of package repos][1]
- [Blog post on user selected packages feature in emacs 25.1][2]

[1]: http://emacs.stackexchange.com/q/268/115
[2]: http://endlessparentheses.com/new-in-package-el-in-emacs-25-1-user-selected-packages.html


## Automatic package management using Cask
[Cask][1] is a project management tool which can be also used to easily manage your local emacs configuration.

Installing cask is easy. You can either run the following command on the command-line:

     curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

Or if you are on a mac, you can install it using `homebrew`:

    brew install cask

Once installed, you create a `Cask` file. Cask files list all package dependencies which should be included in your configuration. You can create a new Cask file at the root of your `~/.emacs` directory.

You will also need to initialize Cask in your `~/.emacs.d/init.el`. If you installed using homebrew, add these lines:

    (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
    (cask-initialize)

Or you can supply the path to cask, if you used the install script:

    (require 'cask "~/.cask/cask.el")
    (cask-initialize)

A simple Cask file looks like this:

    (source gnu)
    (source melpa)
    
    (depends-on "projectile")
    (depends-on "flx")
    (depends-on "flx-ido")

Here we are specifying source repositories to look for packages in. Then we are specifying that we want the `projectile`, `flx`, and `flx-ido` packages installed. 

Once you have a Cask file, you can install all the dependencies with the follwoing command on the command-line:

    cask install

  [1]: https://github.com/cask/cask

## Automatic Package Management with el-get
[el-get](https://github.com/dimitri/el-get) is an open source package management system for GNU Emacs.  el-get works with `melpa`, as well as with many common version control systms.  Its documentation includes a simple self-installer for your `.emacs`:

    (unless (require 'el-get nil t)
      (url-retrieve
        "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
        (lambda (s)
          (let (el-get-master-branch)
            (goto-char (point-max))
            (eval-print-last-sexp)))))
    
    (el-get 'sync)


el-get maintains package installations in a directory structure at  `~/.emacs.d/el-get`.  It loads definitions from `~/.emacs.d/el-get/.loaddefs.el` and tracks package status with `~/.emacs.d/el-get/.status.el`.  `(el-get 'sync)` installs or removes packages to bring the actual machine state in sync with the package `.status.el`.

el-get is self-hosted - here is its own status from `.status.el`:

    (el-get status "installed" recipe
      (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :info "." :compile 
             ("el-get.*\\.el$" "methods/")
             :features el-get :post-init
             (when
                 (memq 'el-get
                       (bound-and-true-p package-activated-list))
               (message "Deleting melpa bootstrap el-get")
               (unless package--initialized
                 (package-initialize t))
               (when
                   (package-installed-p 'el-get)
                 (let
                     ((feats
                       (delete-dups
                        (el-get-package-features
                         (el-get-elpa-package-directory 'el-get)))))
                   (el-get-elpa-delete-package 'el-get)
                   (dolist
                       (feat feats)
                     (unload-feature feat t))))
               (require 'el-get))))






