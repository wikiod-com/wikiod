---
title: "Working with SLIME"
slug: "working-with-slime"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Using REPL
```
CL-USER> (+ 2 3)
5
CL-USER> (sin 1.5)
0.997495
CL-USER> (mapcar (lambda (x) (+ x 2)) '(1 2 3))
(3 4 5)
```

The result that is printed after evaluation is not only a string: there is full-on Lisp object behind it which can be inspected by right-clicking on it and choosing Inspect.

Multi-line input is also possible: use `C-j` to put new line. `Enter`-key will cause the entered form to be evaluated and if the form is not finished, will likely cause an error:

```
CL-USER> (mapcar (lambda (x y)
                   (declare (ignore y))
                   (* x 2))
                 '(1 2 3)
                 '(:a :b :c))
(2 4 6)
```

# Error handling
If evaluation causes an error:

```
CL-USER> (/ 3 0)
```

This will pop up a debugger buffer with the following content (in SBCL lisp):

```
arithmetic error DIVISION-BY-ZERO signalled
Operation was /, operands (3 0).
   [Condition of type DIVISION-BY-ZERO]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1004FA8033}>)

Backtrace:
  0: (SB-KERNEL::INTEGER-/-INTEGER 3 0)
  1: (/ 3 0)
  2: (SB-INT:SIMPLE-EVAL-IN-LEXENV (/ 3 0) #<NULL-LEXENV>)
  3: (EVAL (/ 3 0))
  4: (SWANK::EVAL-REGION "(/ 3 0) ..)
  5: ((LAMBDA NIL :IN SWANK-REPL::REPL-EVAL))
--- more ---
```

Moving cursor down passed `--- more ---` will cause the backtrace to expand further.

At each line of the backtrace pressing `Enter` will show more information about a particular call (if available).

Pressing `Enter` on the line of restarts will cause a particular restart to be invoked. Alternatively, the restart can be chosen by number `0`, `1` or `2` (press corresponding key anywhere in the buffer). The default restart is marked by a star and can be invoked by pressing key `q` (for "quit"). Pressing `q` will close the debugger and show the following in REPL

```
; Evaluation aborted on #<DIVISION-BY-ZERO {10064CCE43}>.
CL-USER> 
```

Finally, quite rarely, but Lisp might encounter an error that cannot be handled by Lisp debugger, in which case it will drop into low-level debugger or finish abnormally. To see the cause of this kind of error, switch to `*inferior-lisp*` buffer.

## Starting and finishing SLIME, special (comma) REPL commands
In Emacs `M-x slime` will start slime with the default (first) Common Lisp implementation. If there are multiple implementations provided (via variable `slime-lisp-implementations`), other implementations can be accessed via `M-- M-x slime`, which will offer the choice of available implementations in mini-buffer.

`M-x slime` will open REPL buffer which will look as follows:

```
; SLIME 2016-04-19
CL-USER> 
```

SLIME REPL buffer accepts a few special commands. All of them start with `,`. Once `,` is typed, the list of options will be shown in mini-buffer. They include:

- `,quit`
- `,restart-inferior-lisp`
- `,pwd` - prints current directory from where Lisp is running
- `,cd` - will change current directory



## Installation
It is best to use latest SLIME from Emacs MELPA repository: the packages may be a bit unstable, but you get the latest features.

### Portale and multiplatform Emacs, Slime, Quicklisp, SBCL and Git

You can download a portable and multiplatform version of Emacs25 already configured with Slime, SBCL, Quicklisp and Git: [Portacle](https://shinmera.github.io/portacle/). It's a quick and easy way to get going. If you want to learn how to install everything yourself, read on.

### Manual install

In GNU Emacs (>= 24.5) initialization file (`~/.emacs` or `~/.emacs.d/init.el`) add the following:

```
;; Use Emacs package system
(require 'package)
;; Add MELPA repository
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Reload package list
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; List of packages to install:
(setq package-list
      '(magit                    ; git interface (OPTIONAL)
        auto-complete            ; auto complete (RECOMMENDED)
        auto-complete-pcmp       ; programmable completion
        idle-highlight-mode      ; highlight words in programming buffer (OPTIONAL)
        rainbow-delimiters       ; highlight parenthesis (OPTIONAL)
        ac-slime                 ; auto-complete for SLIME
        slime                    ; SLIME itself
        eval-sexp-fu             ; Highlight evaluated form (OPTIONAL)
        smartparens              ; Help with many parentheses (OPTIONAL)
        ))

;; Install if are not installed
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Parenthesis - OPTIONAL but recommended
(show-paren-mode t)
(require 'smartparens-config)
(sp-use-paredit-bindings)
(sp-pair "(" ")" :wrap "M-(")
(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-S-<right>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-S-<left>") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-(") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-}") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-{") 'sp-backward-barf-sexp)

(sp-pair "(" ")" :wrap "M-(")
(sp-pair "[" "]" :wrap "M-[")
(sp-pair "{" "}" :wrap "M-{")

;; MAIN Slime setup
;; Choose lisp implementation:
;; The first option uses roswell with default sbcl
;; the second option - uses ccl directly
(setq slime-lisp-implementations
      '((roswell ("ros" "-L" "sbcl-bin" "run"))
        (ccl ("ccl64"
              "-K" "utf-8"))))
;; Other settings...
```

SLIME on its own is OK, but it works better with [Quicklisp][1] package manager. To install Quicklisp, follow the instruction on the website (if you use [roswell][2], follow roswell instructions). Once installed, in your lisp invoke:

```
(ql:quickload :quicklisp-slime-helper)
```

and add the following lines to Emacs init file:

```
;; Find where quicklisp is installed to
;; Add your own location if quicklisp is installed somewhere else
(defvar quicklisp-directories
  '("~/.roswell/lisp/quicklisp/"           ;; default roswell location for quicklisp
    "~/quicklisp/")                        ;; default quicklisp location
  "Possible locations of QUICKLISP")

;; Load slime-helper
(let ((continue-p t)
      (dirs quicklisp-directories))
  (while continue-p
    (cond ((null dirs) (message "Cannot find slime-helper.el"))
          ((file-directory-p (expand-file-name (car dirs)))
           (message "Loading slime-helper.el from %s" (car dirs))
           (load (expand-file-name "slime-helper.el" (car dirs)))
           (setq continue-p nil))
          (t (setq dirs (cdr dirs))))))

;; Autocomplete in SLIME
(require 'slime-autoloads)
(slime-setup '(slime-fancy))

;; (require 'ac-slime)
 (add-hook 'slime-mode-hook 'set-up-slime-ac)
 (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
 (eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'slime-repl-mode))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; Hooks
(add-hook 'lisp-mode-hook (lambda ()
                            (rainbow-delimiters-mode t)
                            (smartparens-strict-mode t)
                            (idle-highlight-mode t)
                            (auto-complete-mode)))

(add-hook 'slime-mode-hook (lambda ()
                             (set-up-slime-ac)
                             (auto-complete-mode)))

(add-hook 'slime-repl-mode-hook (lambda ()
                                  (rainbow-delimiters-mode t)
                                  (smartparens-strict-mode t)
                                  (set-up-slime-ac)
                                  (auto-complete-mode)))
```

After the restart, GNU Emacs will install and set up all the necessary packages.


  [1]: https://www.quicklisp.org/beta/
  [2]: https://github.com/roswell/roswell

## Setting up a SWANK server over a SSH tunnel.
1) Install a Common Lisp implementation on the server. (E.g. `sbcl`, `clisp`, etc...)
2) Install [`quicklisp`](https://www.quicklisp.org/beta/#installation) on the server.
3) Load SWANK with `(ql:quickload :swank)`
4) Start the server with `(swank:create-server)`. The default port is `4005`.
5) [On your local machine] Create a SSH tunnel with `ssh -L4005:127.0.0.1:4005 [remote machine]`
6) Connect to the running remote swank server with `M-x slime-connect`. The host should be `127.0.0.1` and the port `4005`.

