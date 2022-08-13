---
title: "Creating Binaries"
slug: "creating-binaries"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Building Buildapp
Standalone Common Lisp binaries can be built with [`buildapp`](http://www.xach.com/lisp/buildapp/). Before we can use it to generate binaries, we need to install and build it.

The easiest way I know how is using [`quicklisp`](https://www.quicklisp.org/beta/) and a Common Lisp (this example uses [`sbcl`], but it shouldn't make a difference which one you've got).

```
$ sbcl

This is SBCL 1.3.5.nixos, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.

* (ql:quickload :buildapp)
To load "buildapp":
  Load 1 ASDF system:
    buildapp
; Loading "buildapp"

(:BUILDAPP)

* (buildapp:build-buildapp)
;; loading system "buildapp"
[undoing binding stack and other enclosing state... done]
[saving current Lisp image into /home/inaimathi/buildapp:
writing 4800 bytes from the read-only space at 0x20000000
writing 3216 bytes from the static space at 0x20100000
writing 47349760 bytes from the dynamic space at 0x1000000000
done]
NIL

* (quit)

$ ls -lh buildapp 
-rwxr-xr-x 1 inaimathi inaimathi 46M Aug 13 20:12 buildapp
$
```

Once you have that binary built, you can use it to construct binaries of your Common Lisp programs. If you intend to do this a lot, you should also probably put it somewhere on your `PATH` so that you can just run it with `buildapp` from any directory.

## Buildapp Hello Web World
A more realistic example involves a project you're building with multiple files on disk (rather than an `--eval` option passed to `buildapp`), and some dependencies to pull in.

Because arbitrary things can happen during the finding and loading of `asdf` systems (including loading other, potentially unrelated systems), it's not enough to just inspect the `asd` files of the projects you're depending on in order to find out what you need to load. The general approach is to use `quicklisp` to load the target system, then call `ql:write-asdf-manifest-file` to write out a full manifest of everything that's loaded.

Here's a toy system built with [`hunchentoot`](http://weitz.de/hunchentoot/) to illustrate how that might happen in practice:

* * *

```
;;;; buildapp-hello-web-world.asd

(asdf:defsystem #:buildapp-hello-web-world
  :description "An example application to use when getting familiar with buildapp"
  :author "inaimathi <leo.zovic@gmail.com>"
  :license "Expat"
  :depends-on (#:hunchentoot)
  :serial t
  :components ((:file "package")
               (:file "buildapp-hello-web-world"))
```

```
;;;; package.lisp

(defpackage #:buildapp-hello-web-world
  (:use #:cl #:hunchentoot))
```

```
;;;; buildapp-hello-web-world.lisp

(in-package #:buildapp-hello-web-world)

(define-easy-handler (hello :uri "/") ()
  (setf (hunchentoot:content-type*) "text/plain")
  "Hello Web World!")

(defun main (argv)
  (declare (ignore argv))
  (start (make-instance 'easy-acceptor :port 4242))
  (format t "Press any key to exit...~%")
  (read-char))
```

```
;;;; build.lisp
(ql:quickload :buildapp-hello-web-world)
(ql:write-asdf-manifest-file "/tmp/build-hello-web-world.manifest")
(with-open-file (s "/tmp/build-hello-web-world.manifest" :direction :output :if-exists :append)
  (format s "~a~%" (merge-pathnames
            "buildapp-hello-web-world.asd"
            (asdf/system:system-source-directory
             :buildapp-hello-web-world))))
```

```
#### build.sh
sbcl --load "build.lisp" --quit

buildapp --manifest-file /tmp/build-hello-web-world.manifest --load-system hunchentoot --load-system buildapp-hello-web-world --output hello-web-world --entry buildapp-hello-web-world:main
```

* * *

Once you have those files saved in a directory named `buildapp-hello-web-world`, you can do 

```
$ cd buildapp-hello-web-world/

$ sh build.sh 
This is SBCL 1.3.7.nixos, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
To load "cffi":
  Load 1 ASDF system:
    cffi
; Loading "cffi"
........
To load "buildapp-hello-web-world":
  Load 1 ASDF system:
    buildapp-hello-web-world
; Loading "buildapp-hello-web-world"
....
;; loading system "cffi"
;; loading system "hunchentoot"
;; loading system "buildapp-hello-web-world"
[undoing binding stack and other enclosing state... done]
[saving current Lisp image into hello-web-world:
writing 4800 bytes from the read-only space at 0x20000000
writing 4624 bytes from the static space at 0x20100000
writing 66027520 bytes from the dynamic space at 0x1000000000
done]

$ ls -lh hello-web-world 
-rwxr-xr-x 1 inaimathi inaimathi 64M Aug 13 21:17 hello-web-world
```

This produces a binary that does exactly what you think it should, given the above.

```
$ ./hello-web-world 
Press any key to exit...

```

You should then be able to fire up another shell, do `curl localhost:4242` and see the plaintext response of `Hello Web World!` get printed out.

## Buildapp Hello World
The simplest possible binary you could build

1. Has no dependencies
2. Takes no command line arguments
3. Just writes "Hello world!" to `stdout`

After you've built `buildapp`, you can just...

```
$ buildapp --eval '(defun main (argv) (declare (ignore argv)) (write-line "Hello, world!"))' --entry main --output hello-world
[undoing binding stack and other enclosing state... done]
[saving current Lisp image into hello-world:
writing 4800 bytes from the read-only space at 0x20000000
writing 3216 bytes from the static space at 0x20100000
writing 43220992 bytes from the dynamic space at 0x1000000000
done]

$ ./hello-world 
Hello, world!

$
```

