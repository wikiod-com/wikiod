---
title: "Streams"
slug: "streams"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Syntax
- `(read-char &optional stream eof-error-p eof-value recursive-p)` => character
- `(write-char character &optional stream)` => character
- `(read-line &optional stream eof-error-p eof-value recursive-p)` => line, missing-newline-p
- `(write-line line &optional stream)` => line

## Parameters
| Parameter     | Detail |
| ------------- | ------ |
| `stream`      | The stream to read from or write to.
| `eof-error-p` | Should an error be signalled if end of file is encountered.
| `eof-value`   | What value should be returned if eof is encountered, and `eof-error-p` is false.
| `recursive-p` | Is the read-operation called recursively from `READ`. Usually this should be left as `NIL`.
| `character`   | The character to write, or the character that was read.
| `line`        | The line to write, or the line that was read.

## Copying a file
**Copy byte-per-byte of a file**

The following function copies a file into another by performing an exact byte-per-byte copy, ignoring the kind of content (which can be either lines of characters in some encoding or binary data):

    (defun byte-copy (infile outfile)
      (with-open-file (instream infile :direction :input :element-type '(unsigned-byte 8)
                                :if-does-not-exist nil)
        (when instream
          (with-open-file (outstream outfile :direction :output :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
            (loop for byte = (read-byte instream nil)
               while byte
               do (write-byte byte outstream))))))

The type `(unsigned-byte 8)` is the type of 8-bit bytes. The functions `read-byte` and `write-byte` work on bytes, instead of `read-char` and `write-char` that work on characters. `read-byte` returns a byte read from the stream, or `NIL` at the end of the file if the second  optional parameter is `NIL` (otherwise it signals an error). 

**Bulk copy**

An exact copy, more efficient the the previous one. can be done by reading and writing the files with large chunks of data each time, instead of single bytes:

    (defun bulk-copy (infile outfile)
      (with-open-file (instream infile :direction :input :element-type '(unsigned-byte 8)
                                :if-does-not-exist nil)
        (when instream
          (with-open-file (outstream outfile :direction :output :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
            (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
              (loop for bytes-read = (read-sequence buffer instream)
                 while (plusp bytes-read)
                 do (write-sequence buffer outstream :end bytes-read)))))))

`read-sequence` and `write-sequence` are used here with a buffer which is a vector of bytes (they can operate on sequences of bytes or characters). `read-sequence` fills the array with the bytes read each time, and returns the numbers of bytes read (that can be less than the size of the array when the end of file is reached). Note that the array is destructively modified at each iteration.

**Exact copy line-per-line of a file**

The final example is a copy performed by reading each line of characters of the input file, and writing it to the output file. Note that, since we want an exact copy, we must check if the last line of the input file is terminated or not by an end of line character(s).
For this reason, we use the two values returned by `read-line`: a new string containing the characters of the next line, and a boolean value that is *true* if the line is the last of the file and does not contain the final newline character(s). In this case `write-string` is used instead of `write-line`, since the former does not add a newline at the end of the line.

    (defun line-copy (infile outfile)
      (with-open-file (instream infile :direction :input :if-does-not-exist nil)
        (when instream
          (with-open-file (outstream outfile :direction :output :if-exists :supersede)
            (let (line missing-newline-p)
              (loop
                 (multiple-value-setq (line missing-newline-p)
                   (read-line instream nil nil))
                 (cond (missing-newline-p                          ; we are at the end of file
                        (when line (write-string line outstream))  ; note `write-string`
                        (return))                                  ; exit from simple loop
                       (t (write-line line outstream)))))))))

Note that this program is platform independent, since the newline character(s) (varying in different operating systems) is automatically managed by the `read-line` and `write-line` functions.

## Creating input streams from strings
The macro [`WITH-INPUT-FROM-STRING`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_in_f.htm) can be used to make a stream from a string.

    (with-input-from-string (str "Foobar")
      (loop for i from 0
            for char = (read-char str nil nil)
            while char
            do (format t "~d: ~a~%" i char)))
    ; 0: F
    ; 1: o
    ; 2: o
    ; 3: b
    ; 4: a
    ; 5: r
    ;=> NIL

The same can be done manually using [`MAKE-STRING-INPUT-STREAM`](http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_s_1.htm).

    (let ((str (make-string-input-stream "Foobar")))
      (loop for i from 0
            for char = (read-char str nil nil)
            while char
            do (format t "~d: ~a~%" i char)))

## Writing output to a string
The macro [`WITH-OUTPUT-TO-STRING`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_out_.htm#with-output-to-string) can be used to create a string output stream, and return the resulting string at the end.

    (with-output-to-string (str)
      (write-line "Foobar!" str)
      (write-string "Barfoo!" str))
    ;=> "Foobar!
    ;   Barfoo!"

The same can be done manually using [`MAKE-STRING-OUTPUT-STREAM`](http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_s_2.htm) and [`GET-OUTPUT-STREAM-STRING`](http://www.lispworks.com/documentation/HyperSpec/Body/f_get_ou.htm#get-output-stream-string).

    (let ((str (make-string-output-stream)))
      (write-line "Foobar!" str)
      (write-string "Barfoo!" str)
      (get-output-stream-string str))

## Gray streams
Gray streams are a non-standard extension that allows user defined streams. It provides classes and methods that the user can extend. You should check your implementations manual to see if it provides Gray streams.

For a simple example, a character input stream that returns random characters could be implemented like this:

    (defclass random-character-input-stream (fundamental-character-input-stream)
      ((character-table
        :initarg :character-table
        :initform "abcdefghijklmnopqrstuvwxyz
    " ; The newline is necessary.
        :accessor character-table))
      (:documentation "A stream of random characters."))
    
    (defmethod stream-read-char ((stream random-character-input-stream))
      (let ((table (character-table stream)))
        (aref table (random (length table)))))
    
    (let ((stream (make-instance 'random-character-input-stream)))
      (dotimes (i 5)
        (print (read-line stream))))
    ; "gyaexyfjsqdcpciaaftoytsygdeycrrzwivwcfb" 
    ; "gctnoxpajovjqjbkiqykdflbhfspmexjaaggonhydhayvknwpdydyiabithpt" 
    ; "nvfxwzczfalosaqw" 
    ; "sxeiejcovrtesbpmoppfvvjfvx" 
    ; "hjplqgstbodbalnmxhsvxdox" 
    ;=> NIL

## Reading file
A file can be opened for reading as a stream using [`WITH-OPEN-FILE`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_open.htm) macro.

    (with-open-file (file #P"test.file")
      (loop for i from 0
            for line = (read-line file nil nil)
            while line
            do (format t "~d: ~a~%" i line)))
    ; 0: Foobar
    ; 1: Barfoo
    ; 2: Quuxbar
    ; 3: Barquux
    ; 4: Quuxfoo
    ; 5: Fooquux
    ;=> T
    
The same can be done manually using [`OPEN`](http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm) and [`CLOSE`](http://www.lispworks.com/documentation/HyperSpec/Body/f_close.htm).

    (let ((file (open #P"test.file"))
          (aborted t))
      (unwind-protect
           (progn
             (loop for i from 0
                   for line = (read-line file nil nil)
                   while line
                   do (format t "~d: ~a~%" i line))
             (setf aborted nil))
        (close file :abort aborted)))

Note that `READ-LINE` creates a new string for each line. This can be slow. Some implementations provide a variant, which can read a line into a string buffer. Example: [`READ-LINE-INTO`][1] for Allegro CL.


  [1]: http://franz.com/support/documentation/current/doc/operators/excl/read-line-into.htm

## Writing to a file
A file can be opened for writing as a stream using [`WITH-OPEN-FILE`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_open.htm) macro.

    (with-open-file (file #P"test.file" :direction :output
                                        :if-exists :append
                                        :if-does-not-exist :create)
      (dolist (line '("Foobar" "Barfoo" "Quuxbar"
                      "Barquux" "Quuxfoo" "Fooquux"))
        (write-line line file)))

The same can be done manually with [`OPEN`](http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm) and [`CLOSE`](http://www.lispworks.com/documentation/HyperSpec/Body/f_close.htm).

    (let ((file (open #P"test.file" :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)))
      (dolist (line '("Foobar" "Barfoo" "Quuxbar"
                      "Barquux" "Quuxfoo" "Fooquux"))
        (write-line line file))
      (close file))

## Reading and writing entire files to and from strings
The following function reads an entire file into a new string and returns it:

    (defun read-file (infile)
      (with-open-file (instream infile :direction :input :if-does-not-exist nil)
        (when instream 
          (let ((string (make-string (file-length instream))))
            (read-sequence string instream)
            string))))

The result is `NIL` if the file does not exists.

The following function writes a string to a file. A keyword parameter is used to specify what to do if the file already exists (by default it causes an error, the values admissible are those of the `with-open-file` macro).

    (defun write-file (string outfile &key (action-if-exists :error))
       (check-type action-if-exists (member nil :error :new-version :rename :rename-and-delete 
                                            :overwrite :append :supersede))
       (with-open-file (outstream outfile :direction :output :if-exists action-if-exists)
         (write-sequence string outstream)))

In this case `write-sequence` can be substituted with `write-string`.





