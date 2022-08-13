---
title: "File Operations"
slug: "file-operations"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Overview
Read a file all at once (not recommended for large files):

    (slurp "./small_file.txt")

Write data to a file all at once:

    (spit "./file.txt" "Ocelots are Awesome!")    ; overwrite existing content
    (spit "./log.txt" "2016-07-26 New entry." :append true)

Read a file line-by-line:

    (use 'clojure.java.io)
    (with-open [rdr (reader "./file.txt")]
        (line-seq rdr)    ; returns lazy-seq
    )    ; with-open macro calls (.close rdr)

Write a file line-by-line:

    (use 'clojure.java.io)
    (with-open [wrtr (writer "./log.txt" :append true)]
        (.write wrtr "2016-07-26 New entry.")
    )    ; with-open macro calls (.close wrtr)

Write to a file, replacing existing content:

    (use 'clojure.java.io)
    (with-open [wrtr (writer "./file.txt")]
        (.write wrtr "Everything in file.txt has been replaced with this text.")
    )    ; with-open macro calls (.close wrtr)

## Notes:
 - You can specify URLs as well as files
 - Options to `(slurp)` and `(spit)` are passed to `clojure.java.io/reader` and `/writer`, respectively.

