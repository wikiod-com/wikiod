---
title: "sequence - how to split a sequence"
slug: "sequence---how-to-split-a-sequence"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Syntax
1. split regex target-string &key start end limit with-registers-p omit-unmatched-p sharedp => list
2. lispworks:split-sequence separator-bag sequence &key start end test key coalesce-separators => sequences
3. split-sequence delimiter sequence &key start end from-end count remove-empty-subseqs test  test-not key => list of subsequences

## Split strings using regular expressions
The library CL-PPCRE provides the function `split` which allows us to split strings in substrings that match a regular expression, discarding the parts of the string that do not.

    (cl-ppcre:split "\\." "127.0.0.1")
    ;; => ("127" "0" "0" "1")

## Using the split-sequence library
The split-sequence library provides a function `split-sequence`, which allows to split on elements of a sequence

    (split-sequence:split-sequence #\Space "John Doe II")
    ;; => ("John" "Doe" "II")

## SPLIT-SEQUENCE in LIspWorks
Simple split of an IP number string.
 
    > (lispworks:split-sequence "." "127.0.0.1")
      ("127" "0" "0" "1")

Simple split of an URL:

    > (lispworks:split-sequence ".:/" "http://127.0.0.1/foo/bar.html"
                                :coalesce-separators t)
    ("http" "127" "0" "0" "1" "foo" "bar" "html")


