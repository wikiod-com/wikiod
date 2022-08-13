---
title: "Unit testing"
slug: "unit-testing"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Introduction
There are a few libraries for unit testing in Common Lisp

- [FiveAM](https://github.com/sionescu/fiveam)
- [Prove](https://github.com/fukamachi/prove), with a few unique features like extensive test reporters, colored output, report of test duration and asdf integration.
- [Lisp-Unit2](https://github.com/AccelerationNet/lisp-unit2), similar to JUnit
- [Fiasco](https://github.com/joaotavora/fiasco), focusing on providing a good testing experience from the REPL. Successor to [hu.dwim.stefil](http://dwim.hu/darcsweb/darcsweb.cgi?r=HEAD%20hu.dwim.stefil;a=summary)

## Using FiveAM
# Loading the library

    (ql:quickload "fiveam")

# Define a test case

    (fiveam:test sum-1
      (fiveam:is (= 3 (+ 1 2))))

    ;; We'll also add a failing test case
    (fiveam:test sum2
      (fiveam:is (= 4 (+ 1 2))))

# Run tests   


    (fiveam:run!)

which reports
    
    Running test suite NIL
     Running test SUM2 f
     Running test SUM1 .
     Did 2 checks.
        Pass: 1 (50%)
        Skip: 0 ( 0%)
        Fail: 1 (50%)
     Failure Details:
     --------------------------------
     SUM2 []: 
          
    (+ 1 2)
    
     evaluated to 
    
    3
    
     which is not 
    
    =
    
     to 
    
    4
    
    ..
     --------------------------------
    NIL

# Notes

- Tests are grouped by test-suites
- By defaults tests are added to the global test-suite

