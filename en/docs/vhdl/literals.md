---
title: "Literals"
slug: "literals"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

This has how to specify constants, called literals in VHDL

## Numeric literals
       16#A8# -- hex
       2#100# -- binary
       2#1000_1001_1111_0000 -- long number, adding (optional) _ (one or more) for readability
       1234 -- decimal

## Enumerated literal
    type state_t is (START, READING, WRITING); -- user-defined enumerated type

