---
title: "Identifiers"
slug: "identifiers"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Basic identifiers
Basic identifiers consist of letters, underscores and digits and must start with a letter. They are not case sensitive. Reserved words of the language cannot be basic identifiers. Examples of valid VHDL basic identifiers:

    A_myId90
    a_MYID90
    abcDEf100_1
    ABCdef100_1

The two first are equivalent and the two last are also equivalent (case insensitivity).

Examples of invalid basic identifiers:

    _not_reset   -- start with underscore
    85MHz_clock  -- start with digit
    LooP         -- reserved word of the language

## Extended identifiers
VHDL extended identifiers are delimited by backslashes (`\`) and can contain letters, underscores, digits, spaces and other special characters (see the Language Reference Manual for a complete definition of special characters). The sequence of characters between backslashes can be reserved words of the VHDL language. Backslashes can be included in extended identifiers by doubling them (`\\`). Extended identifiers are case sensitive. Examples of (all different) extended identifiers:

    \if\
    \If\
    \My Identifier\
    \An \\ Identifier \\ With \\ Backslashes\
    \&#@[]:.*\
    \$£§{}\

