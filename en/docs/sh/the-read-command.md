---
title: "The `read` command"
slug: "the-read-command"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Read a line verbatim
    $ IFS= read -r foo <<EOF
    >     this is a \n line
    >EOF
    $ printf '%s\n' "$foo"
        this is a \n line
    

## Read a line, stripping leading and trailing whitespace
    $ read -r foo <<EOF
    >    this is a line
    >EOF
    $ printf '%s\n' "$foo"
    this is a line

