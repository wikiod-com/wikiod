---
title: "Comments"
slug: "comments"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Syntax
- (* opens a block comment
- *) closes a block comment
- (* and *) must be balanced in number

## All comments are block comments
    (*************************************************
    *  All comments in SML are block comments
    *  Block Comments begin with '(*'
    *  Block Comments end with '*)'
    *  (* Block Comments can be nested *)
    *  The additional framing asterisks at the beginning
    *  and end of this block comment are common to languages
    *  of SML's vintage.
    *  Likewise the asterisk at the start of each line
    *  But this is solely a matter of style.
    **************************************************)
    
    val _ = print "Block comment example\n" (* line ending block comment *)




## Nested Comments
    (* The block comment syntax allows nested comments
    (* whether or not this is a good thing is probably
    a matter of personal opinion (*or coding standards*)*)*)
    
    val _ = print "Nested comment example\n" (* line ending block comment *)

 

