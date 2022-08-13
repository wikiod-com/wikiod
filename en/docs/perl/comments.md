---
title: "Comments"
slug: "comments"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Single-line comments
Single-line comments begin with a pound sign `#` and go to the end of the line:

    # This is a comment

    my $foo = "bar"; # This is also a comment

## Multi-line comments
Multi-line comments start with `=` and with the `=cut` statement. These are special comments called POD (Plain Old Documentation).

Any text between the markers will be commented out:

    =begin comment
    
    This is another comment.
    And it spans multiple lines!
    
    =end comment
    
    =cut

