---
title: "package Ada.Text_IO"
slug: "package-adatext_io"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Package ``Ada.Text_IO`` is used for putting text or getting text from files or console.

## Put_Line
Prints out string with a newline.

    with Ada.Text_IO;
    
    procedure Put_Text is
        use Ada.Text_IO;
        S : String := "Hello";
    begin
        Put_Line ("Hello");
        Put_Line (Standard_Output, "Hello");
        Put_Line (Standard_Error, "Hello error");
        Put_Line (S & " World");
    end;

# Result

    Hello
    Hello
    Hello error
    Hello World

