---
title: "Getting started with autoit"
slug: "getting-started-with-autoit"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
AutoIt is intended for use on the Microsoft Windows operating system.  It is compatible with versions from Windows XP onwards. 

Download the installation utility from https://www.autoitscript.com/site/autoit/downloads/

If installing on a 64-bit version of Windows, the user is prompted to choose a 64- or 32-bit installation.  The choice affects the compilation utilities that are installed alongside the script interpreter.

The installation utility will install into the given directory:  

 - The script interpreter
 - A script compiler
 - A script editor (SciTE)
 - Help files
 - Examples

## Hello World
A simple AutoIt script showing some of the basic features of the language.  
Note that semicolons introduce comments  
Save this as "HelloWorld.au3"



    ; Include constants.  In this case, for the MsgBox() function
    #include <MsgBoxConstants.au3>
    
    ; Define and initialize the title of a dialogue box
    ; All AutoIt variables are loosely typed
    ; Local specifies the scope
    Local $title
    $title = "Example Window"
    
    ; Alternative method of implicit definition
    ; This time also indicating constant
    Const $text = "Hello World"
    
    ; Third example definition
    ; Strings and integers defined in the same manner
    $timeout = 30
    
    ; Create dialogue box using #included constants
    MsgBox($MB_ICONINFORMATION, $title, $text, $timeout)
    
    ; Program end - Use of Exit is not mandatory
    Exit

This will produce a simple output:-  
[![Hello World output][1]][1]


  [1]: http://i.stack.imgur.com/LU4jJ.png

