---
title: "Directory Stack"
slug: "directory-stack"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
 - PUSHD [path]
 - POPD

## Parameters
| Parameter | Details                      |
| --------- | ---------------------------- |
| path      | The directory to navigate to |

  * Using `pushd` without parameters will print the stack.
  * The `popd` command will overwrite the current Current Directory value.

## Delete Text Files
>The following example shows how you can use the `pushd` command and the `popd` command in a batch program to change the current directory from the one in which the batch program was run and then change it back:
> 
>     @echo off
>     rem This batch file deletes all .txt files in a specified directory
>     pushd %1
>     del *.txt
>     popd
>     cls
>     echo All text files deleted in the %1 directory

Sourced from https://technet.microsoft.com/en-us/library/cc771180%28v=ws.11%29.aspx

## Print Directory Stack
To print the directory stack, use the command `pushd` without any parameters:

    @echo off
    
    cd C:\example\
    pushd one
    pushd ..\two
    pushd ..\..
    
    
    pushd
    echo Current Directory: %cd%
    
    echo:
    popd
    pushd three
    
    pushd
    echo Current Directory: %cd%

Output:

    C:\example\two                            
    C:\example\one
    C:\example                                
    Current Directory: C:\                        
    
    C:\example\two                            
    C:\example\one
    C:\example                                
    Current Directory: C:\example\two\three   

