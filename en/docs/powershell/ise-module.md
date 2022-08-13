---
title: "ISE module"
slug: "ise-module"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Windows PowerShell Integrated Scripting Environment (ISE) is a host application that enables you to write, run, and test scripts and modules in a graphical and intuitive environment. Key features in Windows PowerShell ISE include syntax-coloring, tab completion, Intellisense, visual debugging, Unicode compliance, and context-sensitive Help, and provide a rich scripting experience.

## Test Scripts
The simple, yet powerful use of the ISE is e.g. writing code in the top section (with intuitive syntax coloring) and run the code by simply marking it and hitting the F8 key.

    function Get-Sum
    {
        foreach ($i in $Input)
        {$Sum += $i}
        $Sum

    1..10 | Get-Sum

    #output
    55

