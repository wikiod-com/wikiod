---
title: "Special Operators"
slug: "special-operators"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Array Expression Operator
Returns the expression as an array.

    @(Get-ChildItem $env:windir\System32\ntdll.dll)

Will return an array with one item

    @(Get-ChildItem $env:windir\System32)

Will return an array with all the items in the folder (which is not a change of behavior from the inner expression.

## Call Operation
    $command = 'Get-ChildItem'
    & $Command

Will execute `Get-ChildItem`

## Dot sourcing operator
. .\myScript.ps1

runs `.\myScript.ps1` in the current scope making any functions, and variable available in the current scope.

