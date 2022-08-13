---
title: "Conditional logic"
slug: "conditional-logic"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
 - if(expression){}
 - if(expression){}else{}
 - if(expression){}elseif(expression){}
 - if(expression){}elseif(expression){}else{}

See also [Comparison Operators](https://www.wikiod.com/powershell/operators#Comparison Operators), which can be used in conditional expressions.

## if, else and else if
Powershell supports standard conditional logic operators, much like many programming languages. These allow certain functions or commands to be run under particular circumstances. 

With an `if` the commands inside the brackets (`{}`) are only executed if the conditions inside the if(`()`) are met

    $test = "test"
    if ($test -eq "test"){
        Write-Host "if condition met"
    }

You can also do an `else`. Here the `else` commands are executed if the `if` conditions are **not** met:

    $test = "test"
    if ($test -eq "test2"){
        Write-Host "if condition met"
    }
    else{
        Write-Host "if condition not met"
    }

or an `elseif`. An else if runs the commands if the `if` conditions are not met and the `elseif` conditions are met:

    $test = "test"
    if ($test -eq "test2"){
        Write-Host "if condition met"
    }
    elseif ($test -eq "test"){
        Write-Host "ifelse condition met"
    }

Note the above use `-eq`(equality) CmdLet and not `=` or `==` as many other languages do for equlaity.

## Negation
You may want to negate a boolean value, i.e. enter an `if` statement when a condition is false rather than true. This can be done by using the `-Not` CmdLet

    $test = "test"
    if (-Not $test -eq "test2"){
        Write-Host "if condition not met"
    }

You can also use `!`:

    $test = "test"
    if (!($test -eq "test2")){
        Write-Host "if condition not met"
    }

there is also the `-ne` (not equal) operator:


    $test = "test"
    if ($test -ne "test2"){
        Write-Host "variable test is not equal to 'test2'"
    }


## If conditional shorthand
If you want to use the shorthand you can make use of conditional logic with the following shorthand. Only the string 'false' will evaluate to true (2.0).

    #Done in Powershell 2.0 
    $boolean = $false;
    $string = "false";
    $emptyString = "";
    
    If($boolean){
        # this does not run because $boolean is false
        Write-Host "Shorthand If conditions can be nice, just make sure they are always boolean."
    }

    If($string){
        # This does run because the string is non-zero length
        Write-Host "If the variable is not strictly null or Boolean false, it will evaluate to true as it is an object or string with length greater than 0."
    }

    If($emptyString){
        # This does not run because the string is zero-length
        Write-Host "Checking empty strings can be useful as well."
    }

    If($null){
        # This does not run because the condition is null
        Write-Host "Checking Nulls will not print this statement."
    }

