---
title: "Return behavior in PowerShell"
slug: "return-behavior-in-powershell"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

It can be used to Exit the current scope, which can be a function, script, or script block. In PowerShell, the result of each statement is returned as output, even without an explicit Return keyword or to indicate that the end of the scope has been reached.

You can read more about the return semantics on the [about_Return][1] page on TechNet, or by invoking `get-help return` from a PowerShell prompt.

<hr/>

Notable Q&A question(s) with more examples/explanation:
 * http://stackoverflow.com/questions/10286164/function-return-value-in-powershell
 * http://stackoverflow.com/questions/22663848/powershell-function-doesnt-have-proper-return-value

<hr/>

[about_return][1] on MSDN explains it succinctly:

> The Return keyword exits a function, script, or script block. It can
> be  used to exit a scope at a specific point, to return a value, or to
> indicate  that the end of the scope has been reached. 
> 
> 
> Users who are familiar with languages like C or C# might want to use
> the  Return keyword to make the logic of leaving a scope explicit.
> 
> 
> In Windows PowerShell, the results of each statement are returned as 
> output, even without a statement that contains the Return keyword. 
> Languages like C or C# return only the value or values that are
> specified  by the Return keyword.



[1]: https://technet.microsoft.com/en-us/library/hh847760.aspx

## Early exit
    function earlyexit {
        "Hello"
        return
        "World"
    }

"Hello" will be placed in the output pipeline, "World" will not

## Gotcha! Return in the pipeline
    get-childitem | foreach-object { if ($_.IsReadOnly) { return } } 

Pipeline cmdlets (ex: `ForEach-Object`, `Where-Object`, etc) operate on closures. The return here will only move to the next item on the pipeline, not exit processing. 
You can use **break** instead of **return** if you want to exit processing.

    get-childitem | foreach-object { if ($_.IsReadOnly) { break } } 


## Return with a value
(paraphrased from [about_return][1])

The following methods will have the same values on the pipeline
    
    function foo {
        $a = "Hello"
        return $a
    }
    
    function bar {
        $a = "Hello"
        $a
        return
    } 

    function quux {
        $a = "Hello"
        $a
    } 

[1]: https://technet.microsoft.com/en-us/library/hh847760.aspx

## Gotcha! Ignoring unwanted output
Inspired by 
 * http://stackoverflow.com/questions/22663848/powershell-function-doesnt-have-proper-return-value  


    function bar {
     [System.Collections.ArrayList]$MyVariable = @()
     $MyVariable.Add("a") | Out-Null
     $MyVariable.Add("b") | Out-Null
     $MyVariable
    }

The `Out-Null` is necessary because the .NET `ArrayList.Add` method returns the number of items in the collection after adding. If omitted, the pipeline would have contained `1, 2, "a", "b"`

There are multiple ways to omit unwanted output:

    function bar
    {
        # New-Item cmdlet returns information about newly created file/folder
        New-Item "test1.txt" | out-null
        New-Item "test2.txt" > $null
        [void](New-Item "test3.txt")
        $tmp = New-Item "test4.txt"
    }

**Note:** to learn more about why to prefer `> $null`, see [topic not yet created].

## How to work with functions returns
A function returns everything that is not captured by something else.  
If u use the **return** keyword, every statement after the return line will not be executed!

Like this:

    Function Test-Function
    {
        Param
        (
            [switch]$ExceptionalReturn
        )
        "Start"
        if($ExceptionalReturn){Return "Damn, it didn't work!"}
        New-ItemProperty -Path "HKCU:\" -Name "test" -Value "TestValue" -Type "String"
        Return "Yes, it worked!"
    }
    
   

 Test-Function  
 Will return:  
 - Start
 - The newly created registry key (this is because there are some statements that create output that you may not be expecting)
 - Yes, it worked!

 Test-Function  -ExceptionalReturn
 Will return:  
 - Start
 - Damn, it didn't work!

If you do it like this:

    Function Test-Function
    {
        Param
        (
            [switch]$ExceptionalReturn
        )
        . {
           "Start"
            if($ExceptionalReturn)
            {
                $Return = "Damn, it didn't work!"
                Return
            }
            New-ItemProperty -Path "HKCU:\" -Name "test" -Value "TestValue" -Type "String"
            $Return = "Yes, it worked!"
            Return 
        } | Out-Null
        Return $Return
    }
    
   

 Test-Function  
 Will return:  
 - Yes, it worked!

 Test-Function  -ExceptionalReturn
 Will return:  
 - Damn, it didn't work!

With this trick you can control the returned output even if you are not sure what will each statement will spit out.

It works like this

    .{<Statements>} | Out-Null
the . makes the following scriptblock included in the code  
the {} marks the script block  
the | Out-Null pipes any unexpected output to Out-Null (so it is gone!)  
Because the scriptblock is included it gets the same scope as the rest of the function.  
So you can access variables who were made inside the scriptblock.



