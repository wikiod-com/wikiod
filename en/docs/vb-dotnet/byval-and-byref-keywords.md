---
title: "ByVal and ByRef keywords"
slug: "byval-and-byref-keywords"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## ByRef keyword
ByRef keyword before method parameter says that parameter will be sent in a way allowing the method to change (assign a new value) the variable underlying the parameter.

    Class SomeClass
        Public Property Member As Integer
    End Class

    Module Program
        Sub Main()
            Dim someInstance As New SomeClass With {.Member = 42}
        
            Foo (someInstance)
            ' here someInstance is not Nothing
            ' but someInstance.Member is -42

            Bar(someInstance)
            ' here someInstance is Nothing
        End Sub

        Sub Foo(ByVal arg As SomeClass)
            arg.Member = -arg.Member ' change argument content
            arg = Nothing ' change (re-assign) argument
        End Sub

        Sub Bar(ByRef param As Integer)
            arg.Member = -arg.Member ' change argument content
            arg = Nothing ' change (re-assign) argument
        End Sub
    End Module


## ByVal keyword
ByVal keyword before method parameter (or no keyword as ByVal is assumed by default) says that parameter will be sent in a way **not** allowing the method to change (assign a new value) the variable underlying the parameter.  
It doesn't prevent the content (or state) of the argument to be changed if it's a class.

    Class SomeClass
        Public Property Member As Integer
    End Class

    Module Program
        Sub Main()
            Dim someInstance As New SomeClass With {.Member = 42}
    
            Foo (someInstance)
            ' here someInstance is not Nothing (still the same object)
            ' but someInstance.Member is -42 (internal state can still be changed)

            Dim number As Integer = 42
            Foo(number)
            ' here number is still 42
        End Sub

        Sub Foo(ByVal arg As SomeClass)
            arg.Member = -arg.Member ' change argument content
            arg = Nothing ' change (re-assign) argument
        End Sub    

        Sub Foo(arg As Integer) ' No ByVal or ByRef keyword, ByVal is assumed
            arg = -arg
        End Sub
    End Module

