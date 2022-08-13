---
title: "Generics"
slug: "generics"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Create a generic class
A generic type is created to adapt so that the same functionallity can be accessible for different data types.

    Public Class SomeClass(Of T)
        Public Sub doSomething(newItem As T)
            Dim tempItem As T
            ' Insert code that processes an item of data type t.
        End Sub
    End Class

## Instance of a Generic Class
By creating an instance of the same class with a different type given, the interface of the class changes depending on the given type.

    Dim theStringClass As New SomeClass(Of String)
    Dim theIntegerClass As New SomeClass(Of Integer)


[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/9trTP.png

## Define a 'generic' class
A generic class is a class who adapts to a later-given type so that the same functionality can be offered to different types.

In this basic example a generic class is created. It has a sub who uses the generic type T.  While programming this class, we don't  know the type of T.  In this case T has all the characteristics of Object.

    Public Class SomeClass(Of T)
        Public Sub doSomething(newItem As T)
            Dim tempItem As T
            ' Insert code that processes an item of data type t.
        End Sub
    End Class


## Use a generic class
In this example there are 2 instances created of the SomeClass Class. Depending on the type given the 2 instances have a different interface:

    Dim theStringClass As New SomeClass(Of String)
    Dim theIntegerClass As New SomeClass(Of Integer)

[![enter image description here][1]][1]
[![enter image description here][2]][2]



The most famous generic class is List(of )

  [1]: http://i.stack.imgur.com/8qt7U.png
  [2]: http://i.stack.imgur.com/cJyvz.png

## Limit the possible types given
The possible types passed to a new instance of SomeClass must inherit SomeBaseClass. This can also be an interface.  The characteristics of SomeBaseClass are accessible within this class definition.

  
    Public Class SomeClass(Of T As SomeBaseClass)
        Public Sub DoSomething(newItem As T)
            newItem.DoSomethingElse()
            ' Insert code that processes an item of data type t.
        End Sub
    End Class

    Public Class SomeBaseClass
        Public Sub DoSomethingElse()
        End Sub
    End Class


## Create a new instance of the given type
Creating a new intance of a generic type can be done/checed at compile time.

    Public Class SomeClass(Of T As {New})
        Public Function GetInstance() As T
            Return New T
        End Function
    End Class

Or with limited types:

    Public Class SomeClass(Of T As {New, SomeBaseClass})
        Public Function GetInstance() As T
            Return New T
        End Function
    End Class

    Public Class SomeBaseClass
    End Class

The baseClass (if none given it is Object) must have a parameter less constructor.

*This can also be done at runtime through [reflection][1]*


  [1]: https://www.wikiod.com/vb-dotnet/reflection#Create an instance of a generic type

