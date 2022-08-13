---
title: "Declaring variables"
slug: "declaring-variables"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Syntax
- Public counter As Integer
- Private _counter As Integer
- Dim counter As Integer




## Declaring and assigning a variable using a primitive type
Variables in Visual Basic are declared using the `Dim` keyword. For example, this declares a new variable called `counter` with the data type `Integer`:

    Dim counter As Integer

A variable declaration can also include an [access modifier](https://msdn.microsoft.com/en-us/library/76453kax.aspx), such as `Public`, `Protected`, `Friend`, or `Private`. This works in conjunction with the variable's [scope][1] to determine its accessibility.

| Access Modifier | Meaning
| --- | ---
| [Public][3] | All types which can access the enclosing type
| [Protected][4] | Only the enclosing class and those that inherit from it
| [Friend][5] | All types in the same assembly that can access the enclosing type
| Protected Friend | The enclosing class and its inheritors, *or* the types in the same assembly that can access the enclosing class
| [Private][6]| Only the enclosing type
| [Static][7]| Only on local variables and only initializes once.  


As a shorthand, the `Dim` keyword can be replaced with the access modifier in the variable's declaration:

    Public TotalItems As Integer
    Private counter As Integer

The supported data types are outlined in the table below:

| Type | Alias | Memory allocation | Example
| --- | --- | --- | ---
| SByte | N/A | 1 byte | `Dim example As SByte = 10`
| Int16 | Short | 2 bytes | `Dim example As Short = 10`
| Int32 | Integer | 4 bytes | `Dim example As Integer = 10`
| Int64 | Long | 8 bytes | `Dim example As Long = 10`
| Single | N/A | 4 bytes | `Dim example As Single = 10.95`
| Double | N/A | 8 bytes | `Dim example As Double = 10.95`
| Decimal | N/A | 16 bytes | `Dim example As Decimal = 10.95`
| Boolean | N/A | Dictated by implementing platform | `Dim example As Boolean = True`
| Char | N/A | 2 Bytes | `Dim example As Char = "A"C`
| String | N/A | ![formula][2][source] | `Dim example As String = "Stack Overflow"`
| DateTime | Date | 8 Bytes | `Dim example As Date = Date.Now`
| Byte | N/A | 1 byte | `Dim example As Byte = 10`
| UInt16 | UShort | 2 bytes | `Dim example As UShort = 10`
| UInt32 | UInteger | 4 bytes | `Dim example As UInteger = 10`
| UInt64 | ULong | 8 bytes | `Dim example As ULong = 10`
| Object | N/A | 4 bytes 32 bit architecture, 8 bytes 64 bit architecture | `Dim example As Object = Nothing`

There also exist data identifier and literal type characters usable in replacement for the textual type and or to force literal type:

| Type (or Alias) | Identifier type character | Literal type character
| --- | --- | ---
| Short | N/A | `example = 10S`
| Integer | `Dim example%` | `example = 10%` or `example = 10I`
| Long | `Dim example&` | `example = 10&` or `example = 10L`
| Single | `Dim example!` | `example = 10!` or `example = 10F`
| Double | `Dim example#` | `example = 10#` or `example = 10R`
| Decimal | `Dim example@` | `example = 10@` or `example = 10D`
| Char | N/A | `example = "A"C`
| String | `Dim example$` | N/A
| UShort | N/A | `example = 10US`
| UInteger | N/A | `example = 10UI`
| ULong | N/A | `example = 10UL`

The integral suffixes are also usable with hexadecimal (&H) or octal (&O) prefixes:  
`example = &H8000S` or `example = &O77&`

Date(Time) objects can also be defined using literal syntax:  
`Dim example As Date = #7/26/2016 12:8 PM#`

Once a variable is declared it will exist within the [Scope][1] of the containing type, `Sub` or `Function` declared, as an example:

    Public Function IncrementCounter() As Integer
        Dim counter As Integer = 0
        counter += 1

        Return counter
    End Function

The counter variable will only exist until the `End Function` and then will be out of scope.  If this counter variable is needed outside of the function you will have to define it at class/structure or module level.

    Public Class ExampleClass

        Private _counter As Integer
       
        Public Function IncrementCounter() As Integer
           _counter += 1
           Return _counter
        End Function
    
    End Class

Alternatively, you can use the `Static` (not to be confused with `Shared`) modifier to allow a local variable to retain it's value between calls of its enclosing method:

    Function IncrementCounter() As Integer
        Static counter As Integer = 0
        counter += 1

        Return counter
    End Function


[source]: http://csharpindepth.com/Articles/General/Strings.aspx


  [1]: https://msdn.microsoft.com/en-us/library/1t0wsc67.aspx
  [2]: https://chart.googleapis.com/chart?cht=tx&chl=20%2B%5Cleft%5Clfloor%5Cfrac%7Blength%7D%7B2%7D%5Cright%5Crfloor*4bytes
  [3]: https://msdn.microsoft.com/en-us/library/9dc6we3z.aspx
  [4]: https://msdn.microsoft.com/en-us/library/8050kawf.aspx
  [5]: https://msdn.microsoft.com/en-us/library/08w05ey2.aspx
  [6]: https://msdn.microsoft.com/en-us/library/wx059ey1.aspx
  [7]: https://msdn.microsoft.com/en-us/library/z2cty7t8.aspx

## Levels of declaration – Local and Member variables
**Local variables** - Those declared within a procedure (subroutine or function) of a class (or other structure). In this example, `exampleLocalVariable` is a local variable  declared within `ExampleFunction()`:

    Public Class ExampleClass1
    
        Public Function ExampleFunction() As Integer
            Dim exampleLocalVariable As Integer = 3
            Return exampleLocalVariable
        End Function
    
    End Class

The `Static` keyword allows a local variable to be retained and keep its value after termination (where usually, local variables cease to exist when the containing procedure terminates).

In this example, the console is `024`. On each call to `ExampleSub()` from `Main()` the static variable retains the value it had at the end of the previous call:

    Module Module1
    
        Sub Main()
            ExampleSub()
            ExampleSub()
            ExampleSub()
        End Sub
    
        Public Sub ExampleSub()
            Static exampleStaticLocalVariable As Integer = 0
            Console.Write(exampleStaticLocalVariable.ToString)
            exampleStaticLocalVariable += 2
        End Sub
    
    End Module

**Member variables** - Declared outside of any procedure, at the class (or other structure) level. They may be **instance variables**, in which each instance of the containing class has its own distinct copy of that variable, or `Shared` **variables**, which exist as a single variable associated with the class itself, independent of any instance.

Here, `ExampleClass2` contains two member variables. Each instance of the `ExampleClass2` has an individual `ExampleInstanceVariable` which can be accessed via the class reference. The shared variable `ExampleSharedVariable` however is accessed using the class name:

    Module Module1
    
        Sub Main()
    
            Dim instance1 As ExampleClass4 = New ExampleClass4
            instance1.ExampleInstanceVariable = "Foo"
    
            Dim instance2 As ExampleClass4 = New ExampleClass4
            instance2.ExampleInstanceVariable = "Bar"
    
            Console.WriteLine(instance1.ExampleInstanceVariable)
            Console.WriteLine(instance2.ExampleInstanceVariable)
            Console.WriteLine(ExampleClass4.ExampleSharedVariable)
    
        End Sub
    
        Public Class ExampleClass4
    
            Public ExampleInstanceVariable As String
            Public Shared ExampleSharedVariable As String = "FizzBuzz"
    
        End Class
    
    End Module



## Example of Access Modifiers
In the following example consider you have a solution hosting two projects: **ConsoleApplication1** and **SampleClassLibrary**. The first project will have the classes **SampleClass1** and **SampleClass2**. The second one will have **SampleClass3** and **SampleClass4**. In other words we have two assemblies with two classes each. **ConsoleApplication1** has a reference to **SampleClassLibrary**.

See how **SampleClass1.MethodA** interacts with other classes and methods.

SampleClass1.vb:
<pre>
Imports SampleClassLibrary

Public Class SampleClass1
    Public Sub MethodA()
        'MethodA can call any of the following methods because
        'they all are in the same scope.
        MethodB()
        MethodC()
        MethodD()
        MethodE()

        'Sample2 is defined as friend. It is accessible within
        'the type itself and all namespaces and code within the same assembly.
        Dim class2 As New SampleClass2() 
        class2.MethodA()
        'class2.MethodB() 'SampleClass2.MethodB is not accessible because
                          'this method is private. SampleClass2.MethodB
                          'can only be called from SampleClass2.MethodA,
                          'SampleClass2.MethodC, SampleClass2.MethodD
                          'and SampleClass2.MethodE
        class2.MethodC()
        'class2.MethodD() 'SampleClass2.MethodD is not accessible because
                          'this method is protected. SampleClass2.MethodD
                          'can only be called from any class that inherits
                          'SampleClass2, SampleClass2.MethodA, SampleClass2.MethodC,
                          'SampleClass2.MethodD and SampleClass2.MethodE
        class2.MethodE()

        Dim class3 As New SampleClass3() 'SampleClass3 resides in other
                                         'assembly and is defined as public.
                                         'It is accessible anywhere.
        class3.MethodA()
        'class3.MethodB() 'SampleClass3.MethodB is not accessible because
                          'this method is private. SampleClass3.MethodB can
                          'only be called from SampleClass3.MethodA,
                          'SampleClass3.MethodC, SampleClass3.MethodD
                          'and SampleClass3.MethodE

        'class3.MethodC() 'SampleClass3.MethodC is not accessible because
                          'this method is friend and resides in another assembly.
                          'SampleClass3.MethodC can only be called anywhere from the
                          'same assembly, SampleClass3.MethodA, SampleClass3.MethodB,
                          'SampleClass3.MethodD and SampleClass3.MethodE

        'class4.MethodD() 'SampleClass3.MethodE is not accessible because
                          'this method is protected friend. SampleClass3.MethodD
                          'can only be called from any class that resides inside
                          'the same assembly and inherits SampleClass3,
                          'SampleClass3.MethodA, SampleClass3.MethodB,
                          'SampleClass3.MethodC and SampleClass3.MethodD      

        'Dim class4 As New SampleClass4() 'SampleClass4 is not accessible because
                                          'it is defined as friend and resides in
                                          'other assembly.
    End Sub

    Private Sub MethodB()
        'Doing MethodB stuff...
    End Sub

    Friend Sub MethodC()
        'Doing MethodC stuff...
    End Sub

    Protected Sub MethodD()
        'Doing MethodD stuff...
    End Sub

    Protected Friend Sub MethodE()
        'Doing MethodE stuff...
    End Sub
End Class
</pre>

SampleClass2.vb:
<pre>
Friend Class SampleClass2
    Public Sub MethodA()
        'Doing MethodA stuff...
    End Sub

    Private Sub MethodB()
        'Doing MethodB stuff...
    End Sub

    Friend Sub MethodC()
        'Doing MethodC stuff...
    End Sub

    Protected Sub MethodD()
        'Doing MethodD stuff...
    End Sub

    Protected Friend Sub MethodE()
        'Doing MethodE stuff...
    End Sub
End Class
</pre>
SampleClass3.vb:
<pre>
Public Class SampleClass3
    Public Sub MethodA()
        'Doing MethodA stuff...
    End Sub
    Private Sub MethodB()
        'Doing MethodB stuff...
    End Sub

    Friend Sub MethodC()
        'Doing MethodC stuff...
    End Sub

    Protected Sub MethodD()
        'Doing MethodD stuff...
    End Sub

    Protected Friend Sub MethodE()
        'Doing MethodE stuff...
    End Sub
End Class
</pre>
SampleClass4.vb:
<pre>
Friend Class SampleClass4
    Public Sub MethodA()
        'Doing MethodA stuff...
    End Sub
    Private Sub MethodB()
        'Doing MethodB stuff...
    End Sub

    Friend Sub MethodC()
        'Doing MethodC stuff...
    End Sub

    Protected Sub MethodD()
        'Doing MethodD stuff...
    End Sub

    Protected Friend Sub MethodE()
        'Doing MethodE stuff...
    End Sub
End Class
</pre>

