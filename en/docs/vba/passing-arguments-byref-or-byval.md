---
title: "Passing Arguments ByRef or ByVal"
slug: "passing-arguments-byref-or-byval"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

The `ByRef` and `ByVal` modifiers are part of a procedure's signature and indicate how an argument is passed to a procedure. In VBA a parameter is passed `ByRef` unless specified otherwise (i.e. `ByRef` is implicit if absent).

**Note** In many other programming languages (including VB.NET), parameters are implicitly passed by value if no modifier is specified: consider specifying `ByRef` modifiers explicitly to avoid possible confusion.


## Passing arrays

Arrays **must** be passed by reference. This code compiles, but raises run-time error 424 "Object Required":

    Public Sub Test()
        DoSomething Array(1, 2, 3)
    End Sub
    
    Private Sub DoSomething(ByVal foo As Variant)
        foo.Add 42
    End Sub
    
This code does not compile:

    Private Sub DoSomething(ByVal foo() As Variant) 'ByVal is illegal for arrays
        foo.Add 42
    End Sub


## Passing Simple Variables ByRef And ByVal
Passing `ByRef` or `ByVal` indicates whether the actual value of an argument is passed to the `CalledProcedure` by the `CallingProcedure`, or whether a reference (called a pointer in some other languages) is passed to the `CalledProcedure`. 

If an argument is passed `ByRef`, the memory address of the argument is passed to the `CalledProcedure` and any modification to that parameter by the `CalledProcedure` is made to the value in the `CallingProcedure`. 

If an argument is passed `ByVal`, the actual value, not a reference to the variable, is passed to the `CalledProcedure`.

A simple example will illustrate this clearly:
    
    Sub CalledProcedure(ByRef X As Long, ByVal Y As Long)
        X = 321
        Y = 654
    End Sub

    Sub CallingProcedure()
        Dim A As Long
        Dim B As Long
        A = 123
        B = 456

        Debug.Print "BEFORE CALL => A: " & CStr(A), "B: " & CStr(B)
        ''Result : BEFORE CALL => A: 123 B: 456

        CalledProcedure X:=A, Y:=B

        Debug.Print "AFTER CALL =  A: " & CStr(A), "B: " & CStr(B)
        ''Result : AFTER CALL => A: 321 B: 456
    End Sub

Another example:  

    Sub Main()
        Dim IntVarByVal As Integer
        Dim IntVarByRef As Integer
        
        IntVarByVal = 5
        IntVarByRef = 10
        
        SubChangeArguments IntVarByVal, IntVarByRef '5 goes in as a "copy". 10 goes in as a reference
        Debug.Print "IntVarByVal: " & IntVarByVal 'prints 5 (no change made by SubChangeArguments)
        Debug.Print "IntVarByRef: " & IntVarByRef 'prints 99 (the variable was changed in SubChangeArguments)
    End Sub
    
    Sub SubChangeArguments(ByVal ParameterByVal As Integer, ByRef ParameterByRef As Integer)
        ParameterByVal = ParameterByVal + 2 ' 5 + 2 = 7 (changed only inside this Sub)
        ParameterByRef = ParameterByRef + 89 ' 10 + 89 = 99 (changes the IntVarByRef itself - in the Main Sub)
    End Sub

## ByRef

---

## Default modifier

If no modifier is specified for a parameter, that parameter is implicitly passed by reference.

    Public Sub DoSomething1(foo As Long)
    End Sub

<!-- -->

    Public Sub DoSomething2(ByRef foo As Long)
    End Sub

The `foo` parameter is passed `ByRef` in both `DoSomething1` and `DoSomething2`.

> **Watch out!** If you're coming to VBA with experience from other languages, this is very likely the exact opposite behavior to the one you're used to. In many other programming languages (including VB.NET), the implicit/default modifier passes parameters by value.

---

### Passing by reference

- When a *value* is passed `ByRef`, the procedure receives **a reference** to the value.

      Public Sub Test()
          Dim foo As Long
          foo = 42
          DoSomething foo
          Debug.Print foo
      End Sub

      Private Sub DoSomething(ByRef foo As Long)
          foo = foo * 2
      End Sub

    Calling the above `Test` procedure outputs 84. `DoSomething` is given `foo` and receives a *reference* to the value, and therefore works with the same memory address as the caller.

- When a *reference* is passed `ByRef`, the procedure receives **a reference** to the pointer.

      Public Sub Test()
          Dim foo As Collection
          Set foo = New Collection
          DoSomething foo
          Debug.Print foo.Count
      End Sub

      Private Sub DoSomething(ByRef foo As Collection)
          foo.Add 42
          Set foo = Nothing
      End Sub

    The above code raises [run-time error 91](https://www.wikiod.com/vba/vba-run-time-errors#Run-time error '91': Object variable or With block variable not set), because the caller is calling the `Count` member of an object that no longer exists, because `DoSomething` was given a *reference* to the object pointer and assigned it to `Nothing` before returning.

---

## Forcing ByVal at call site

Using parentheses at the call site, you can override `ByRef` and force an argument to be passed `ByVal`:

    Public Sub Test()
        Dim foo As Long
        foo = 42
        DoSomething (foo)
        Debug.Print foo
    End Sub

    Private Sub DoSomething(ByRef foo As Long)
        foo = foo * 2
    End Sub

The above code outputs 42, regardless of whether `ByRef` is specified implicitly or explicitly.

> **Watch out!** Because of this, using extraneous parentheses in procedure calls can easily introduce bugs. Pay attention to the whitespace between the procedure name and the argument list:
>
>     bar = DoSomething(foo) 'function call, no whitespace; parens are part of args list
>     DoSomething (foo) 'procedure call, notice whitespace; parens are NOT part of args list
>     DoSomething foo 'procedure call does not force the foo parameter to be ByVal



## ByVal
### Passing by value

- When a *value* is passed `ByVal`, the procedure receives **a copy** of the value.

      Public Sub Test()
          Dim foo As Long
          foo = 42
          DoSomething foo
          Debug.Print foo
      End Sub

      Private Sub DoSomething(ByVal foo As Long)
          foo = foo * 2
      End Sub

    Calling the above `Test` procedure outputs 42. `DoSomething` is given `foo` and receives **a copy** of the value. The copy is multiplied by 2, and then discarded when the procedure exits; the caller's copy was never altered.

- When a *reference* is passed `ByVal`, the procedure receives **a copy** of the pointer.

      Public Sub Test()
          Dim foo As Collection
          Set foo = New Collection
          DoSomething foo
          Debug.Print foo.Count
      End Sub

      Private Sub DoSomething(ByVal foo As Collection)
          foo.Add 42
          Set foo = Nothing
      End Sub

    Calling the above `Test` procedure outputs 1. `DoSomething` is given `foo` and receives *a copy* of **the pointer** to the `Collection` object. Because the `foo` object variable in the `Test` scope points to the same object, adding an item in `DoSomething` adds the item to the same object. Because it's *a copy* of the pointer, setting its reference to `Nothing` does not affect the caller's own copy.

