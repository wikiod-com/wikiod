---
title: "Getting started with excel-vba"
slug: "getting-started-with-excel-vba"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Opening the Visual Basic Editor (VBE)
----------


**Step 1: Open a Workbook**

[![enter image description here][1]][1]


----------


**Step 2 Option A: Press <kbd>Alt</kbd> + <kbd>F11</kbd>**

This is the standard shortcut to open the VBE.

**Step 2 Option B: Developer Tab --> View Code**

First, the Developer Tab must be added to the ribbon.  Go to File -> Options -> Customize Ribbon, then check the box for developer.

[![enter image description here][2]][2]

Then, go to the developer tab and click "View Code" or "Visual Basic"

[![enter image description here][3]][3]

**Step 2 Option C: View tab > Macros > Click Edit button to open an Existing Macro**

All three of these options will open the Visual Basic Editor (VBE):

[![enter image description here][4]][4]


  [1]: http://i.stack.imgur.com/MHMA9.png
  [2]: http://i.stack.imgur.com/8WoiR.png
  [3]: http://i.stack.imgur.com/388eU.png
  [4]: http://i.stack.imgur.com/azT5a.png

## Declaring Variables
To explicitly declare variables in VBA, use the `Dim` statement, followed by the variable name and type. If a variable is used without being declared, or if no type is specified, it will be assigned the type `Variant`.

Use the `Option Explicit` statement on first line of a module to force all variables to be declared before usage (see https://www.wikiod.com/excel-vba/vba-best-practices#ALWAYS Use "Option Explicit" ).

Always using `Option Explicit` is highly recommended because it helps prevent typo/spelling errors and ensures variables/objects will stay their intended type.

    Option Explicit

    Sub Example()
        Dim a As Integer
        a = 2
        Debug.Print a
        'Outputs: 2
    
        Dim b As Long
        b = a + 2
        Debug.Print b
        'Outputs: 4
    
        Dim c As String
        c = "Hello, world!"
        Debug.Print c
        'Outputs: Hello, world!
    End Sub

Multiple variables can be declared on a single line using commas as delimiters, but **each type must be declared individually**, or they will default to the `Variant` type.

    Dim Str As String, IntOne, IntTwo As Integer, Lng As Long
    Debug.Print TypeName(Str)    'Output: String
    Debug.Print TypeName(IntOne) 'Output: Variant <--- !!!
    Debug.Print TypeName(IntTwo) 'Output: Integer
    Debug.Print TypeName(Lng)    'Output: Long

Variables can also be declared using Data Type Character suffixes ($ % & ! # @), however using these are increasingly discouraged.

     Dim this$  'String
     Dim this%  'Integer
     Dim this&  'Long
     Dim this!  'Single
     Dim this#  'Double
     Dim this@  'Currency

Other ways of declaring variables are:
--------------------------------------

 - `Static` like: `Static CounterVariable as Integer`

> When you use the Static statement instead of a Dim statement, the declared variable will retain its value between calls.

 - `Public` like: `Public CounterVariable as Integer`

> Public variables can be used in any procedures in the project. If a public variable is declared in a standard module or a class module, it can also be used in any projects that reference the project where the public variable is declared.

 - `Private` like: `Private CounterVariable as Integer` 

> Private variables can be used only by procedures in the same module.

Source and more info:

[MSDN-Declaring Variables](https://msdn.microsoft.com/en-us/library/office/gg264241.aspx)

[Type Characters (Visual Basic)](https://docs.microsoft.com/en-us/dotnet/visual-basic/programming-guide/language-features/data-types/type-characters)



## Adding a new Object Library Reference
The procedure describes how to add an Object library reference, and afterwards how to declare new variables with reference to the new library class objects.

The example below shows how to add the *PowerPoint* library to the existing VB Project.
As can be seen, currently the PowerPoint Object library is not available.

[![enter image description here][1]][1]

**Step 1**: Select Menu ***Tools*** **-->** ***References…***
[![enter image description here][2]][2]

**Step 2**: Select the Reference you want to add. This example we scroll down to find “***Microsoft PowerPoint 14.0 Object Library***”, and then press “**OK**”.
[![enter image description here][3]][3]

Note: PowerPoint 14.0 means that Office 2010 version is installed on the PC.

**Step 3**: in the VB Editor, once you press **Ctrl+Space** together, you get the autocomplete option of PowerPoint.
[![enter image description here][4]][4]


After selecting `PowerPoint` and pressing `.`, another menu appears with all objects options related to the PowerPoint Object Library.
This example shows how to select the PowerPoint's object `Application`.
[![enter image description here][5]][5]



**Step 4**: Now the user can declare more variables using the PowerPoint object library.

Declare a variable that is referencing the `Presentation` object of the PowerPoint object library.
[![enter image description here][6]][6]

Declare another variable that is referencing the `Slide` object of the PowerPoint object library.
[![enter image description here][7]][7]

Now the variables declaration section looks like in the screen-shot below, and the user can start using these variables in his code.
[![enter image description here][8]][8]



Code version of this tutorial:

    Option Explicit
    
    Sub Export_toPPT()
    
    Dim ppApp As PowerPoint.Application
    Dim ppPres As PowerPoint.Presentation
    Dim ppSlide As PowerPoint.Slide
    
    ' here write down everything you want to do with the PowerPoint Class and objects
    
    
    End Sub

  [1]: http://i.stack.imgur.com/0IwJy.jpg
  [2]: http://i.stack.imgur.com/yfb7J.jpg
  [3]: http://i.stack.imgur.com/vsKbO.jpg
  [4]: http://i.stack.imgur.com/6DoDc.jpg
  [5]: http://i.stack.imgur.com/Av3V7.jpg
  [6]: http://i.stack.imgur.com/dzCOc.jpg
  [7]: http://i.stack.imgur.com/QARnI.jpg
  [8]: http://i.stack.imgur.com/bfQff.jpg

## Hello World

 1. Open the Visual Basic Editor ( see  https://www.wikiod.com/excel-vba/getting-started-with-excel-vba#Opening the Visual Basic Editor (VBE) )
 2. Click Insert --> Module to add a new Module :

[![enter image description here][1]][1]

 3. Copy and Paste the following code in the new module :


      Sub hello()
        MsgBox "Hello World !"
      End Sub

To obtain :

 [![enter image description here][2]][2]
       

 4.  Click on the green “play” arrow (or press F5)  in the Visual Basic toolbar to run the  program: 
[![enter image description here][3]][3]

 5.  Select the new created sub "hello" and click `Run` :
[![enter image description here][4]][4]

 6. Done, your should see the following window:


[![enter image description here][5]][5]


  [1]: http://i.stack.imgur.com/0KhKM.png
  [2]: http://i.stack.imgur.com/wv7kE.png
  [3]: http://i.stack.imgur.com/aFU8E.png
  [4]: http://i.stack.imgur.com/Mcj1X.png
  [5]: http://i.stack.imgur.com/j88GC.png

## Getting Started with the Excel Object Model
>  This example intend to be a gentle introduction to the Excel Object Model **for beginners**.


----------


 1. Open the Visual Basic Editor (VBE)
 2. Click View --> Immediate Window to open the Immediate Window (or <kbd>ctrl</kbd> + <kbd>G</kbd>):

 
[![enter image description here][1]][1]

 3. You should see the following Immediate Window at the bottom on VBE:

[![enter image description here][2]][2]

This window allow you to directly test some VBA code.  So let's start, type in this console :

    ?Worksheets. 

VBE has intellisense and then it should open a tooltip as in the following figure :

[![enter image description here][3]][3]

Select .Count in the list or directly type `.Cout` to obtain :

    ?Worksheets.Count

 4. Then press Enter. The expression is evaluated and it should returns 1.  This indicates the number of Worksheet currently present in the workbook. The question mark (`?`) is an alias for Debug.Print.

Worksheets is an **Object** and Count is a **Method**. Excel has several Object (`Workbook`, `Worksheet`, `Range`, `Chart` ..) and each of one contains specific methods and properties. You can find the complete list of Object in the [Excel VBA reference][4]. Worksheets Object is presented [here][5] . 

> This Excel VBA reference should become your primary source of information regarding the Excel Object Model.


 5. Now let's try another expression, type (without the `?` character):


    Worksheets.Add().Name = "StackOveflow"


6. Press Enter. This should create a new worksheet called `StackOverflow.`:

[![enter image description here][6]][6]


To understand this expression you need to read the Add function in the aforementioned Excel reference. You will find the following:

    Add:  Creates a new worksheet, chart, or macro sheet. 
    The new worksheet becomes the active sheet. 
    Return Value: An Object value that represents the new worksheet, chart,
     or macro sheet.

So the `Worksheets.Add()` create a new worksheet and return it. Worksheet(**without s**) is itself a Object that [can be found][7] in the documentation and  `Name`  is one of its **property** (see [here][8]). It is defined as :

    Worksheet.Name Property:  Returns or sets a String value that 
     represents the object name.

So, by investigating the different objects definitions we are able to understand this code `Worksheets.Add().Name = "StackOveflow"`. 

`Add()` creates and add a new worksheet and return a **reference** to it, then we set its Name **property** to "StackOverflow"  
 


----------

Now let's be more formal, Excel contains several Objects. These Objects may be composed of one or several collection(s) of Excel objects of the same class.  It is the case for `WorkSheets` which is a collection of  `Worksheet` object. Each Object has some properties and methods that the programmer can interact with. 

> The Excel Object model refers to the Excel **object hierarchy** 

At the top of all objects is the `Application` object, it represents the Excel instance itself. Programming in VBA  requires a good understanding of this hierarchy because we always need a reference to an object to be able to call a Method or to Set/Get a property.

The (very simplified) Excel Object Model can be represented as,

                                Application
                                 Workbooks
                                 Workbook
                                Worksheets
                                 Worksheet
                                  Range



A more detail version for the Worksheet Object (as it is in Excel 2007) is shown below,

[![enter image description here][9]][9]

 

> The full Excel Object Model can be found [here][10].

Finally some objects may have  `events` (ex: `Workbook.WindowActivate`) that are also part of the Excel Object Model. 

 

  [1]: http://i.stack.imgur.com/I57Nk.png
  [2]: http://i.stack.imgur.com/msMIR.png
  [3]: http://i.stack.imgur.com/f1i7c.png
  [4]: https://msdn.microsoft.com/en-us/library/ff194068.aspx
  [5]: https://msdn.microsoft.com/en-us/library/ff821537.aspx
  [6]: http://i.stack.imgur.com/7YbHr.png
  [7]: https://msdn.microsoft.com/en-us/library/ff194464.aspx
  [8]: https://msdn.microsoft.com/en-us/library/ff841127.aspx
  [9]: http://i.stack.imgur.com/3yhD8.png
  [10]: https://msdn.microsoft.com/en-us/library/ff194068(v=office.15).aspx

