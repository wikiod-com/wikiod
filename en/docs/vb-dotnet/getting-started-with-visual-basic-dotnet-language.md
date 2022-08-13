---
title: "Getting started with Visual Basic .NET Language"
slug: "getting-started-with-visual-basic-net-language"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
First, install a version of [Microsoft Visual Studio][1], including the free Community edition. Then, create a Visual Basic Console Application project of type *Console Application*, and the following code will print the string `'Hello World'` to the Console:

    Module Module1

        Sub Main()
            Console.WriteLine("Hello World")
        End Sub

    End Module

Then, save and press <kbd>F5</kbd> on the keyboard (or go to the *Debug* menu, then click *Run without Debug* or *Run*) to compile and run the program. `'Hello World'` should appear in the console window.

[![Output window, showing the Hello World.][2]][2]


  [1]: https://www.visualstudio.com/downloads/download-visual-studio-vs
  [2]: http://i.stack.imgur.com/rZcqG.png

## Hello World on a Textbox upon Clicking of a Button
Drag 1 textbox and 1 button

[![enter image description here][1]][1]

Double click the button1 and you will be transferred to the `Button1_Click event`

    Public Class Form1
        Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
    
        End Sub
    End Class

Type the name of the object that you want to target, in our case it is the `textbox1`. `.Text` is the property that we want to use if we want to put a text on it. 

`Property Textbox.Text, gets or sets the current text in the TextBox`. Now, we have `Textbox1.Text`

We need to set the value of that `Textbox1.Text` so we will use the `=` sign. The value that we want to put in the `Textbox1.Text` is `Hello World`. Overall, this is the total code for putting a value of `Hello World` to the `Textbox1.Text`

    TextBox1.Text = "Hello World"

Adding that code to the `clicked event` of `button1`




    Public Class Form1
        Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
            TextBox1.Text = "Hello World"
        End Sub
    End Class

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/sgFeW.jpg
  [2]: http://i.stack.imgur.com/axKMb.jpg

## Region
For the sake of readability, which will be useful for beginners when reading VB code as well for full time developers to maintain the code, we can use "Region" to set a region of the same set of events, functions, or variables:

    #Region "Events"
        Protected Sub txtPrice_TextChanged(...) Handles txtPrice.TextChanged
            'Do the ops here...
        End Sub

        Protected Sub txtTotal_TextChanged(...) Handles txtTotal.TextChanged
            'Do the ops here...
        End Sub

        'Some other events....

    #End Region

This region block could be collapsed to gain some visual help when the code row goes to 1000+. It is also save your scroll efforts.

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/GXRx8.png

Tested on VS 2005, 2008 2010, 2015 and 2017.

## Creating a simple Calculator to get familiar with the interface and code.
1. Once you have installed Visual Studio from https://www.visualstudio.com/downloads/, start a new project. 


2. [![Interface][1]][1] 

3. Select 'Windows Forms Application' from Visual Basic Tab. You can rename it here if you need to. 

4.  Once you click 'OK', you will see this window: 

[![VB.Net editor][2]][2] 

5. Click on the 'Toolbox' tab on the left. The toolbar has 'auto-hide' option enabled by default. To disable this option, click the small symbol between the 'down arrow' symbol and the 'x' symbol, on the top-right corner of Toolbox window.    

6. Get yourself familiar with the tools provided in the box. I have made a calculator interface by using buttons and a Textbox.

[![Calculator][3]][3]

7. Click on the *Properties* tab (It is on the right side of the editor). You can change the *Text* property of a button, and the textbox to rename them. *Font* property can be used to alter the font of the controls. 

8. To write the specific action for an event(eg. clicking on a button), double click on the control. Code window will open. 

[![Sample Code][4]][4]

9. VB.Net is a powerful language designed for fast development. High encapsulation and abstraction is cost for it. You do not need to add *semicolon* to indicate the end of a statement, there are no brackets, and most of the time, it auto-corrects the case of the alphabets. 
10. Code provided in the picture should be simple to understand. 
*Dim* is the keyword used to initialize a variable, and *new* allocates memory.
Anything you type in the textbox is of type *string* by default. Casting is required to use the value as a different type.

Enjoy your first creation in VB.Net!


  [1]: https://i.stack.imgur.com/AEVuZ.png
  [2]: https://i.stack.imgur.com/hd4h6.png
  [3]: https://i.stack.imgur.com/lJykr.png
  [4]: https://i.stack.imgur.com/op2kd.png

