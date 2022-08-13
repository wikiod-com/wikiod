---
title: "Getting started with winforms"
slug: "getting-started-with-winforms"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating a Simple WinForms Application using Visual Studio
This example will show you how to create a Windows Forms Application project in Visual Studio.

## Create Windows Forms Project

1. Start Visual Studio. 

2. On the **File** menu, point to **New**, and then select **Project**. The **New Project** dialog box appears.

3. In the **Installed Templates** pane, select "Visual C#" or "Visual Basic". 

4. Above the middle pane, you can select the target framework from the drop-down list.

5. In the middle pane, select the **Windows Forms Application** template.

6. In the **Name** text box, type a name for the project.

7. In the **Location** text box, choose a folder to save the project.

8. Click **OK**.

9. The Windows Forms Designer opens and displays **Form1** of the project.

## Add Controls to the Form

1. From the **Toolbox** palette, drag a **Button** control onto the form.

2. Click the button to select it. In the Properties window, set the `Text` property to **Say Hello**.

    [![Visual Studio designer, showing a form with a button][1]][1]

## Write Code

1. Double-click the button to add an event handler for the `Click` event. The Code Editor will open with the insertion point placed within the event handler function.

2. Type the following code:

    **C#**

       MessageBox.Show("Hello, World!");

    **VB.NET**

       MessageBox.Show("Hello, World!")

## Run and Test

1. Press <kbd>F5</kbd> to run the application.

    [![The form, as displayed when running the application][2]][2]

2. When your application is running, click the button to see the "Hello, World!" message.

    [![A Message Box that says "Hello, World!"][3]][3]

3. Close the form to return to Visual Studio.


  [1]: http://i.stack.imgur.com/h6Cfv.png
  [2]: http://i.stack.imgur.com/9HxmL.png
  [3]: http://i.stack.imgur.com/r9Cg6.png

## Creating a Simple C# WinForms Application using a Text Editor
1. Open a text editor (like Notepad), and type the code below:

   <!-- language: lang-cs -->

        using System;
        using System.ComponentModel;
        using System.Drawing;
        using System.Windows.Forms;

        namespace SampleApp
        {
            public class MainForm : Form
            {
                private Button btnHello;

                // The form's constructor: this initializes the form and its controls.
                public MainForm()
                {
                    // Set the form's caption, which will appear in the title bar.
                    this.Text = "MainForm";

                    // Create a button control and set its properties.
                    btnHello = new Button();
                    btnHello.Location = new Point(89, 12);
                    btnHello.Name = "btnHello";
                    btnHello.Size = new Size(105, 30);
                    btnHello.Text = "Say Hello";

                    // Wire up an event handler to the button's "Click" event
                    // (see the code in the btnHello_Click function below).
                    btnHello.Click += new EventHandler(btnHello_Click);

                    // Add the button to the form's control collection,
                    // so that it will appear on the form.
                    this.Controls.Add(btnHello);
                }

                // When the button is clicked, display a message.
                private void btnHello_Click(object sender, EventArgs e)
                {
                    MessageBox.Show("Hello, World!");
                }

                // This is the main entry point for the application.
                // All C# applications have one and only one of these methods.
                [STAThread]
                static void Main()
                {
                    Application.EnableVisualStyles();
                    Application.Run(new MainForm());
                }
            }
        }


2. Save the file to a path you have read/write access to. It is conventional to name the file after the class that it contains—for example, `X:\MainForm.cs`.


3. Run the C# compiler from the command line, passing the path to the code file as an argument:

   <!-- language: lang-none -->

        %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe /target:winexe "X:\MainForm.cs"

   *Note:* To use a version of the C# compiler for other .NET framework versions, take a look in the path, `%WINDIR%\Microsoft.NET` and modify the example above accordingly. For more information on compiling C# applications, see https://www.wikiod.com/docs/c%23/15/compile-and-run-your-first-c-sharp-program#t=201607240523313550097. 

   
4. After compilation has completed, an application called `MainForm.exe` will be created in the same directory as your code file. You can run this application either from the command line or by double-clicking on it in Explorer.


## Creating a Simple VB.NET WinForms Application using a Text Editor
1. Open a text editor (like Notepad), and type the code below:

    <!-- language: lang-vb -->

        Imports System.ComponentModel
        Imports System.Drawing
        Imports System.Windows.Forms
    
        Namespace SampleApp
            Public Class MainForm : Inherits Form
                Private btnHello As Button
        
                ' The form's constructor: this initializes the form and its controls.
                Public Sub New()
                    ' Set the form's caption, which will appear in the title bar.
                    Me.Text = "MainForm"
        
                    ' Create a button control and set its properties.
                    btnHello = New Button()
                    btnHello.Location = New Point(89, 12)
                    btnHello.Name = "btnHello"
                    btnHello.Size = New Size(105, 30)
                    btnHello.Text = "Say Hello"
        
                    ' Wire up an event handler to the button's "Click" event
                    ' (see the code in the btnHello_Click function below).
                    AddHandler btnHello.Click, New EventHandler(AddressOf btnHello_Click)
        
                    ' Add the button to the form's control collection,
                    ' so that it will appear on the form.
                    Me.Controls.Add(btnHello)
                End Sub
        
                ' When the button is clicked, display a message.
                Private Sub btnHello_Click(sender As Object, e As EventArgs)
                    MessageBox.Show("Hello, World!")
                End Sub
        
                ' This is the main entry point for the application.
                ' All VB.NET applications have one and only one of these methods.
                <STAThread> _
                Public Shared Sub Main()
                    Application.EnableVisualStyles()
                    Application.Run(New MainForm())
                End Sub
            End Class
        End Namespace


2. Save the file to a path you have read/write access to. It is conventional to name the file after the class that it contains—for example, `X:\MainForm.vb`.


3. Run the VB.NET compiler from the command line, passing the path to the code file as an argument:

    <!-- language: lang-none -->

        %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\vbc.exe /target:winexe "X:\MainForm.vb"
    
    *Note:* To use a version of the VB.NET compiler for other .NET framework versions, take a look in the path `%WINDIR%\Microsoft.NET` and modify the example above accordingly. For more information on compiling VB.NET applications, see https://www.wikiod.com/vb-dotnet/getting-started-with-visual-basic-net-language


4. After compilation has completed, an application called `MainForm.exe` will be created in the same directory as your code file. You can run this application either from the command line or by double-clicking on it in Explorer.

