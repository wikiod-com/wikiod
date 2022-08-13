---
title: "Creating Own MessageBox in Windows Form Application"
slug: "creating-own-messagebox-in-windows-form-application"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

First we need to know what a MessageBox is...

The MessageBox control displays a message with specified text, and can be customised by specifying a custom image, title and button sets (These button sets allow the user to choose more than a basic yes/no answer).

By creating our own MessageBox we can re-use that MessageBox Control in any new applications just by using the generated dll, or copying the file containing the class.

## Syntax
 - 'static DialogResult result = DialogResult.No; //DialogResult is returned by dialogs after dismissal.'

## Creating Own MessageBox Control.
To create our own MessageBox control simply follow the guide below...

1. Open up your instance of Visual Studio (VS 2008/2010/2012/2015/2017)

2. Go to the toolbar at the top and click File -> New Project --> Windows Forms Application --> Give the project a name and then click ok.
3. Once loaded, drag and drop a button control from the Toolbox (found on the left) onto the form (as shown below).

[![enter image description here][1]][1]


4. Double click the button and the Integrated Development Environment will automatically generate the click event handler for you.

5. Edit the code for the form so that it looks like the following (You can right-click the form and click Edit Code):


    namespace MsgBoxExample {
        public partial class MsgBoxExampleForm : Form {
            //Constructor, called when the class is initialised.
            public MsgBoxExampleForm() {
                InitializeComponent();
            }

            //Called whenever the button is clicked.
            private void btnShowMessageBox_Click(object sender, EventArgs e) {
               CustomMsgBox.Show($"I'm a {nameof(CustomMsgBox)}!", "MSG", "OK");
            }
        }
    }

6. Solution Explorer -> Right Click on your project --> Add --> Windows Form and set the name as "CustomMsgBox.cs"

7. Drag in a button & label control from the Toolbox to the form (It'll look something like the form below after doing it):


[![enter image description here][2]][2]

8. Now write out the code below into the newly created form:


    private DialogResult result = DialogResult.No;
    public static DialogResult Show(string text, string caption, string btnOkText) {
        var msgBox = new CustomMsgBox();
        msgBox.lblText.Text = text; //The text for the label...
        msgBox.Text = caption; //Title of form
        msgBox.btnOk.Text = btnOkText; //Text on the button
        //This method is blocking, and will only return once the user
        //clicks ok or closes the form.
        msgBox.ShowDialog(); 
        return result;
    }

    private void btnOk_Click(object sender, EventArgs e) {
        result = DialogResult.Yes;
        MsgBox.Close();
    }

9. Now run the program by just pressing F5 Key.
Congratulations, you've made a reusable control.

[1]: https://i.stack.imgur.com/aW1q1.jpg
[2]: https://i.stack.imgur.com/73c1M.jpg









## How to use own created MessageBox control in another Windows Form application.
To find your existing .cs files, right click on the project in your instance of Visual Studio, and click Open Folder in File Explorer.

1. Visual Studio --> Your current project (Windows Form) --> Solution Explorer --> Project Name --> Right Click --> Add --> Existing Item --> Then locate your existing .cs file.

2. Now there's one last thing to do in order to use the control. Add a using statement to your code, so that your assembly knows about its dependencies.

       using System;
       using System.Collections.Generic;
       using System.ComponentModel;
       using System.Data;
       using System.Drawing;
       .
       .
       .
       using CustomMsgBox; //Here's the using statement for our dependency.

3. To display the messagebox, simply use the following...

    CustomMsgBox.Show("Your Message for Message Box...","MSG","OK");

