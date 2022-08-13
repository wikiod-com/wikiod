---
title: "Showing a form"
slug: "showing-a-form"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

This topic explains how the WinForms engine works to display forms and how you control their lifetimes.

## Closing a modal form
When a form is shown using the `ShowDialog` method, it is necessary to set the form's `DialogResult` property to close to form. This property can be set using the enum that's also called [DialogResult][1].  

To close a form, you just need to set the form's `DialogResult` property (to any value by `DialogResult.None`) in some event handler. When your code exits from the event handler the WinForm engine will hide the form and the code that follows the initial `ShowDialog` method call will continue execution.

    private cmdClose_Click(object sender, EventArgs e)
    {
        this.DialogResult = DialogResult.Cancel;
    }

The calling code can capture the return value from ShowDialog to determine what button the user clicked in the form. When displayed using `ShowDialog()`, the form is not disposed of automatically (since it was simply hidden and not closed), so it is important to use an `using` block to ensure the form is disposed.

Below is an example of checking the result of using the built-in `OpenFileDialog`, checking the result, and accessing a property from the dialog before disposing it.

    using (var form = new OpenFileDialog())
    {
        DialogResult result = form.ShowDialog();
        if (result == DialogResult.OK)
        {
            MessageBox.Show("Selected file is: " + form.FileName);
        }
    }

You can also set the `DialogResult` property on a button. Clicking that button will set the `DialogResult` property on the form to the value associated with the button. This allows you close the form without adding an event handler to set the `DialogResult` in the code.

For example, if you add an OK button to your form and sets its property to `DialogResult.OK` then the form closes automatically when you press that button and the calling code receives a `DialogResult.OK` in return from the `ShowDialog()` method call.

  [1]: https://msdn.microsoft.com/en-us/library/system.windows.forms.dialogresult(v=vs.110).aspx

## Show a modeless or a modal form
After defining the structure of your form with the WinForms designer you can display your forms in code with two different methods.

 - Method - A Modeless form

        Form1 aForm1Instance = new Form1(); 
        aForm1Instance.Show();

 - Method - A Modal Dialog

        Form2 aForm2Instance = new Form2(); 
        aForm2Instance.ShowDialog();

The two methods have a very important distinction. The first method (the modeless one) shows your form and then returns immediately without waiting the closure of the just opened form. So your code continues with whatever follows the Show call.
The second method instead (the modal one) opens the form and blocks any activity on the whole application until you close the form via the close button or with some buttons appropriately configured to close the form

## Closing a modeless form
A modeless form  is employed (usually) when you need to shows something permanentely alongside your application main screen (think about a legend or an view on a stream of data coming asynchronously from a device or an MDI Child Window).  
But a modeless form poses an unique challenge when you want to close it. How to retrieve the instance and call the Close method in that instance? 

You can keep a global variable referencing the instance you want to close. 

    theGlobalInstance.Close();
    theGlobalInstance.Dispose();
    theGlobalInstance = null;

But we can also choose to use the Application.OpenForms collection where the form engine stores all the form instances created and still open.

You can retrieve that particular instance from this collection and call the Close method 

    Form2 toClose = Application.OpenForms.OfType<Form2>().FirstOrDefault();
    if(toClose != null)
    {
        toClose.Close();
        toClose.Dispose();
    }


