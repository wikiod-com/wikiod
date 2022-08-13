---
title: "Help Integration"
slug: "help-integration"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

You can provide help for Forms and Controls in a Windows Forms Applications in different ways. You can show a pop-up help, open a CHM file or a URL. You can show context-sensitive help for Forms, Controls and dialogs.

## HelpProvider Component

You can setup a [`HelpProvider`](https://msdn.microsoft.com/en-us/library/2ksk25ts(v=vs.110).aspx) component to provide context sensitive help for component. This way when the user press <kbd>F1</kbd> key or Help button of form, you can automatically:

 - Show a context sensitive help pop-up for controls
 - Open a CHM file based on context (Show table of content, Show a keyword or index, show a topic)
 - Navigate to a URL using default browser

## Help Class

You can use [`Help`](https://msdn.microsoft.com/en-us/library/system.windows.forms.help(v=vs.110).aspx) class in code, to provide these kinds of help:

 - Show a help pop-up for a control
 - Open a CHM file based on context (Show table of content, Show a keyword or index, show a topic)
 - Navigate to a URL using default browser

## HelpRequested Event

You can handle [`HelpRequested`](https://msdn.microsoft.com/en-us/library/system.windows.forms.control.helprequested(v=vs.110).aspx) event of `Control` objects or `Form` to perform custom actions when the user press <kbd>F1</kbd> or click on Help button of form.

## Help Button of Form

You can setup the `Form` to show Help button on title-bar. This way, if the user click on Help button, the cursor will change to a `?` cursor and after click on any point, any context-sensitive help associated with the control using `HelpProvider` will be show.

## Help button of MessgeBox and CommonDialogs

You can provide help for `MessageBox`, `OpenFileDialog`, `SaveDialog` and `ColorDialog` using Help button of the components.

## ToolTip Component 

You can use [`ToolTip`](https://msdn.microsoft.com/en-us/library/he23h308(v=vs.110).aspx) component to display some help text when the user points at controls. A `ToolTip` can be associated with any control. 


**Note**

Using `HelpProvider` and `Help` class You can show compiled Help files (.chm) or HTML files in the HTML Help format. Compiled Help files provide a table of contents, an index, search capability, and keyword links in pages. Shortcuts work only in compiled Help files.
You can generate HTML Help 1.x files by using HTML Help Workshop. For more information about HTML Help, see "HTML Help Workshop" and other HTML Help topics at [Microsoft HTML Help](https://msdn.microsoft.com/en-us/library/ms670169(VS.85).aspx).

## Show help file
The `Help Class` encapsulates the HTML Help 1.0 engine. You can use the Help object to show compiled Help files (.chm) or HTML files in the HTML Help format. Compiled Help files provide table of contents, index, search, and keyword links in pages. Shortcuts work only in compiled Help files. You can generate HTML Help 1.x files with a free tool from Microsft called `HTML Help Workshop`.

An easy way to show a compiled help file in a second window:

**C#**

    Help.ShowHelp(this, helpProviderMain.HelpNamespace);

**VB.NET**

    Help.ShowHelp(Me, hlpProviderMain.HelpNamespace)



## Handling HelpRequested event of Controls and Form
When a user press <kbd>F1</kbd> on a control or click on Help button of form (<kbd>?</kbd>) and then clicks on a control the [`HelpRequested`](https://msdn.microsoft.com/en-us/library/system.windows.forms.control.helprequested(v=vs.110).aspx) event will be raised.

You can handle this event to provide custom action when user requests help for controls or form.

The [`HelpRequested`](https://msdn.microsoft.com/en-us/library/system.windows.forms.control.helprequested(v=vs.110).aspx) supports bubble up mechanism. It fires for your active control and if you don't handle the event and not set [`Handled`](https://msdn.microsoft.com/en-us/library/system.windows.forms.helpeventargs.handled(v=vs.110).aspx) property of its event arg to `true`, then it bubbles up to the parent control hierarchy up to form. 

For example if you handle `HelpRequested` event of the form like below, then when you press <kbd>F1</kbd> a message box will pop up and show name of active control, but for `textBox1` it will show a different message:

    private void Form1_HelpRequested(object sender, HelpEventArgs hlpevent)
    {
        var c = this.ActiveControl;
        if(c!=null)
            MessageBox.Show(c.Name);
    }
    private void textBox1_HelpRequested(object sender, HelpEventArgs hlpevent)
    {
        hlpevent.Handled = true;
        MessageBox.Show("Help request handled and will not bubble up");
    }

You can perform any other custom action like using navigating to a URL or showing a CHM file using [`Help`](https://msdn.microsoft.com/en-us/library/system.windows.forms.help(v=vs.110).aspx) class.


## Create custom Help button which acts like standard Form HelpButton
If you have a `Form` with `MinimizeBox` and `MaximizeBox` set to `true`, then you can not show Help button on title-bar of `Form` and will lose the feature of click on help button to convert it to help cursor to be able to click on controls to show help.

You can make a menu item on `MenuStrip` act like standard Help Button. To do so, add a `MenuStrip` to the form and add a `ToolStripMenuItem` to it, then handle `Click` event of the item:

    private const int WM_SYSCOMMAND = 0x0112;
    private const int SC_CONTEXTHELP = 0xF180;
    [System.Runtime.InteropServices.DllImport("user32.dll")]
    static extern IntPtr SendMessage(IntPtr hWnd, int Msg, int wParam, int lParam);
    private void helpToolStripMenuItem_Click(object sender, EventArgs e)
    {
        SendMessage(this.Handle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0);
    }

**Note:** If you want to do it using a `Button`, you also need to set `button1.Capture = false;` before sending the message. But it's not necessary for a `ToolStripMenuItem`.

Then when you click on the help menu, the cursor will be changed to `?` cursor and will act like when you click on standard Help button:


[![Custom Help Button][1]][1]




  [1]: http://i.stack.imgur.com/vX6Fk.png

## Show Help for MessageBox
You can provide help for message box in different ways. You can configure a `MessageBox` to show a `Help` button or not. Also you can configure `MessageBox` in a way that when the user requests for help by click on Help button or by pressing <kbd>F1</kbd>, it show a CHM file or navigate to a URL or perform a custom action. Here are some examples in this topic.

In all below examples, the `MessageBox` would be like this:

[![MessageBox with Help Button][1]][1]


## Show a CHM file and navigate to a keyword (index)

    MessageBox.Show("Some Message", "Title", MessageBoxButtons.YesNoCancel,
        MessageBoxIcon.Question, MessageBoxDefaultButton.Button3, 0,
        "help.chm", HelpNavigator.KeywordIndex, "SomeKeyword");

## Show a CHM file and navigate to a topic

    MessageBox.Show("Some Message", "Title", MessageBoxButtons.YesNoCancel,
        MessageBoxIcon.Question, MessageBoxDefaultButton.Button3, 0,
        "help.chm", HelpNavigator.Topic, "/SomePath/SomePage.html");

## Show a CHM file and navigate first help page in table of contents

    MessageBox.Show("Some Message", "Title", MessageBoxButtons.YesNoCancel,
        MessageBoxIcon.Question, MessageBoxDefaultButton.Button3, 0,
        "help.chm");

## Open default browser and navigate to a URL

    MessageBox.Show("Some Message", "Title", MessageBoxButtons.YesNoCancel,
        MessageBoxIcon.Question, MessageBoxDefaultButton.Button3, 0,
        "http://example.com");

## Perform custom action on when pressing Help Button or F1 Key

In this case you should handle [`HelpRequested`](https://msdn.microsoft.com/en-us/library/system.windows.forms.control.helprequested(v=vs.110).aspx) event of parent of `MessageBox` and perform custom operation:

    private void Form1_HelpRequested(object sender, HelpEventArgs hlpevent)
    {
        // Perform custom action, for example show a custom help form
        var f = new Form();  
        f.ShowDialog();
    }

Then you can show the `MessageBox` with Help button:

    MessageBox.Show("Some Message", "Title", MessageBoxButtons.YesNoCancel,
        MessageBoxIcon.Question, MessageBoxDefaultButton.Button3, 0, true);


Or show it without Help button:

    MessageBox.Show("Some Message", "Title", MessageBoxButtons.YesNoCancel,
        MessageBoxIcon.Question, MessageBoxDefaultButton.Button3, 0, false);

 

  [1]: http://i.stack.imgur.com/GahF4.png

## Show Help for CommonDialogs
You can provide help for `OpenFileDialog`, `SaveFileDialog` and `ColorDialog`. To do so set `ShowHelp` property of dialog to `true` and handle [`HelpRequest`](https://msdn.microsoft.com/en-us/library/system.windows.forms.commondialog.helprequest(v=vs.110).aspx) event for dialog:


    void openFileDialog1_HelpRequest(object sender, EventArgs e)
    {
        //Perform custom action
        Help.ShowHelp(this, "Http://example.com");
    }

**Note**

 - The event will be raised only if you set `ShowHelp` to `true`.
 - The event will be raised only by click on `Help` button and will not raise using F1 key.


In the image below you can see an `OpenFileDialog` with a Help button:

[![Open File Dialog with Help Button][1]][1]


  [1]: http://i.stack.imgur.com/mcvGL.png

## Show Help using Help class
You can use [`Help`](https://msdn.microsoft.com/en-us/library/system.windows.forms.help(v=vs.110).aspx) class in code, to provide these kinds of help:

 - Show a help pop-up for a control
- Open a CHM file based on context (Show table of content, Show a keyword or index, show a topic)
- Navigate to a URL using default browser


## Show Help pop-up window

You can use [`Help.ShowPopup`](https://msdn.microsoft.com/en-us/library/system.windows.forms.help.showpopup(v=vs.110).aspx) to display a help pop-up window:

    private void control_MouseClick(object sender, MouseEventArgs e)
    {
        var c = (Control)sender;
        var help = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, " +
                   "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
        if (c != null)
            Help.ShowPopup(c, "Lorem ipsum dolor sit amet.", c.PointToScreen(e.Location));
    }

It will show such help pop-up  at your mouse pointer location:

[![enter image description here][1]][1]

## Show CHM Help file

You can use different overloads of `Help.ShowHelp` method, to show a CHM file and navigate to a keyword, a topic, index or table of content:

### Show Help Table of Content

    Help.ShowHelp(this, "Help.chm"); 


### Show Help for specific Keyword (index)

    Help.ShowHelp(this, "Help.chm", HelpNavigator.Index, "SomeKeyword");

### Show Help for specific Topic

    Help.ShowHelp(this, "Help.chm", HelpNavigator.Topic, "/SomePath/SomePage.html");


## Show Url

You can show any URL in default browser using `ShowHelp` method:

    Help.ShowHelp(this, "Http://example.com");
  [1]: http://i.stack.imgur.com/UrZtU.png

## Show Help Button on Titlebar of Form
You can show a Help Button at title-bar of a `Form`. To do so, you should:

1. Set [`HelpButton`](https://msdn.microsoft.com/en-us/library/system.windows.forms.form.helpbutton(v=vs.110).aspx) property of form to `true`.
2. Set `MinimizeBox` and `MaximizeBox` to `false`.

Then a help button will appear on title-bar of `Form`:

[![enter image description here][1]][1]

Also when you click on Help button, the cursor will be changed to a `?` cursor:

[![enter image description here][2]][2]

Then if you click on a `Control` or `Form`, the `HelpRequested` event will be raised and also if you have setup a `HelpProvider`, the help for the control will be shown using `HelpProvider`.


  [1]: http://i.stack.imgur.com/otusY.png
  [2]: http://i.stack.imgur.com/HS79o.png

## Handling HelpButtonClicked event of Form
You can detect when a user Clicked on a `HelpButton` on title-bar of form by handling [`HelpButtonClicked`](https://msdn.microsoft.com/en-us/library/system.windows.forms.form.helpbuttonclicked(v=vs.110).aspx). You can let the event continue or cancel it by setting `Cancel` property of its event args to `true`.

    private void Form1_HelpButtonClicked(object sender, CancelEventArgs e)
    {
        e.Cancel = true;
        //Perform some custom action
        MessageBox.Show("Some Custom Help");
    }

