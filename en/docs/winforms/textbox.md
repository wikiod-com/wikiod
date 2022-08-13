---
title: "TextBox"
slug: "textbox"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Auto completion from a collection of strings
    var source = new AutoCompleteStringCollection();

    // Add your collection of strings.
    source.AddRange(new[] { "Guybrush Threepwood", "LeChuck" });

    var textBox = new TextBox
    {
        AutoCompleteCustomSource = source,
        AutoCompleteMode = AutoCompleteMode.SuggestAppend,
        AutoCompleteSource = AutoCompleteSource.CustomSource
    };

    form.Controls.Add(textBox);

This will **autocomplete** the as the user tries to type **G** or **L**.

`AutoCompleteMode.SuggestAppend` will both display a list of suggested values and it will auto type the first match, `Append` only and `Suggest` only are available, too.

## Allow only digits in the text
    textBox.KeyPress += (sender, e) => e.Handled = !char.IsControl(e.KeyChar) && !char.IsDigit(e.KeyChar);

This will only permit the use of digits and control characters in the `TextBox`, other combinations are possible using the same approach of setting the `Handle` property to true to block the text.

The user can still copy/paste unwanted characters so an additional check should be on the `TextChanged` to cleanse the input:

    textBox.TextChanged += (sender, e) => textBox.Text = Regex.Match(textBox.Text, @"\d+").Value

In this example a **Regular expression** is used to filter the text.

**NumericUpDown** should be preferred for numbers when possible.

## Adding a Placeholder to textbox
This code places the _hint_ text at form load and manipulates it as follows:

**C#**

    private void Form_load(object sender, EventArgs e)
    {
        textBox.Text = "Place Holder text...";
    }
 
    private void textBox_Enter(object sender, EventArgs e)
    {
        if(textBox.Text == "Place Holder text...")
        {
            textBox.Text = "";
        }
    }
 
    private void textBox_Leave(object sender, EventArgs e)
    {
        if(textBox.Text.Trim() == "")
        {
            textBox.Text = "Place Holder text...";
        }
    }

**VB.NET**

    Private Sub Form_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        textBox.Text = "Place Holder text..."
    End Sub

    Private Sub textBox_GotFocus(sender as Object,e as EventArgs) Handles textBox.GotFocus
        if Trim(textBox.Text) = "Place Holder text..." Then
            textBox.Text = ""
        End If
    End Sub

    Private Sub textBox_LostFocus(sender as Object,e as EventArgs) Handles textBox.LostFocus
        if Trim(textBox.Text) = "" Then
            textBox.Text = "Place Holder text..."
        End If
    End Sub



## How to scroll to the end
    textBox.SelectionStart = textBox.TextLength;
    textBox.ScrollToCaret();

Applying the same principle, `SelectionStart` can be set to `0` to scroll to the top or to a specific number to go to a specific character.

