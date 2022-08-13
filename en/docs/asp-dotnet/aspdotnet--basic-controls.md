---
title: "ASP.NET - Basic Controls"
slug: "aspnet---basic-controls"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Syntax
 - <asp:Button ID="Button1" runat="server" onclick="Button1_Click"
   Text="Click" / > <asp:TextBox ID="txtstate" runat="server">
 - </asp:TextBox> <asp:CheckBox ID= "chkoption" runat= "Server">  </asp:CheckBox> <asp:RadioButton ID= "rdboption" runat= "Server"> 
   </asp: RadioButton> 
 - <asp:ListBox ID="ListBox1" runat="server"
   AutoPostBack="True"   
   OnSelectedIndexChanged="ListBox1_SelectedIndexChanged">
   </asp:ListBox> 
 - <asp:DropDownList ID="DropDownList1" runat="server"
   AutoPostBack="True"  
   OnSelectedIndexChanged="DropDownList1_SelectedIndexChanged">
   </asp:DropDownList>
 - <asp:RadioButtonList ID="RadioButtonList1"
   runat="server" AutoPostBack="True"    
   OnSelectedIndexChanged="RadioButtonList1_SelectedIndexChanged">
   </asp:RadioButtonList> 
 - <asp:CheckBoxList ID="CheckBoxList1"
   runat="server" AutoPostBack="True"    
   OnSelectedIndexChanged="CheckBoxList1_SelectedIndexChanged">
   </asp:CheckBoxList> 
 - <asp:BulletedList ID="BulletedList1"
   runat="server"> </asp:BulletedList> 
 - <asp:HyperLink ID="HyperLink1"
   runat="server">    HyperLink </asp:HyperLink> <asp:Image ID="Image1"
   runat="server">

## Text Boxes and Labels
Text box controls are typically used to accept input from the user. A text box control can accept one or more lines of text depending upon the settings of the TextMode attribute.

Label controls provide an easy way to display text which can be changed from one execution of a page to the next. If you want to display text that does not change, you use the literal text.

Basic syntax of text control:

    <asp:TextBox ID="txtstate" runat="server" ></asp:TextBox>

Common Properties of the Text Box and Labels:

| Properties     | Description |
| -------------- | ----------- | 
| TextMode    | Specifies the type of text box. SingleLine creates a standard text box, MultiLIne creates a text box that accepts more than one line of text and the Password causes the characters that are entered to be masked. The default is SingleLine. |       
| Text    | The text content of the text box.|
| MaxLength    | The maximum number of characters that can be entered into the text box.|      
| Wrap    | It determines whether or not text wraps automatically for multi-line text box; default is true.|      
| ReadOnly    | Determines whether the user can change the text in the box; default is false, i.e., the user can change the text.|      
| Columns    | The width of the text box in characters. The actual width is determined based on the font that is used for the text entry.|      
| Rows    | The height of a multi-line text box in lines. The default value is 0, means a single line text box.|
The mostly used attribute for a label control is 'Text', which implies the text displayed on the label.

## Check Boxes and Radio Buttons
A check box displays a single option that the user can either check or uncheck and radio buttons present a group of options from which the user can select just one option.

To create a group of radio buttons, you specify the same name for the GroupName attribute of each radio button in the group. If more than one group is required in a single form, then specify a different group name for each group.

If you want check box or radio button to be selected when the form is initially displayed, set its Checked attribute to true. If the Checked attribute is set to true for multiple radio buttons in a group, then only the last one is considered as true.

Basic syntax of check box:

    <asp:CheckBox ID= "chkoption" runat= "Server"> </asp:CheckBox>

Basic syntax of radio button:

    <asp:RadioButton ID= "rdboption" runat= "Server"> </asp: RadioButton>

Common properties of check boxes and radio buttons:

| Properties     | Description |
| -------------- | ----------- |   
| Text    | The text displayed next to the check box or radio button.|      
| Checked    | Specifies whether it is selected or not, default is false.|      
| GroupName    | Name of the group the control belongs to.|

## List Controls
ASP.NET provides the following controls

 - Drop-down list 
 - List box 
 - Radio button list 
 - Check box list 
 - Bulleted  list

These control let a user choose from one or more items from the list. List boxes and drop-down lists contain one or more list items. These lists can be loaded either by code or by the ListItemCollection editor.

Basic syntax of list box control:

    <asp:ListBox ID="ListBox1" runat="server" AutoPostBack="True"    OnSelectedIndexChanged="ListBox1_SelectedIndexChanged">
    </asp:ListBox>

 Basic syntax of drop-down list control:

    <asp:DropDownList ID="DropDownList1" runat="server" AutoPostBack="True"   OnSelectedIndexChanged="DropDownList1_SelectedIndexChanged">
    </asp:DropDownList>
   Common properties of list box and drop-down Lists:

| Properties     | Description |
| -------------- | ----------- |   
| Items    | The collection of ListItem objects that represents the items in the control. This property returns an object of type ListItemCollection.|      
| Rows    | Specifies the number of items displayed in the box. If actual list contains more rows than displayed then a scroll bar is added.|      
| SelectedIndex    | The index of the currently selected item. If more than one item is selected, then the index of the first selected item. If no item is selected, the value of this property is -1.|
| SelectedValue    | The value of the currently selected item. If more than one item is selected, then the value of the first selected item. If no item is selected, the value of this property is an empty string ("").|      
| SelectionMode    | Indicates whether a list box allows single selections or multiple selections.|  
Common properties of each list item objects:

| Properties     | Description |
| -------------- | ----------- |   
| Text    | The text displayed for the item.|      
| Selected    | A string value associated with the item.|      
| Value    | Indicates whether the item is selected.| 

It is important to notes that:

 - To work with the items in a drop-down list or list box, you use the
   Items property of the control. This property returns a
   ListItemCollection object which contains all the items of the list.
 - The SelectedIndexChanged event is raised when the user selects a
   different item from a drop-down list or list box.

## Radio Button list and Check Box list
A radio button list presents a list of mutually exclusive options. A check box list presents a list of independent options. These controls contain a collection of ListItem objects that could be referred to through the Items property of the control.

Basic syntax of radio button list:

    <asp:RadioButtonList ID="RadioButtonList1" runat="server" AutoPostBack="True" 
       OnSelectedIndexChanged="RadioButtonList1_SelectedIndexChanged">
    </asp:RadioButtonList>

Basic syntax of check box list:

    <asp:CheckBoxList ID="CheckBoxList1" runat="server" AutoPostBack="True" 
       OnSelectedIndexChanged="CheckBoxList1_SelectedIndexChanged">
    </asp:CheckBoxList>

Common properties of check box and radio button lists:

| Properties     | Description |
| -------------- | ----------- |   
| RepeatLayout    | This attribute specifies whether the table tags or the normal html flow to use while formatting the list when it is rendered. The default is Table.|      
| RepeatDirection    | It specifies the direction in which the controls to be repeated. The values available are Horizontal and Vertical. Default is Vertical.|      
| RepeatColumns    | It specifies the number of columns to use when repeating the controls; default is 0.|    

## Bulleted lists and Numbered lists
The bulleted list control creates bulleted lists or numbered lists. These controls contain a collection of ListItem objects that could be referred to through the Items property of the control.

Basic syntax of a bulleted list:

    <asp:BulletedList ID="BulletedList1" runat="server">
    </asp:BulletedList>
Common properties of the bulleted list:

| Properties     | Description |
| -------------- | ----------- |   
| BulletStyle    | This property specifies the style and looks of the bullets, or numbers.|      
| RepeatDirection    | It specifies the direction in which the controls to be repeated. The values available are Horizontal and Vertical. Default is Vertical.|      
| RepeatColumns    | It specifies the number of columns to use when repeating the controls; default is 0.|    

## HyperLink Control
The HyperLink control is like the HTML <a> element.

Basic syntax for a hyperlink control:

    <asp:HyperLink ID="HyperLink1" runat="server">
       HyperLink
    </asp:HyperLink>
  
It has the following important properties:

| Properties     | Description |
| -------------- | ----------- |   
| ImageUrl    | Path of the image to be displayed by the control.|      
| NavigateUrl    | Target link URL.|      
| Text    | The text to be displayed as the link.|    
| Target    | The window or frame which loads the linked page.| 

## Image Control
The image control is used for displaying images on the web page, or some alternative text, if the image is not available.

Basic syntax for an image control:

    <asp:Image ID="Image1" runat="server">

It has the following important properties:

| Properties     | Description |
| -------------- | ----------- |   
| AlternateText    | Alternate text to be displayed in absence of the image.|      
| ImageAlign    | Alignment options for the control.|      
| ImageUrl    | Path of the image to be displayed by the control.|  

