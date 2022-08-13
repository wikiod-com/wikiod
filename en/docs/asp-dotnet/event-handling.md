---
title: "Event Handling"
slug: "event-handling"
draft: false
images: []
weight: 9922
type: docs
toc: true
---

## Syntax
-    private void EventName (object sender, EventArgs e);

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| object sender | sender refers to the object that invoked the event that fired the event handler. This is useful if you have many objects using the same event handler. |  
| EventArgs e | EventArgs is something of a dummy base class. In and of itself it's more or less useless, but if you derive from it, you can add whatever data you need to pass to your event handlers.|  

## Default Events
The default event for the Page object is Load event. Similarly, every control has a default event. For example, default event for the button control is the Click event.

The default event handler could be created in Visual Studio, just by double clicking the control in design view. The following table shows some of the default events for common controls: 

| Control    | Default Event|
| ------ | ------ |
| AdRotator | AdCreated |
| BulletedList | Click |
| Button | Click |
| Calender | SelectionChanged |
| CheckBox | CheckedChanged |
| CheckBoxList | SelectedIndexChanged |
| DataGrid | SelectedIndexChanged |
| DataList | SelectedIndexChanged |
| DropDownList | SelectedIndexChanged |
| HyperLink | Click |
| ImageButton | Click |
| ImageMap | Click |
| LinkButton | Click |
| ListBox | SelectedIndexChanged |
| Menu    MenuItem | Click |
| RadioButton | CheckedChanged |
| RadioButtonList | SelectedIndexChanged |

**Example**
This example includes a simple page with a label control and a button control on it. As the page events such as Page_Load, Page_Init, Page_PreRender etc. take place, it sends a message, which is displayed by the label control. When the button is clicked, the Button_Click event is raised and that also sends a message to be displayed on the label.

Create a new website and drag a label control and a button control on it from the control tool box. Using the properties window, set the IDs of the controls as .lblmessage. and .btnclick. respectively. Set the Text property of the Button control as 'Click'.

The markup file (.aspx):

    <%@ Page Language="C#" AutoEventWireup="true" CodeBehind="Default.aspx.cs" 
       Inherits="eventdemo._Default" %>
    
    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" 
       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    
    <html xmlns="http://www.w3.org/1999/xhtml" >
    
       <head runat="server">
          <title>Untitled Page</title>
       </head>
       
       <body>
          <form id="form1" runat="server">
             <div>
                <asp:Label ID="lblmessage" runat="server" >
                
                </asp:Label>
                
                <br />
                <br />
                <br />
                
                <asp:Button ID="btnclick" runat="server" Text="Click" onclick="btnclick_Click" />
             </div>
          </form>
       </body>
       
    </html>
Double click on the design view to move to the code behind file. The Page_Load event is automatically created without any code in it. Write down the following self-explanatory code lines:

    using System;
    using System.Collections;
    using System.Configuration;
    using System.Data;
    using System.Linq;
    
    using System.Web;
    using System.Web.Security;
    using System.Web.UI;
    using System.Web.UI.HtmlControls;
    using System.Web.UI.WebControls;
    using System.Web.UI.WebControls.WebParts;
    
    using System.Xml.Linq;
    
    namespace eventdemo {
    
       public partial class _Default : System.Web.UI.Page {
       
          protected void Page_Load(object sender, EventArgs e) {
             lblmessage.Text += "Page load event handled. <br />";
             
             if (Page.IsPostBack) {
                lblmessage.Text += "Page post back event handled.<br/>";
             }
          }
          
          protected void Page_Init(object sender, EventArgs e) {
             lblmessage.Text += "Page initialization event handled.<br/>";
          }
          
          protected void Page_PreRender(object sender, EventArgs e) {
             lblmessage.Text += "Page prerender event handled. <br/>";
          }
          
          protected void btnclick_Click(object sender, EventArgs e) {
             lblmessage.Text += "Button click event handled. <br/>";
          }
           }
        }
Execute the page. The label shows page load, page initialization and, the page pre-render events. Click the button to see effect:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/tXCjC.jpg

## Application and Session Events
The most important application events are:

**Application_Start** - It is raised when the application/website is started.

**Application_End** - It is raised when the application/website is stopped.

Similarly, the most used Session events are:

**Session_Start** - It is raised when a user first requests a page from the application.

**Session_End** - It is raised when the session ends.

## Page and Control Events
Common page and control events are:

**DataBinding** - It is raised when a control binds to a data source.

**Disposed** - It is raised when the page or the control is released.

**Error** - It is a page event, occurs when an unhandled exception is thrown.

**Init** - It is raised when the page or the control is initialized.

**Load** - It is raised when the page or a control is loaded.

**PreRender** - It is raised when the page or the control is to be rendered.

**Unload** - It is raised when the page or control is unloaded from memory.

