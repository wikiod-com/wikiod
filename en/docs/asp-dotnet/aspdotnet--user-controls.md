---
title: "ASP.NET - User Controls"
slug: "aspnet---user-controls"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

User controls are containers which can be populated with HTML markup & server controls with code-behind in the same way as ASPX page. They're treated as reusable smaller units of a page, so they can't run as stand-alone pages and must not having **html**, **body** or **form** HTML elements in them.

## Introduction of User Controls
User controls are made for reusability across ASP.NET pages, similar to master pages. Instead of sharing base page layout, user controls share group of HTML/ASP.NET built-in server controls or a specific form layout, e.g. comment submission or guest notes. 

A user control can contain both HTML controls and ASP.NET server controls, including client-side scripts.

The user controls usually include `Control` directive on top of its definition:

    <%@ Control Language="C#" AutoEventWireup="True" CodeFile="UserControl.ascx.cs" %>

Like ASPX page, user controls consists of markups which can be associated with a code behind file to perform certain events and tasks, therefore all HTML tags available on ASPX page can be used on user controls except `<html>`, `<body>` and `<form>` tags. 

Here is an example for simple user control markup:

    <%-- UserControl.ascx --%>
    <%@ Control Language="C#" AutoEventWireup="True" CodeFile="UserControl.ascx.cs" %>
    <div>
        <asp:Label ID="Label1" runat="server" />
        <br />
        <asp:Button ID="Button1" runat="server" Text="Click Here" OnClick="Button1_Click" />
    </div>

Code-behind example:

    // UserControl.ascx.cs
    public partial class UserControl : System.Web.UI.UserControl
    {
        protected void Button1_Click(Object sender, EventArgs e)
        {
            Label1.Text = "Hello World!";
        }
    }

Before a user control inserted in ASPX page, `Register` directive should declared on top of the page referencing the user control with its source URL, tag name & tag prefix.

    <%@ Register Src="UserControl.ascx" TagName="UserControl" TagPrefix="uc" %>

Afterwards, you can place user control inside ASPX page like ASP.NET built-in server control:

    <uc:UserControl ID="UserControl1" runat="server" />


## Creating User Control Instance Programmatically
If you want to instantiate an instance of user control inside ASPX code behind page, you need to write user control declaration on `Page_Load` event as follows:

    public partial class Default : System.Web.UI.Page
    {
        protected void Page_Load(Object sender, EventArgs e)
        {
            Control control1 = LoadControl("UserControl.ascx");
            Page.Controls.Add(control1);
        }
    }

Note that the user control ASCX file should be already created when executing LoadControl method.

Another way known to declare user controls programatically is using `PlaceHolder`:

    public partial class Default : System.Web.UI.Page
    {
        public PlaceHolder Placeholder1;
        protected void Page_Load(Object sender, EventArgs e)
        {
            Control control1 = LoadControl("UserControl.ascx");
            Placeholder1.Controls.Add(control1);
        }
    }

Depending on your need, `PlaceHolder` places user controls on a container storing all server controls dynamically added into the page, where `Page.Controls` directly inserts user control inside the page which more preferred for rendering HTML literal controls.

## Adding Custom Properties for User Control
Like standard ASP.NET built-in server controls, user controls can have properties (attributes) on its definition tag. Suppose you want to add color effect on `UserControl.ascx` file like this:

    <uc:UserControl ID="UserControl1" runat="server" Color="blue" />

At this point, custom attributes/properties for user controls can be set by declaring properties inside user control's code behind:

    private String _color;
    public String Color
    {
        get
        {
            return _color;
        }
        set
        {
            _color = value;
        }
    }

Additionally, if you want to set default value on a user control property, assign the default value inside user control's constructor method.

    public UserControl()
    {
        _color = "red";
    }

Then, user control markup should be modified to add color attribute as following example:

    <%@ Control Language="C#" AutoEventWireup="True" CodeFile="UserControl.ascx.cs" %>
    <div>
        <span style="color:<%= Color %>"><asp:Label ID="Label1" runat="server" /></span>
        <br />
        <asp:Button ID="Button1" runat="server" Text="Click Here" OnClick="Button1_Click" />
    </div>

