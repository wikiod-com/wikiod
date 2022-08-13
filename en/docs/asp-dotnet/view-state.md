---
title: "View State"
slug: "view-state"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

View State is the method to preserve the Value of the Page and Controls between round trips. It is a Page-Level State Management technique. View State is turned on by default and normally serializes the data in every control on the page regardless of whether it is actually used  during a post-back. 

## Syntax


    

 - ViewState["NameofViewstate"] = "Value";

 



## Example

**ASPX**
 

    <%@ Page Language="C#" AutoEventWireup="true"  CodeFile="Default.aspx.cs" Inherits="_Default" %>
        <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
        <html xmlns="http://www.w3.org/1999/xhtml" >
        <head runat="server">
            <title>ViewState</title>
        </head>
        <body>
            <form id="form1" runat="server">
                <asp:TextBox runat="server" id="NameField" />
                <asp:Button runat="server" id="SubmitForm" onclick="SubmitForm_Click" text="Submit & set name" />
                <asp:Button runat="server" id="RefreshPage" text="Just submit" />
                <br /><br />
                Name retrieved from ViewState: <asp:Label runat="server" id="NameLabel" />
            </form> 
        </body>
        </html>

**Code behind**

    using System;
    using System.Data;
    using System.Web;
    
    public partial class _Default : System.Web.UI.Page 
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            if(ViewState["NameOfUser"] != null)
                NameLabel.Text = ViewState["NameOfUser"].ToString();
            else
                NameLabel.Text = "Not set yet...";
        }
    
        protected void SubmitForm_Click(object sender, EventArgs e)
        {
            ViewState["NameOfUser"] = NameField.Text;
            NameLabel.Text = NameField.Text;
        }
    }


