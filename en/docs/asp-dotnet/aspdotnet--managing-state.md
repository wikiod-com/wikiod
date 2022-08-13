---
title: "ASP.NET - Managing State"
slug: "aspnet---managing-state"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## View State
The following example demonstrates the concept of storing view state. Let us keep a counter, which is incremented each time the page is posted back by clicking a button on the page. A label control shows the value in the counter.

The markup file code is as follows:

    <%@ Page Language="C#" AutoEventWireup="true" CodeBehind="Default.aspx.cs" Inherits="statedemo._Default" %>
    
    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    
    <html xmlns="http://www.w3.org/1999/xhtml" >
    
       <head runat="server">
          <title>
             Untitled Page
          </title>
       </head>
       
       <body>
          <form id="form1" runat="server">
          
             <div>
                <h3>View State demo</h3>
             
                Page Counter:
                
                <asp:Label ID="lblCounter" runat="server" />
                <asp:Button ID="btnIncrement" runat="server" Text="Add Count" onclick="btnIncrement_Click" />
             </div>
             
          </form>
       </body>
       
    </html>

The code behind file for the example is shown here:

    public partial class _Default : System.Web.UI.Page
    {
       public int counter
       {
          get
          {
             if (ViewState["pcounter"] != null)
             {
                return ((int)ViewState["pcounter"]);
             }
             else
             {
                return 0;
             }
          }
          
          set
          {
             ViewState["pcounter"] = value;
          }
       }
            
       protected void Page_Load(object sender, EventArgs e)
       {
          lblCounter.Text = counter.ToString();
          counter++;
       }
    }

It would produce the following result:

View State Demo

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/xX1Sm.jpg

