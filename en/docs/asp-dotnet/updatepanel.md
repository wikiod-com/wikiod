---
title: "UpdatePanel"
slug: "updatepanel"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

This topic describes how to add partial-page update support to a Web page by using two Microsoft Ajax server controls: the ScriptManager control and the UpdatePanel control. These controls remove the requirement to refresh the whole page with each postback, which improves the user experience.

## Syntax
 - <asp:UpdatePanel ID="UpdatePanel1" runat="server">  
   </asp:UpdatePanel>

A ScriptManager must be added to page to make the UpdatePanel to work.

## Update Panel Example
Step 1: Add ScriptManager to your page

    <asp:ScriptManager ID="ScriptManager1" runat="server">
                </asp:ScriptManager>

Step 2: Add UpdatePanel to your page just after ScriptManager.

    <asp:UpdatePanel ID="UpdatePanel1" runat="server">
                <ContentTemplate></ContentTemplate>
            </asp:UpdatePanel>

Step 3: After adding content to  your UpdatePanels Content Template your aspx page should look something like this:

    <%@ Page Language="C#" %>
    
    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">    
    
    <html xmlns="http://www.w3.org/1999/xhtml" >
    <head runat="server">
        <title>Untitled Page</title>
        <style type="text/css">
        #UpdatePanel1 { 
          width:300px; height:100px;
         }
        </style>
    </head>
    <body>
        <form id="form1" runat="server">
        <div style="padding-top: 10px">
            <asp:ScriptManager ID="ScriptManager1" runat="server">
            </asp:ScriptManager>
            <asp:UpdatePanel ID="UpdatePanel1" runat="server">
                <ContentTemplate>
                    <fieldset>
                    <legend>UpdatePanel</legend>
                    <asp:Label ID="Label1" runat="server" Text="Panel created."></asp:Label><br />
                    <asp:Button ID="Button1" runat="server" OnClick="Button1_Click" Text="Button" />
                    </fieldset>
                </ContentTemplate>
            </asp:UpdatePanel>
            <br />
            </div>
    
        </div>
        </form>
    </body>
    </html>

Step 4: Add this part to your C# page:

    protected void Button1_Click(object sender, EventArgs e)
    {
        Label1.Text = "Refreshed at " +
            DateTime.Now.ToString();
    }
Step 5: Now run your application.

**Expected Result:** 

The panel content changes every time that you click the button, but the whole page is not refreshed. By default, the ChildrenAsTriggers property of an UpdatePanel control is true. When this property is set to true, controls inside the panel participate in partial-page updates when any control in the panel causes a postback.

