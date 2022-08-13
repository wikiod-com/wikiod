---
title: "ScriptManager"
slug: "scriptmanager"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

ScriptManager control registers the script for the Microsoft AJAX Library with the page. This enables client script support features such as partial-page rendering and Web-service calls.

## Syntax
 1. <asp:ScriptManager ID="smPop" runat="server"></asp:ScriptManager>
 2. ScriptManager.RegisterStartupScript(Control, Type, String, String, Boolean);

## Working with ScriptManager
You must use a ScriptManager control on a page to enable the following features of ASP.NET AJAX:

**1.** Client-script functionality of the Microsoft AJAX Library, and any custom script that you want to send to the browser.

    protected void Button1_Click(object sender, EventArgs e)
    {
        Page.ClientScript.RegisterStartupScript(
            this.GetType(),"myscript","alert('hello world!');");
    }

**2.** Partial-page rendering, which enables regions on the page to be independently refreshed without a postback. The ASP.NET AJAX UpdatePanel, UpdateProgress, and Timer controls require a ScriptManager control to support partial-page rendering.

**3.** JavaScript proxy classes for Web services, which enable you to use client script to access Web services by exposing Web services as strongly typed objects.

    [WebMethod]
    public int Add(int a, int b) { return a + b; }

    function CallAdd()
    {
        // method will return immediately
        // processing done asynchronously
        WebService.Add(0,6, OnMethodSucceeded, OnMethodFailed);
    }


**4.** JavaScript classes to access ASP.NET authentication and profile application services.

    Sys.Services.AuthenticationService.login
    Sys.Services.AuthenticationService.logout
    
    <script type="text/javascript">
        function MyMethod(username, password)
        {
            Sys.Services.AuthenticationService.login(username,
                password,false,null,null,null,null,"User Context"); 
        }
    </script>

See more at https://msdn.microsoft.com/en-us/library/system.web.ui.scriptmanager.aspx

