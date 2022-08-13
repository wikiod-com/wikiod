---
title: "httpHandlers"
slug: "httphandlers"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Using an httpHandler (.ashx) to download a file from a specific location
Create a new httpHandler inside your ASP.NET project. Apply the following code (VB) to the handler file:

    Public Class AttachmentDownload
        Implements System.Web.IHttpHandler
    
        Sub ProcessRequest(ByVal context As HttpContext) Implements IHttpHandler.ProcessRequest
    
            ' pass an ID through the query string to append a unique identifer to your downloadable fileName
    
            Dim fileUniqueId As Integer = CInt(context.Request.QueryString("id"))
    
            ' file path could also be something like "C:\FolderName\FilesForUserToDownload
    
            Dim filePath As String = "\\ServerName\FolderName\FilesForUserToDownload"
            Dim fileName As String = "UserWillDownloadThisFile_" & fileUniqueId
            Dim fullFilePath = filePath & "\" & fileName
            Dim byteArray() As Byte = File.ReadAllBytes(fullFilePath)
    
            ' promt the user to download the file
    
            context.Response.Clear()
            context.Response.ContentType = "application/x-please-download-me" ' "application/x-unknown"
            context.Response.AppendHeader("Content-Disposition", "attachment; filename=" & fileName)
            context.Response.BinaryWrite(byteArray)
            context.Response.Flush()
            context.Response.Close()
            byteArray = Nothing
    
        End Sub
    
        ReadOnly Property IsReusable() As Boolean Implements IHttpHandler.IsReusable
            Get
                Return False
            End Get
        End Property
    
    End Class

You can call the handler from code behind, or from a client side language. In this example I am using a javascript which will call the handler.

    function openAttachmentDownloadHandler(fileId) {
    
        // the location of your handler, and query strings to be passed to it

        var url = "..\\_Handlers\\AttachmentDownload.ashx?";
        url = url + "id=" + fileId;
    
        // opening the handler will run its code, and it will close automatically
        // when it is finished. 

        window.open(url);
    
    } 

Now attach that assign the javascript function to a button click event on a clickable element in your web form. For example:

    <asp:LinkButton ID="lbtnDownloadFile" runat="server" OnClientClick="openAttachmentDownloadHandler(20);">Download A File</asp:LinkButton>

Or you can call the javascript function from the code behind as well:

    ScriptManager.RegisterStartupScript(Page,
                    Page.GetType(),
                    "openAttachmentDownloadHandler",
                    "openAttachmentDownloadHandler(" & fileId & ");",
                    True)

Now when you click your button the httpHandler will get your file to the browser and ask the user if they would like to download it.

