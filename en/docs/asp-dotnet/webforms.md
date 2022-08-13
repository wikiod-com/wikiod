---
title: "WebForms"
slug: "webforms"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Syntax
- <asp:TextBox runat="server" ID="" TextMode="" Text="" />
- <asp:Repeater runat="server" ID="" OnItemDataBound="">  
`<HeaderTemplate></HeaderTemplate>`  
`<ItemTemplate></ItemTemplate>`  
`<FooterTemplate></FooterTemplate>`   
`</asp:Repeater>`


All ASP.Net WebForm controls require `runat="server"` in order to communicate with the CodeBehind.

## Using a Repeater to create a  HTML Table


## Grouping in ListView


## Example
<html>
<head>

    <script language="VB" runat="server">

        Sub SubmitBtn_Click(sender As Object, e As EventArgs)
            Label1.Text = "Text1.Text = " & Text1.Text
        End Sub

    </script>

</head>
<body>

    <h3><font face="Verdana">TextBox Sample</font></h3>

    <form runat="server">

      <asp:TextBox id="Text1" Text="Copy this text to the label" Width="200px" runat="server"/>

      <asp:Button OnClick="SubmitBtn_Click" Text="Copy Text to Label" Runat="server"/>

      <p>
      
      <asp:Label id="Label1" Text="Label1" runat="server"/>

    </form>

</body>
</html>

## Hyperlink
The HyperLink control is used to navigate from the client to another page.

    <html>
    
    <script language="VB" runat="server">
    
    
       Sub Page_Load(sender As Object, e As EventArgs) 
          ' Set hyperlink to "~", which indicates application root.
          HyperLink1.NavigateUrl = "~"
       End Sub
    
    </script>
    
    <body>
    
        <h3><font face="Verdana">Simple asp:hyperlink Sample</font></h3>
    
        <form runat=server>
    
            <p>
    
            <asp:hyperlink id=HyperLink1 runat="server">
                Go To QuickStart
            </asp:hyperlink>
    
        </form>
    
    </body>
    
    </html>



