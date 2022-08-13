---
title: "Expressions"
slug: "expressions"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Value From App.Config
    <asp:Literal runat="server" text="<%$ AppSettings:MyAppSettingName %>"/>

## Evaluated Expression
    <div>
      The time is now <%= DateTime.Now.ToString() %>
    </div>

## Code Block Within ASP Markup
    <div>
        <form id="form1" runat="server">
            <% 
                for (int i = 1; i <= 10; j++)
                {
                    Response.Write(i) + " ";
                }
            %>
        </form>
    <div>

