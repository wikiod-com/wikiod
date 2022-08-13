---
title: "Repeater"
slug: "repeater"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Basic usage
This example creates a simple 1-column repeater that displays a list of numbers, one per repeater item.

Markup:

    <asp:Repeater ID="Repeater1" runat="server">
        <ItemTemplate>
            <%# Container.DataItem.ToString() %>
        </ItemTemplate>
    </Repeater>

Code behind:

    protected void Page_Load(object sender, EventArgs e)
    {
        List<int> numbers = new List<int>{1, 2, 3, 4, 5};
        Repeater1.DataSource = numbers;
        Repeater1.DataBind();
    }

