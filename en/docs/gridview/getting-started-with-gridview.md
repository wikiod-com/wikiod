---
title: "Getting started with gridview"
slug: "getting-started-with-gridview"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Binding a GridView with a DataSource e.g. DataTable
Step 1: Make a design of GridView for displaying your data (**HTML Code**):

    <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="false">
        <Columns>
            <asp:TemplateField HeaderText="ID">
                <ItemTemplate>
                    <asp:Label ID="lblId" runat="server" Text='<% #Bind("id") %>'></asp:Label>
                </ItemTemplate>
            </asp:TemplateField>
            <asp:TemplateField HeaderText="NAME">
                <ItemTemplate>
                    <asp:Label ID="lblName" runat="server" Text='<% #Bind("name") %>'></asp:Label>
                </ItemTemplate>
            </asp:TemplateField>
            <asp:TemplateField HeaderText="COUNTRY">
                <ItemTemplate>
                    <asp:Label ID="lblCountry" runat="server" Text='<% #Bind("country") %>'></asp:Label>
                </ItemTemplate>
            </asp:TemplateField>
        </Columns>
    </asp:GridView>

Step 2: Bind your GridView with DataTable (**.CS Code**):

    protected void Page_Load(object sender, EventArgs e)
        {
            if (!IsPostBack)
            {
                // Create a datatable as a DataSource of your GridView
                DataTable dt = new DataTable();

                // Add three columns in datatable and their names and data types
                dt.Columns.Add(new DataColumn("id", typeof(int)));
                dt.Columns.Add(new DataColumn("name", typeof(string)));
                dt.Columns.Add(new DataColumn("country", typeof(string)));

                // Add five records in datatable
                for (int i = 0; i < 5; i++)
                {
                    dt.Rows.Add(i, "Name" + i, "Country" + i);
                }

                GridView1.DataSource = dt; // set your datatable to your gridview as datasource
                GridView1.DataBind(); // bind the gridview with datasource
            }
    }

*After Binding your GridView looks like this:*

[![enter image description here][1]][1]

**Note:** You can also bind your GridView from database.

  [1]: https://i.stack.imgur.com/QOaf9.png

## Installation or Setup
`GridView` is an ASP.NET server control and as such simply requires any version of .Net installed on your computer along with a .Net development environment, typically any version of Visual Studio.

Assuming you have a .Net development environment, create any Web Forms Application or MVC Application project.

`GridView` controls can be added via drag-and-drop from the designer toolbox or manually in the html markup on the Web Form/MVC View.

An empty GridView Control:

    <asp:GridView ID="GridView1" runat="server">
    </asp:GridView>


