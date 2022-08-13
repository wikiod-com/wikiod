---
title: "Nested-GridView"
slug: "nested-gridview"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

A Nested-GridView is a GridView control inside the grid row of the parent GridView control.

## Syntax
- <asp:GridView ID="gvParent" runat="server"></asp:GridView>
- <asp:GridView ID="gvChild" runat="server"></asp:GridView>


## Parameters
| gvParent | It is a parent gridview control which contains a child gridview like `gvChild` control and other controls in gridview row like Labels, Buttons etc. |
| -------- | ------------------------------------------------------------------------- |
| gvChild | It is a child gridview control that can also contain some other controls like in parent gridview. It will be nested in `gvParent` gridview control. |

Parent gridview can be bind using a simple `DataBind` function like `gvParent.DataBind();`. And for binding Child gridview, you have to bind it in `OnRowDataBound` event of parent gridview.

## Binding Nested-GridView with DataSource e.g. DataTable
1. Design of Nested-GridView (**HTML Code**):


    <asp:GridView ID="gvParent" runat="server" AutoGenerateColumns="false" OnRowDataBound="gvParent_RowDataBound">
        <Columns>
            <asp:TemplateField HeaderText="Parent Column">
                <ItemTemplate>
                    <asp:Label ID="lblParent" runat="server" Text='<% #Bind("parent") %>'></asp:Label>
                    <asp:GridView ID="gvChild" AutoGenerateColumns="false" runat="server">
                        <Columns>
                            <asp:TemplateField HeaderText="Child Column">
                                <ItemTemplate>
                                    <asp:Label ID="lblChild" runat="server" Text='<% #Bind("child") %>'></asp:Label>
                                </ItemTemplate>
                            </asp:TemplateField>
                        </Columns>
                    </asp:GridView>
                </ItemTemplate>
            </asp:TemplateField>
        </Columns>
    </asp:GridView>

2. Binding parent GridView in `Page_Load` event:


    DataTable cgv = new DataTable(); // define temporary a datatable for accessing in rowdatabound event
    protected void Page_Load(object sender, EventArgs e)
    {
        if (!IsPostBack)
        {
            // Create a datatable as a DataSource of your GridViews
            DataTable dtParent = new DataTable(); // parent gridview datasource
            DataTable dtChild = new DataTable(); // child gridview datasource

            // Add column(s) in datatables and their names and data types
            dtParent.Columns.Add(new DataColumn("parent", typeof(string))); // parent column
            dtChild.Columns.Add(new DataColumn("child", typeof(string))); // child column

            // Add two records in parent datatable
            for (int i = 0; i < 2; i++)
                dtParent.Rows.Add("Parent" + i);

            // Add three records in child datatable
            for (int i = 0; i < 3; i++)
                dtChild.Rows.Add("Child" + i);

            cgv = dtChild; // set child datatable to temprary datatable

            gvParent.DataSource = dtParent; // set your parent datatable to parent gridview as datasource
            gvParent.DataBind(); // bind the gridview with datasource
        }
    }
3. Binding Child GridView in `OrRowDataBound` event of parent GridView.


    protected void gvParent_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            // find all child gridviews from parent
            GridView gvChild = ((GridView)e.Row.FindControl("gvChild"));

            gvChild.DataSource = cgv; // set your child datatable to parent gridview as datasource
            gvChild.DataBind(); // bind the gridview with datasource
        }
    }

*After binding Nested-GridView looks like:*

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/IMnlL.png

