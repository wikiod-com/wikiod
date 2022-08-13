---
title: "Data Binding"
slug: "data-binding"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## SQL Data Source
Controls that can be bound with data can make use of `SqlDataSource` controls. The `SqlDataSource` control not only allows you to retrieve data from a database, but also edit and sort the data.

Retrieving Data
---
Stored Procedure:

    <asp:SqlDataSource ID="SqlDataSourceEmployees"
        runat="server"
        ConnectionString="<%$ ConnectionStrings:MyConnectionString %>"
        SelectCommand="sp_GetEmployees"
        SelectCommandType="StoredProcedure">
    </asp:SqlDataSource>

SQL Query:

    <asp:SqlDataSource ID="SqlDataSourceEmployees"
        runat="server"
        ConnectionString="<%$ ConnectionStrings:MyConnectionString %>"
        SelectCommand="SELECT
                           EmployeeID, 
                           EmployeeFirstName,
                           EmployeeLastName
                       FROM
                           dbo.Employees">
    </asp:SqlDataSource>

Parameters:

    <asp:SqlDataSource ID="SqlDataSourceEmployees"
        runat="server"
        ConnectionString="<%$ ConnectionStrings:MyConnectionString %>"
        SelectCommand="SELECT
                           EmployeeID, 
                           EmployeeFirstName,
                           EmployeeLastName
                       FROM
                           dbo.Employees
                       WHERE
                           DepartmentID = @DepartmentID;">
        <SelectParameters>
            <asp:ControlParameter ControlID="ddlDepartment"
                Name="DepartmentID"
                PropertyName="SelectedValue" />
        </SelectParameters>
    </asp:SqlDataSource>

Be aware of the `CancelSelectOnNullParameter` option, that if set to true (default) will stop the data binding if any parameter is NULL

Basic Usage
---
GridView:

    <asp:GridView ID="GridViewEmployees"
        runat="server"
        AutoGenerateColumns="false"
        DataSourceID="SqlDataSourceEmployees">
        <Columns>
            <asp:BoundField DataField="EmployeeID" HeaderText="Employee ID" />
            <asp:BoundField DataField="EmployeeFirstName" HeaderText="First Name" />
            <asp:BoundField DataField="EmployeeLastName" HeaderText="Last Name" />
        </Columns>
    </asp:GridView>

## Object Data Source
    <asp:ObjectDataSource ID="ObjectDataSourceEmployees" runat="server"
        TypeName="MyPackage.MyDataAccessClass"                       
        DataObjectTypeName="MyPackage.Employee" 
        SelectMethod="GetEmployees"
        UpdateMethod="SaveEmployee"
        InsertMethod="SaveEmployee">
    </asp:ObjectDataSource>

In the code behind

The Data Access Class

    public class MyDataAccess
    {
        public static List<Employee> GetEmployees()
        {
            List<Employee> results = new List<Employee>()
            {
                new Employee(){ Id=1, Name="John Smith" }, 
                new Employee(){ Id=2, Name="Mary Jane" } 
            };
        
            return results;
        }  

        public static void SaveEmployee(Employee e)
        {
            // Persist Employee e to the DB/cache etc. here
        }        
    }


The Employee Class

    public class Employee
    {
        public Int32EmployeeId { get; set; }
        public string Name { get; set; }
    }

