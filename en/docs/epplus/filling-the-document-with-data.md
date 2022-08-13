---
title: "Filling the document with data"
slug: "filling-the-document-with-data"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

How to fill your created Excel sheet with data from different sources.

## Fill with a DataTable
    //create a new ExcelPackage
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create a datatable
        DataTable dataTable = new DataTable();
    
        //add three colums to the datatable
        dataTable.Columns.Add("ID", typeof(int));
        dataTable.Columns.Add("Type", typeof(string));
        dataTable.Columns.Add("Name", typeof(string));
    
        //add some rows
        dataTable.Rows.Add(0, "Country", "Netherlands");
        dataTable.Rows.Add(1, "Country", "Japan");
        dataTable.Rows.Add(2, "Country", "America");
        dataTable.Rows.Add(3, "State", "Gelderland");
        dataTable.Rows.Add(4, "State", "Texas");
        dataTable.Rows.Add(5, "State", "Echizen");
        dataTable.Rows.Add(6, "City", "Amsterdam");
        dataTable.Rows.Add(7, "City", "Tokyo");
        dataTable.Rows.Add(8, "City", "New York");
    
        //create a WorkSheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //add all the content from the DataTable, starting at cell A1
        worksheet.Cells["A1"].LoadFromDataTable(dataTable, true);
    }

## Fill with a DataTable from an SQL query or Stored Procedure
    //create a new ExcelPackage
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //the query or stored procedure name for the database
        string sqlQuery = "SELECT * FROM myTable";
    
        //create a datatable
        DataTable dataTable = loadExternalDataSet(sqlQuery);
    
        //create a WorkSheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //add all the content from the DataTable, starting at cell A1
        worksheet.Cells["A1"].LoadFromDataTable(dataTable, true);
    }
    
    //method for retrieving data from the database and return it as a datatable
    public static DataTable loadExternalDataSet(string sqlQuery)
    {
        DataTable dt = new DataTable();
    
        using (SqlConnection connection = new SqlConnection(ConfigurationManager.ConnectionStrings["myConnStr"].ConnectionString))
        using (SqlDataAdapter adapter = new SqlDataAdapter(sqlQuery, connection))
        {
            try
            {
                adapter.Fill(dt);
            }
            catch
            {
            }
        }
    
        return dt;
    }

## Manually fill cells
Fill some cells with text.

    worksheet.Cells["A1"].Value = "Lorem ipsum";
    worksheet.Cells["B2"].Value = "dolor sit amet";
    worksheet.Cells["C3"].Value = "consectetur adipiscing";
    worksheet.Cells["D4"].Value = "elit sed do eiusmod";

    worksheet.Cells["E5"].Value = 12345;
    worksheet.Cells["F6"].Value = DateTime.Now;

Fill cell data with a loop, note that row and column indexes start at 1

    for (int i = 1; i <= 30; i++)
    {
        for (int j = 1; j <= 15; j++)
        {
            worksheet.Cells[i, j].Value = "Row " + i + ", Column " + j;
        }
    }

## Fill from collection
    //create a new ExcelPackage
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create a WorkSheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //create a new list with books
        List<Book> books = new List<Book>();
        
        //add some books to the list
        for (int i = 0; i < 10; i++)
        {
            Book b = new Book();

            b.id = i;
            b.name = "Name " + i;
            b.category = "Category " + i;
            b.date = DateTime.Now.AddDays(i).AddHours(i);
   
            books.Add(b);
        }    
    
        //add all the content from the List<Book> collection, starting at cell A1
        worksheet.Cells["A1"].LoadFromCollection(books);
    }

