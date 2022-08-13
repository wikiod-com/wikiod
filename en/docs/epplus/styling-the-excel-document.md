---
title: "Styling the Excel document"
slug: "styling-the-excel-document"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

How to style Cells with font types, background color, border styles etc.

## Background color
    //fill column A with solid red color from hex
    worksheet.Column(1).Style.Fill.PatternType = ExcelFillStyle.Solid;
    worksheet.Column(1).Style.Fill.BackgroundColor.SetColor(ColorTranslator.FromHtml("#FF0000"));

    //fill row 4 with striped orange background
    worksheet.Row(4).Style.Fill.PatternType = ExcelFillStyle.DarkHorizontal;
    worksheet.Row(4).Style.Fill.BackgroundColor.SetColor(Color.Orange);

## Border styles
    //make the borders of cell F6 thick
    worksheet.Cells[6, 6].Style.Border.Top.Style = ExcelBorderStyle.Thick;
    worksheet.Cells[6, 6].Style.Border.Right.Style = ExcelBorderStyle.Thick;
    worksheet.Cells[6, 6].Style.Border.Bottom.Style = ExcelBorderStyle.Thick;
    worksheet.Cells[6, 6].Style.Border.Left.Style = ExcelBorderStyle.Thick;

    //make the borders of cells A18 - J18 double and with a purple color
    worksheet.Cells["A18:J18"].Style.Border.Top.Style = ExcelBorderStyle.Double;
    worksheet.Cells["A18:J18"].Style.Border.Bottom.Style = ExcelBorderStyle.Double;
    worksheet.Cells["A18:J18"].Style.Border.Top.Color.SetColor(Color.Purple);
    worksheet.Cells["A18:J18"].Style.Border.Bottom.Color.SetColor(Color.Purple);

## Font styles
    //set the font type for cells C1 - C30
    worksheet.Cells["C1:C30"].Style.Font.Size = 13;
    worksheet.Cells["C1:C30"].Style.Font.Name = "Calibri";
    worksheet.Cells["C1:C30"].Style.Font.Bold = true;
    worksheet.Cells["C1:C30"].Style.Font.Color.SetColor(Color.Blue);

    //Multiple Fonts in the same cell
    ExcelRange rg = worksheet.Cells["A1"];
    rg.IsRichText = true;
    //ExcelRichText uses "using OfficeOpenXml.Style;"
    ExcelRichText text1 = rg.RichText.Add("Text with Font1");
    text1.Bold = true;
    text1.Italic = true;
    text1.Color = System.Drawing.Color.Blue;
    ExcelRichText text2 = rg.RichText.Add("Text with Font2");
    text2.UnderLine = true;
    text2.Bold = false;
    text2.Color = System.Drawing.Color.Red;
    ExcelRichText text3 = rg.RichText.Add("Text with Font3");
    text3.UnderLine = false;
    text3.Strike = true;
    

## Text alignment and word wrap
    //make column H wider and set the text align to the top and right
    worksheet.Column(8).Width = 25;
    worksheet.Column(8).Style.HorizontalAlignment = ExcelHorizontalAlignment.Right;
    worksheet.Column(8).Style.VerticalAlignment = ExcelVerticalAlignment.Top;

    //wrap text in the cells
    worksheet.Column(8).Style.WrapText = true;

## Complete example with all styles
    //create a new ExcelPackage
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create the WorkSheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //add some dummy data, note that row and column indexes start at 1
        for (int i = 1; i <= 30; i++)
        {
            for (int j = 1; j <= 15; j++)
            {
                worksheet.Cells[i, j].Value = "Row " + i + ", Column " + j;
            }
        }
    
        //fill column A with solid red color
        worksheet.Column(1).Style.Fill.PatternType = ExcelFillStyle.Solid;
        worksheet.Column(1).Style.Fill.BackgroundColor.SetColor(ColorTranslator.FromHtml("#FF0000"));
    
        //set the font type for cells C1 - C30
        worksheet.Cells["C1:C30"].Style.Font.Size = 13;
        worksheet.Cells["C1:C30"].Style.Font.Name = "Calibri";
        worksheet.Cells["C1:C30"].Style.Font.Bold = true;
        worksheet.Cells["C1:C30"].Style.Font.Color.SetColor(Color.Blue);
    
        //fill row 4 with striped orange background
        worksheet.Row(4).Style.Fill.PatternType = ExcelFillStyle.DarkHorizontal;
        worksheet.Row(4).Style.Fill.BackgroundColor.SetColor(Color.Orange);
    
        //make the borders of cell F6 thick
        worksheet.Cells[6, 6].Style.Border.Top.Style = ExcelBorderStyle.Thick;
        worksheet.Cells[6, 6].Style.Border.Right.Style = ExcelBorderStyle.Thick;
        worksheet.Cells[6, 6].Style.Border.Bottom.Style = ExcelBorderStyle.Thick;
        worksheet.Cells[6, 6].Style.Border.Left.Style = ExcelBorderStyle.Thick;
    
        //make the borders of cells A18 - J18 double and with a purple color
        worksheet.Cells["A18:J18"].Style.Border.Top.Style = ExcelBorderStyle.Double;
        worksheet.Cells["A18:J18"].Style.Border.Bottom.Style = ExcelBorderStyle.Double;
        worksheet.Cells["A18:J18"].Style.Border.Top.Color.SetColor(Color.Purple);
        worksheet.Cells["A18:J18"].Style.Border.Bottom.Color.SetColor(Color.Purple);
    
        //make all text fit the cells
        worksheet.Cells[worksheet.Dimension.Address].AutoFitColumns();
    
        //i use this to make all columms just a bit wider, text would sometimes still overflow after AutoFitColumns(). Bug?
        for (int col = 1; col <= worksheet.Dimension.End.Column; col++)
        {
            worksheet.Column(col).Width = worksheet.Column(col).Width + 1;
        }
    
        //make column H wider and set the text align to the top and right
        worksheet.Column(8).Width = 25;
        worksheet.Column(8).Style.HorizontalAlignment = ExcelHorizontalAlignment.Right;
        worksheet.Column(8).Style.VerticalAlignment = ExcelVerticalAlignment.Top;

        //get the image from disk
        using (System.Drawing.Image image = System.Drawing.Image.FromFile(HttpContext.Current.Server.MapPath("logo.jpg")))
        {
            var excelImage = worksheet.Drawings.AddPicture("My Logo", image);
    
            //add the image to row 20, column E
            excelImage.SetPosition(20, 0, 5, 0);
        }
    }

## Add an image to a sheet
    //get the image from disk
    using (System.Drawing.Image image = System.Drawing.Image.FromFile(HttpContext.Current.Server.MapPath("logo.jpg")))
    {
        var excelImage = worksheet.Drawings.AddPicture("My Logo", image);
    
        //add the image to row 20, column E
        excelImage.SetPosition(20, 0, 5, 0);
    }

