---
title: "Creating charts"
slug: "creating-charts"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

How to create charts with EPPlus

## Pie Chart
    //create a new ExcelPackage
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create a WorkSheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //fill cell data with a loop, note that row and column indexes start at 1
        Random rnd = new Random();
        for (int i = 1; i <= 10; i++)
        {
            worksheet.Cells[1, i].Value = "Value " + i;
            worksheet.Cells[2, i].Value = rnd.Next(5, 15);
        }
    
        //create a new piechart of type Pie3D
        ExcelPieChart pieChart = worksheet.Drawings.AddChart("pieChart", eChartType.Pie3D) as ExcelPieChart;
    
        //set the title
        pieChart.Title.Text = "PieChart Example";
    
        //select the ranges for the pie. First the values, then the header range
        pieChart.Series.Add(ExcelRange.GetAddress(2, 1, 2, 10), ExcelRange.GetAddress(1, 1, 1, 10));
    
        //position of the legend
        pieChart.Legend.Position = eLegendPosition.Bottom;
    
        //show the percentages in the pie
        pieChart.DataLabel.ShowPercent = true;
    
        //size of the chart
        pieChart.SetSize(500, 400);
    
        //add the chart at cell C5
        pieChart.SetPosition(4, 0, 2, 0);
    }

## Line Chart
    //create a new ExcelPackage
    using (ExcelPackage excelPackage = new ExcelPackage())
    {
        //create a WorkSheet
        ExcelWorksheet worksheet = excelPackage.Workbook.Worksheets.Add("Sheet 1");
    
        //fill cell data with a loop, note that row and column indexes start at 1
        Random rnd = new Random();
        for (int i = 2; i <= 11; i++)
        {
            worksheet.Cells[1, i].Value = "Value " + (i - 1);
            worksheet.Cells[2, i].Value = rnd.Next(5, 25);
            worksheet.Cells[3, i].Value = rnd.Next(5, 25);
        }
        worksheet.Cells[2, 1].Value = "Age 1";
        worksheet.Cells[3, 1].Value = "Age 2";
    
        //create a new piechart of type Line
        ExcelLineChart lineChart = worksheet.Drawings.AddChart("lineChart", eChartType.Line) as ExcelLineChart;
    
        //set the title
        lineChart.Title.Text = "LineChart Example";
    
        //create the ranges for the chart
        var rangeLabel = worksheet.Cells["B1:K1"];
        var range1 = worksheet.Cells["B2:K2"];
        var range2 = worksheet.Cells["B3:K3"];
    
        //add the ranges to the chart
        lineChart.Series.Add(range1, rangeLabel);
        lineChart.Series.Add(range2, rangeLabel);
    
        //set the names of the legend
        lineChart.Series[0].Header = worksheet.Cells["A2"].Value.ToString();
        lineChart.Series[1].Header = worksheet.Cells["A3"].Value.ToString();
    
        //position of the legend
        lineChart.Legend.Position = eLegendPosition.Right;
    
        //size of the chart
        lineChart.SetSize(600, 300);
    
        //add the chart at cell B6
        lineChart.SetPosition(5, 0, 1, 0);
    }

