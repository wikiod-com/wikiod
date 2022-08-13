---
title: "Simple Excel (XLSX) creation"
slug: "simple-excel-xlsx-creation"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Basic excel
        String fileName = "Fruit.xlsx"; 

        String sheetName = "Apples";

        XSSFWorkbook wb = new XSSFWorkbook();
        XSSFSheet sheet = wb.createSheet(sheetName) ;

        for (int r=0;r < 3; r++ )
        {
            XSSFRow row = sheet.createRow(r);

            //iterating c number of columns
            for (int c=0;c < 3; c++ )
            {
                XSSFCell cell = row.createCell(c);
    
                cell.setCellValue("Nice apple, in row: "+r+" and col: "+c);
            }
        }

        try(FileOutputStream fos = new FileOutputStream(fileName))
        {
            wb.write(fos);
        }

