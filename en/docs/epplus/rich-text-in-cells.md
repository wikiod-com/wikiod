---
title: "Rich Text in cells"
slug: "rich-text-in-cells"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Most of the time, when we create spreadsheets, we just use a Cell's Value property to put content in the cell and the Style property to format it.

Occasionally, however, we may wish to apply multiple styles to a cell - maybe put a bold and underlined title before the rest of the content, or highlight a particular part of the text in Red - this is where the cell's RichText property comes into play.

## Adding RichText to a cell
Each element of text you want to use distinct formatting on should be added separately, by adding to the cell's RichText collection property.


    var cell = ws.Cells[1,1];
    cell.IsRichText = true;     // Cell contains RichText rather than basic values
    cell.Style.WrapText = true; // Required to honor new lines

    var title = cell.RichText.Add("This is my title");
    var text = cell.RichText.Add("\nAnd this is my text");

Note that each time you Add() a new string, it will inherit the formatting from the previous section. As such, if you want to change the default formatting you will only need to change it on the first string added.

This behaviour can, however, cause some confusion when formatting your text. Using the example above, the following code will make ***all of the text*** in the cell Bold and Italic - this is not the desired behavior:

    // Common Mistake
    var title = cell.RichText.Add("This is my title");
    title.Bold = true;
    title.Italic = true;

    var text = cell.RichText.Add("\nAnd this is my text"); // Will be Bold and Italic too


The preferred approach is to add all your text sections first, then apply section-specific formatting afterwards, as shown here:

    var title = cell.RichText.Add("This is my title");
    title.FontName = "Verdana";    // This will be applied to all subsequent sections as well

    var text = cell.RichText.Add("\nAnd this is my text");

    // Format JUST the title
    title.Bold = true;
    title.Italic = true;



## Text formatting Properties
There are a number of properties that can be applied to sections of RichText.

    var title = cell.RichText.Add("This is my title");

    // Data Type:     bool
    // Default Value: false
    title.Bold = true;

    // Data Type:     System.Drawing.Color
    // Default Value: Color.Black
    title.Color = Color.Red;
    title.Color = Color.FromArgb(255, 0, 0);
    title.Color = ColorTranslator.FromHtml("#FF0000");

    // Data Type:     string
    // Default Value: "Calibri"
    title.FontName = "Verdana";

    // Data Type:     bool
    // Default Value: false
    title.Italic = true;

    // Data Type:     bool
    // Default Value: true
    // If this property is set to false, any whitespace (including new lines) 
    // is trimmed from the start and end of the Text
    title.PreserveSpace = true;

    // Data Type:     float
    // Default Value: 11
    // The font size is specified in Points
    title.Size = 16;

    // Data Type:     bool
    // Default Value: false
    // Strikethrough
    title.Strike = false;

    // Data Type:     string
    // Default Value: Whatever was set when the text was added to the RichText collection
    title.Text += " (updated)";

    // Data Type:     bool
    // Default Value: false
    title.UnderLine = true;

    // Data Type:     OfficeOpenXml.Style.ExcelVerticalAlignmentFont
    // Default Value: ExcelVerticalAlignmentFont.None
    title.VerticalAlign = ExcelVerticalAlignmentFont.None;

## Inserting RichText in a cell
EPPlus also supports the ability to insert text in a cell using the Insert() method. For example:

    var file = new FileInfo(filePath);
    using (var p = new ExcelPackage(file))
    {
        var wb = p.Workbook;
        var ws = wb.Worksheets.FirstOrDefault() ?? wb.Worksheets.Add("Sheet1");

        var cell = ws.Cells[1, 1];
        cell.IsRichText = true;
        cell.RichText.Clear(); // Remove any RichText that may be in the cell already
        var s1 = cell.RichText.Add("Section 1.");
        var s2 = cell.RichText.Add("Section 2.");

        var s3 = cell.RichText.Insert(1, "Section 3.");

        s3.Bold = true;
        p.Save();
    }

Note that the Insert() method does NOT insert at a character index, but at a Section index. Because the sections are zero-indexed, the above code will produce the following text in the cell:

Section 1.Section 3.Section 2.



