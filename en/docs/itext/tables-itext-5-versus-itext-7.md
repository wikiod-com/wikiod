---
title: "Tables iText 5 versus iText 7"
slug: "tables-itext-5-versus-itext-7"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

The iText 5 class names `PdfPTable` and `PdfPCell` were chosen because we already had classes named `Table` and `Cell` to create table and cell objects at the highest programming level. There was also a class named `PdfTable` to be used by iText internally. Those classes had a lot of flaws and they were deprecated in favor of `PdfPTable` and `PdfPCell`. They have been removed a long time ago.

Over the years, `PdfPTable` and `PdfPCell` also received some criticism from users. For instance: users didn't understand the difference between **text mode** and **composite** mode.

**Text mode** is used when you create a `PdfPCell` like this:

    cell = new PdfPCell(new Phrase("Cell with rowspan 2"));

In this case, you define properties like the horizontal alignment on the level of the `PdfPCell`.

**Composite mode** kicks in the moment you use the `addElement()` method:

    cell = new PdfPCell();
    cell.addElement(new Phrase("Cell 1.2"));

In this case, some properties defined at the level of the `PdfPCell` (such as the horizontal alignment) are ignored. The horizontal alignment is to be defined at the level of the elements added to the cell. For instance: if you want to create a cell in which different paragraphs need to have a different horizontal alignment, you will switch to composite mode.

If you look at the screen shot of the table created with the iText 5 example, you will notice that the cells with content **Cell 1.1** (added in text mode) and **Cell 1.2** (added in composite mode) are aligned quite differently.

In answer to the criticism on the odd alignment, we introduced methods to use ascender and descender information. We use these methods for the cells with content **Cell 2.1** (added in text mode) and **Cell 2.2** (added in composite mode). We also introduced a padding of 5 for these cells.

Now the result is much better.

**What we fixed in iText 7:**

Since we created iText 7 from scratch, we had no legacy classes with names we couldn't reuse. We introduced a new `Table` and a new `Cell` class.

There is no more text mode and no more composite mode. A `Cell` is created either without parameters, or with parameters that define the rowspan and the colspan. All content is added the same way: using the `add()` method.

Our customers were also asking to provide a means to distinguish a margin and a padding. In the iText 7 example, we added a gray background to show the difference. In the cell with content **Cell 2.1**, we define a margin of 5 user units. The default padding is 2. In the cell with content **Cell 2.2**, we define a padding of 5 user units, the default margin in 0.

As you can tell from the screen shots, the cells are rendered quite nicely. We didn't have to use methods to set the ascender or descender. The default behavior is much closer to the behavior a developer would expect.

**Want to know more about tables and cells in iText 7?**

Read [Adding AbstractElement objects (part 2)](http://developers.itextpdf.com/content/itext-7-building-blocks/chapter-1-introducing-pdffont-class) which is chapter 5 in the [iText 7: Building Blocks](http://developers.itextpdf.com/content/itext-7-building-blocks/) tutorial. [Get the free ebook!](http://pages.itextpdf.com/iText-7-abc.html)

## HelloWorldTable.java (iText 5)
In this example, we'll create the following table using iText 5:

[![enter image description here][1]][1]

We need the `PdfPTable` and `PdfPCell` class to achieve this:

    public void createPdf(String dest) throws IOException, DocumentException {
        Document document = new Document();
        PdfWriter.getInstance(document, new FileOutputStream(dest));
        document.open();
        PdfPTable table = new PdfPTable(3);
        PdfPCell cell = new PdfPCell(new Phrase("Cell with colspan 3"));
        cell.setColspan(3);
        cell.setHorizontalAlignment(Element.ALIGN_CENTER);
        table.addCell(cell);
        cell = new PdfPCell(new Phrase("Cell with rowspan 2"));
        cell.setRowspan(2);
        cell.setVerticalAlignment(Element.ALIGN_MIDDLE);
        table.addCell(cell);
        table.addCell("Cell 1.1");
        cell = new PdfPCell();
        cell.addElement(new Phrase("Cell 1.2"));
        table.addCell(cell);
        cell = new PdfPCell(new Phrase("Cell 2.1"));
        cell.setPadding(5);
        cell.setUseAscender(true);
        cell.setUseDescender(true);
        cell.setHorizontalAlignment(Element.ALIGN_CENTER);
        table.addCell(cell);
        cell = new PdfPCell();
        cell.setPadding(5);
        cell.setUseAscender(true);
        cell.setUseDescender(true);
        Paragraph p = new Paragraph("Cell 2.2");
        p.setAlignment(Element.ALIGN_CENTER);
        cell.addElement(p);
        table.addCell(cell);
        document.add(table);
        document.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/tables#2850-helloworldtable.java)

  [1]: http://i.stack.imgur.com/tIQcW.png

## HelloWorldTable.java (iText 7)
In this example, we'll create the following table using iText 7:

[![enter image description here][1]][1]

We'll need the `Table` and `Cell` class to achieve this:

    public void createPdf(String dest) throws IOException {
        PdfDocument pdf = new PdfDocument(new PdfWriter(dest));
        try (Document document = new Document(pdf)) {
            Table table = new Table(3);
            Cell cell = new Cell(1, 3)
                .setTextAlignment(TextAlignment.CENTER)
                .add("Cell with colspan 3");
            table.addCell(cell);
            cell = new Cell(2, 1)
                .add("Cell with rowspan 2")
                .setVerticalAlignment(VerticalAlignment.MIDDLE);
            table.addCell(cell);
            table.addCell("Cell 1.1");
            table.addCell(new Cell().add("Cell 1.2"));
            table.addCell(new Cell()
                .add("Cell 2.1")
                .setBackgroundColor(Color.LIGHT_GRAY)
                .setMargin(5));
            table.addCell(new Cell()
                .add("Cell 1.2")
                .setBackgroundColor(Color.LIGHT_GRAY)
                .setPadding(5));
            document.add(table);
        }
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/tables#2851-helloworldtable.java) and the [iText 7: Building Blocks](http://pages.itextpdf.com/iText-7-abc.html) tutorial.

  [1]: http://i.stack.imgur.com/GZOzw.png

