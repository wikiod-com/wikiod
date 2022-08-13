---
title: "Styles iText 5 versus iText 7"
slug: "styles-itext-5-versus-itext-7"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

Creating a document in which you have to switch between styles frequently tends to be tedious in iText 5. You need to create a lot of `Chunk` objects and you always have to make a trade-off between applying the styles directly to every new `Chunk` or creating a helper method that creates the `Chunk` for you.

**What we fixed in iText 7:**

It is now possible to chain methods. The `setFont()`, `setFontSize()`, `addStyle()`, and other methods all return the object on which they are invoked. Adding a `Paragraph` involving different styles can now be done in one line:

    document.add(
        new Paragraph()
            .add("In this example, named ")
            .add(new Text("HelloWorldStyles").addStyle(style))
            .add(", we experiment with some text in ")
            .add(new Text("code style").addStyle(style))
            .add("."));

Using the `Style` object, you can now also apply different properties (font, font color, background color, font size,...) in one go with the `addStyle()` method.

**Want to know more?**

Read [Introducing the PdfFont class](http://developers.itextpdf.com/content/itext-7-building-blocks/chapter-1-introducing-pdffont-class) which is chapter 1 in the [iText 7: Building Blocks](http://developers.itextpdf.com/content/itext-7-building-blocks/) tutorial. [Get the free ebook!](http://pages.itextpdf.com/iText-7-abc.html)

## HelloWorldStyles.java (iText 5)
In this iText 5 example, we need to switch between different styles in the same document:

[![enter image description here][1]][1]

The best way to do this in iText 5, is to create a convenience method that creates a `Chunk` in the style that needs to be used frequently; see the `createBgChunk()` method:

    public Chunk createBgChunk(String s, Font font) {
        Chunk chunk = new Chunk(s, font);
        chunk.setBackground(BaseColor.LIGHT_GRAY);
        return chunk;
    }

We can now use this method in the code that creates the PDF:

    public void createPdf(String dest)
    throws DocumentException, IOException {
        Document document = new Document();
        PdfWriter.getInstance(document, new FileOutputStream(dest));
        document.open();
        Font code = new Font(FontFamily.COURIER, 12, Font.NORMAL, BaseColor.RED);
        Paragraph p = new Paragraph("In this example, named ");
        p.add(createBgChunk("HelloWorldStyles", code));
        p.add(", we experiment with some text in ");
        p.add(createBgChunk("code style", code));
        p.add(".");
        document.add(p);
        document.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/styles#2846-helloworldstyles.java)

  [1]: http://i.stack.imgur.com/RF0Ze.png


## HelloWorldStyles.java (iText 7)
In this iText 7 example, we need to switch between different styles in the same document:

[![enter image description here][1]][1]

The best way to achieve this in iText 7, is to create a `Style` object, and to apply that `Style` to a `Text` object:

    public void createPdf(String dest) throws IOException {
        PdfDocument pdf = new PdfDocument(new PdfWriter(dest));
        PdfFont code = PdfFontFactory.createFont(FontConstants.COURIER);
        Style style = new Style()
            .setFont(code)
            .setFontSize(12)
            .setFontColor(Color.RED)
            .setBackgroundColor(Color.LIGHT_GRAY);
        try (Document document = new Document(pdf)) {
            document.add(
                new Paragraph()
                    .add("In this example, named ")
                    .add(new Text("HelloWorldStyles").addStyle(style))
                    .add(", we experiment with some text in ")
                    .add(new Text("code style").addStyle(style))
                    .add("."));
        }
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/styles#2847-helloworldstyles.java) and the [iText 7: Building Blocks](http://pages.itextpdf.com/iText-7-abc.html) tutorial.

  [1]: http://i.stack.imgur.com/EraL4.png

