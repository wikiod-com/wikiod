---
title: "Fonts iText 5 versus iText 7"
slug: "fonts-itext-5-versus-itext-7"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

In the first versions of iText, there was only one font class: `Font`.

With this font, you could create a `Font` object for fourteen fonts from five font families: Helvetica (regular, bold, oblique, bold-oblique), Times Roman (regular, bold, italic, bold-italic), Courier (regular, bold, oblique, bold-oblique), Symbol and Zapf Dingbats.

Such a `Font` object was created like this:

    Font font = new Font(FontFamily.TIMES_ROMAN);

You also had to define the font size, for instance:

    Font font14pt = new Font(FontFamily.TIMES_ROMAN, 14);

The default font was Helvetica; the default font size 12.

iText evolved and more fonts were supported. The `BaseFont` class was used to deal with these fonts internally. A `BaseFont` class was created like this:

    BaseFont bf_russian = BaseFont.createFont(
        "resources/fonts/FreeSans.ttf",
        "CP1251",
        BaseFont.EMBEDDED);

The first parameter is the path to a font program, for instance a TTF file, the second parameter is the encoding, for instance CP1251 for Cyrillic characters, the third parameter indicates if a subset of the font needs to be embedded.

The `BaseFont` class is to be used when you add content at the lowest level, for instance when creating text objects in your code using `beginText()`, `setFontAndSize()`, `setTextMatrix()`, `showText()`, `endText()` sequences. Typically, you will only use this low-level approach if you are a PDF specialist. If you don't know anything of PDF syntax, you shouldn't use such a sequence.

You can also use the `BaseFont` class to create a `Font` object:

    Font russian = new Font(bf_russian, 12);

Now we can use the `russian` font to create a `Paragraph` that contains Russian text.

There are some other ways in iText 5 to create `Font` objects, but this is the most common procedure. People were sometimes confused by the difference between `Font` and `BaseFont`, and they didn't always use the correct approach.

**What we fixed in iText 7:**

We made things more uniform. There is now a single `PdfFont` class, and you create a font using a `PdfFontFactory`:

    PdfFont font = PdfFontFactory.createFont(FontConstants.TIMES_ROMAN);
    PdfFont russian = PdfFontFactory.createFont(
        "src/main/resources/fonts/FreeSans.ttf", "CP1251", true);

You no longer need to create different font objects if you want to switch to another font size. Switching to a different font size can simply be done using the `setFontSize()` method:

    Paragraph p = new Paragraph("Hello World! ")
        .add(new Text("Hallo Wereld! ").setFontSize(14))
        .add(new Text("Bonjour le monde! ").setFontSize(10));

The default font is still Helvetica and the default font size is still 12, but you can now define a font (and a font size) for the document:

    document.setFont(font);

In this case `font` will be the default font when adding a building block (for instance a `Paragraph`) without specifying a font.

**Want to know more?**

Read [Introducing the PdfFont class](http://developers.itextpdf.com/content/itext-7-building-blocks/chapter-1-introducing-pdffont-class) which is chapter 1 in the [iText 7: Building Blocks](http://developers.itextpdf.com/content/itext-7-building-blocks/) tutorial. [Get the free ebook!](http://pages.itextpdf.com/iText-7-abc.html)

## HelloWorldInternational.java (iText 5)
In this iText 5 example, we will create a Hello World example in different languages, using different fonts:

[![enter image description here][1]][1]

    public void createPdf(String dest)
        throws DocumentException, IOException {
        Document document = new Document();
        PdfWriter.getInstance(document, new FileOutputStream(dest));
        document.open();
        Font font = new Font(FontFamily.TIMES_ROMAN);
        Font font14pt = new Font(FontFamily.TIMES_ROMAN, 14);
        Font font10pt = new Font(FontFamily.TIMES_ROMAN, 10);
        BaseFont bf_russian = BaseFont.createFont(
            "resources/fonts/FreeSans.ttf",
            "CP1251",
            BaseFont.EMBEDDED);
        Font russian = new Font(bf_russian, 12);
        BaseFont bf_cjk = BaseFont.createFont(
            "resources/fonts/NotoSansCJKsc-Regular.otf",
            BaseFont.IDENTITY_H,
            BaseFont.EMBEDDED);
        Font cjk = new Font(bf_cjk, 12);
        Paragraph p = new Paragraph("Hello World! ", font);
        Chunk chunk = new Chunk("Hallo Wereld! ", font14pt);
        p.add(chunk);
        chunk = new Chunk("Bonjour le monde! ", font10pt);
        chunk.setTextRise(4);
        p.add(chunk);
        chunk = new Chunk(
            "\u0417\u0434\u0440\u0430\u0432\u0441\u0442\u0432\u0443\u043b\u0442\u0435 \u043c\u0438\u0440! ",
            russian);
        p.add(chunk);
        p.add(new Chunk("\u4f60\u597d\u4e16\u754c! ", cjk));
        p.add(new Chunk("\uc5ec\ubcf4\uc138\uc694 \uc138\uacc4!", cjk));
        document.add(p);
        document.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/fonts#2842-helloworldinternational.java)

  [1]: http://i.stack.imgur.com/2cvy0.png

## HelloWorldInternational.java (iText 7)
In this iText 7 example, we will create a Hello World example in different languages, using different fonts:

[![enter image description here][1]][1]

    public void createPdf(String dest) throws IOException {
        PdfDocument pdf = new PdfDocument(new PdfWriter(dest));
        try (Document document = new Document(pdf)) {
            PdfFont font = PdfFontFactory.createFont(FontConstants.TIMES_ROMAN);
            PdfFont russian = PdfFontFactory.createFont(
                "src/main/resources/fonts/FreeSans.ttf",
                "CP1251", true);
            PdfFont cjk = PdfFontFactory.createFont(
                "src/main/resources/fonts/NotoSansCJKsc-Regular.otf",
                PdfEncodings.IDENTITY_H, true);
            document.setFont(font);
            Paragraph p = new Paragraph("Hello World! ")
                .add(new Text("Hallo Wereld! ").setFontSize(14))
                .add(new Text("Bonjour le monde! ").setFontSize(10).setTextRise(4))
                .add(
                    new Text("\u0417\u0434\u0440\u0430\u0432\u0441\u0442\u0432\u0443\u043b\u0442\u0435 \u043c\u0438\u0440! ")
                        .setFont(russian))
                .add(new Text("\u4f60\u597d\u4e16\u754c! ")
                    .setFont(cjk))
                .add(new Text("\uc5ec\ubcf4\uc138\uc694 \uc138\uacc4!")
                    .setFont(cjk));
            document.add(p);
        }
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/fonts#2843-helloworldinternational.java) and the [iText 7: Building Blocks](http://pages.itextpdf.com/iText-7-abc.html) tutorial.

  [1]: http://i.stack.imgur.com/NhQ9V.png

