---
title: "Columns iText 5 versus iText 7"
slug: "columns-itext-5-versus-itext-7"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

In iText 5, you can't use the `add()` method to add a `Paragraph` to a `Document` if you want to organize the content in columns. We can't reuse the code of the https://www.wikiod.com/itext/text-to-pdf-itext-5-versus-itext-7#Text2Pdf.java (iText 5) example.

Instead we have to create a `ColumnText` object, we have to add all the `Paragraph` objects to this object, and once we've finished adding all the content, we can start rendering that content using the `go()` method. While doing so, we have to keep track of the columns, and create new pages when necessary.

**What we fixed in iText 7:**

With iText 7, we can copy and paste the code from the https://www.wikiod.com/itext/text-to-pdf-itext-5-versus-itext-7#Text2Pdf.java (iText 7) example. We can continue using the `add()` method the same way we did before. If we want to render the content in two columns instead of in one, we simple have to change the document renderer:

    Rectangle[] columns = {
        new Rectangle(36, 36, 254, 770),
        new Rectangle(305, 36, 254, 770)};
    document.setRenderer(new ColumnDocumentRenderer(document, columns));

**Want to know more?**

Read [Working with the RootElement](http://developers.itextpdf.com/content/itext-7-building-blocks/chapter-2-working-rootelement) which is chapter 5 in the [iText 7: Building Blocks](http://developers.itextpdf.com/content/itext-7-building-blocks/) tutorial. [Get the free ebook!](http://pages.itextpdf.com/iText-7-abc.html)

## Text2PdfColumns.java (iText 5)
Suppose that we have the following text file: [jekyll_hyde.txt](http://gitlab.itextsupport.com/itext/sandbox/raw/master/resources/text/jekyll_hyde.txt)

How do we convert it to a PDF that looks like this:

[![enter image description here][1]][1]

When using iText 5, you'd need code like this:

    public void createPdf(String dest)
    throws DocumentException, IOException {
        Document document = new Document();
        PdfWriter writer = PdfWriter.getInstance(document, new FileOutputStream(dest));
        document.open();
        ColumnText ct = new ColumnText(writer.getDirectContent());
        BufferedReader br = new BufferedReader(new FileReader(TEXT));
        String line;
        Paragraph p;
        Font normal = new Font(FontFamily.TIMES_ROMAN, 12);
        Font bold = new Font(FontFamily.TIMES_ROMAN, 12, Font.BOLD);
        boolean title = true;
        while ((line = br.readLine()) != null) {
            p = new Paragraph(line, title ? bold : normal);
            p.setAlignment(Element.ALIGN_JUSTIFIED);
            title = line.isEmpty();
            ct.addElement(p);
        }
        Rectangle[] columns = {
            new Rectangle(36, 36, 290, 806), new Rectangle(305, 36, 559, 806)
        };
        int c = 0;
        int status = ColumnText.START_COLUMN;
        while (ColumnText.hasMoreText(status)) {
            ct.setSimpleColumn(columns[c]);
            status = ct.go();
            if (++c == 2) {
                document.newPage();
                c = 0;
            }
        }
        document.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/columns#2860-text2pdfcolumns.java)

  [1]: http://i.stack.imgur.com/zbn3v.png

## Text2PdfColumns.java (iText 7)
Suppose that you have the following text file: [jekyll_hyde.txt](http://gitlab.itextsupport.com/itext7/samples/raw/develop/sandbox/src/main/resources/txt/jekyll_hyde.txt)

How do we convert it to a PDF that looks like this:

[![enter image description here][1]][1]

When using iText 7, you'd need code like this:

    public void createPdf(String dest) throws IOException {
        PdfDocument pdf = new PdfDocument(new PdfWriter(dest));
        Document document = new Document(pdf)
            .setTextAlignment(TextAlignment.JUSTIFIED);
        Rectangle[] columns = {
            new Rectangle(36, 36, 254, 770),
            new Rectangle(305, 36, 254, 770)};
        document.setRenderer(new ColumnDocumentRenderer(document, columns));
        BufferedReader br = new BufferedReader(new FileReader(TEXT));
        String line;
        PdfFont normal = PdfFontFactory.createFont(FontConstants.TIMES_ROMAN);
        PdfFont bold = PdfFontFactory.createFont(FontConstants.TIMES_BOLD);
        boolean title = true;
        while ((line = br.readLine()) != null) {
            document.add(new Paragraph(line).setFont(title ? bold : normal));
            title = line.isEmpty();
        }
        document.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/columns#2861-text2pdfcolumns.java) and the [iText 7: Building Blocks](http://pages.itextpdf.com/iText-7-abc.html) tutorial.

  [1]: http://i.stack.imgur.com/lBwb5.png

