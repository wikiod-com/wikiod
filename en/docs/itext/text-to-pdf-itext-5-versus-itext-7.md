---
title: "Text to PDF iText 5 versus iText 7"
slug: "text-to-pdf-itext-5-versus-itext-7"
draft: false
images: []
weight: 9864
type: docs
toc: true
---

The code to convert a plain text file to a PDF document is pretty simple whether you use iText 5 or iText 7. In iText 7, you have the advantage that you can define the alignment at the level of the document. In iText 5, you have to set the alignment for every separate `Paragraph` object.

To understand the real difference between iText 5 and iText 7 in this pair of examples, we have to take a look at the resulting PDF. In iText 5, we end up with 35 pages of text. In iText 7, we have the same text distributed over 38 pages.

The text is easier to read when created by iText 7 because different defaults are used when creating the layout. You could get the same result from iText 5 code, but then you'd have to change some values with respect to spacing.

In iText 7, the default values were chosen based on 16 years of experience with iText. This way, you get a better result with less code.

**Want to know more?**

Read [Working with the RootElement](http://developers.itextpdf.com/content/itext-7-building-blocks/chapter-2-working-rootelement) which is chapter 5 in the [iText 7: Building Blocks](http://developers.itextpdf.com/content/itext-7-building-blocks/) tutorial. [Get the free ebook!](http://pages.itextpdf.com/iText-7-abc.html)

## Text2Pdf.java (iText 5)
Suppose that we have the following text file: [jekyll_hyde.txt](http://gitlab.itextsupport.com/itext/sandbox/raw/master/resources/text/jekyll_hyde.txt)

How do we convert it to a PDF that looks like this:

[![enter image description here][1]][1]

When using iText 5, we'd use the following code:

    public void createPdf(String dest)
    throws DocumentException, IOException {
        Document document = new Document();
        PdfWriter.getInstance(document, new FileOutputStream(dest));
        document.open();
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
            document.add(p);
        }
        document.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/text-pdf#2854-text2pdf.java)

  [1]: http://i.stack.imgur.com/U8GOT.png

## Text2Pdf.java (iText 7)
Suppose that you have the following text file: [jekyll_hyde.txt](http://gitlab.itextsupport.com/itext7/samples/raw/develop/sandbox/src/main/resources/txt/jekyll_hyde.txt)

How do we convert it to a PDF that looks like this:

[![enter image description here][1]][1]

When using iText 7, we'd need the following code:

    public void createPdf(String dest) throws IOException {
        PdfDocument pdf = new PdfDocument(new PdfWriter(dest));
        Document document = new Document(pdf)
            .setTextAlignment(TextAlignment.JUSTIFIED);
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

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/text-pdf#2855-text2pdf.java) and the [iText 7: Building Blocks](http://pages.itextpdf.com/iText-7-abc.html) tutorial.

  [1]: http://i.stack.imgur.com/2nyg2.png

