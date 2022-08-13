---
title: "Pdf Creation iText 5 versus iText 7"
slug: "pdf-creation-itext-5-versus-itext-7"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

In the original design for iText, it was possible to create a high-level `Document` object, and then have different `DocListener` objects listening to that `Document` object. This was achieved by using different writers: a `PdfWriter`, an `HTMLWriter`, and an `RtfWriter`. When using a `PdfWriter`, a `PdfDocument` was created internally. This low-level class took care of all PDF-related structures. More or less the same was true for the other formats.

Over the years, iText specialized and it became a pure PDF library. The creation of HTML and RTF was abandoned, hence it was no longer necessary to create a `Document` before creating a `PdfWriter`, but we had to stick to the original architecture because we weren't ready to break the API.

Over the years, we added more and more PDF functionality to iText, and the fact that `PdfDocument` was a class *for internal use only* became problematic. We used workarounds so that we could introduce new PDF features that belonged in the `PdfDocument` class up until the point that we reached the ceiling of what we considered acceptable as workarounds.

That's when we decided to rewrite iText from scratch and to create a completely new architecture for iText. Now we have a clear distinction between the `PdfDocument` (for low-level operations) and the `Document` (for high-level functionality). We no longer have to open the document, and if we use the *try-with-resources* approach, we don't even have to close it ourselves.

**Want to know more?** [Get the free ebook!](http://pages.itextpdf.com/iText-7-abc.html)

## HelloWorld.java (iText 5)
Suppose that we want to create a simple Hello World document:

[![enter image description here][1]][1]

In iText 5, this would be done like this:

    public void createPdf(String dest)
        throws DocumentException, IOException {
        Document document = new Document();
        PdfWriter.getInstance(
            document, new FileOutputStream(dest));
        document.open();
        document.add(new Paragraph("Hello World!"));
        document.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/hello-world#2836-helloworld.java)

  [1]: http://i.stack.imgur.com/m7ImG.png

## HelloWorld1.java and HelloWorld2.java (iText 7)
Suppose that we wanted to create a simple Hello World document:

[![enter image description here][1]][1]

In iText 7, we could do that like this:

    public void createPdf(String dest) throws IOException {
        PdfDocument pdf = new PdfDocument(new PdfWriter(dest));
        Document document = new Document(pdf);
        document.add(new Paragraph("Hello World!"));
        document.close();
    }

Or, we could even do it like this:

    public void createPdf(String dest) throws IOException {
        PdfDocument pdf = new PdfDocument(new PdfWriter(dest));
        try (Document document = new Document(pdf)) {
            document.add(new Paragraph("Hello World!"));
        }
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/hello-world#2837-helloworld1.java) and the [iText 7: Building Blocks](http://pages.itextpdf.com/iText-7-abc.html) tutorial.


  [1]: http://i.stack.imgur.com/j4AzY.png

