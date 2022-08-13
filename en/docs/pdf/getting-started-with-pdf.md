---
title: "Getting started with pdf"
slug: "getting-started-with-pdf"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
To view a pdf you can [download Adobe reader for free][1] . You can create pdfs programmatically with the help of, e.g by using [iTextSharp][2], [jsPDF][3]  or [PDFSharp][4] (there are other libraries available)


  [1]: https://get.adobe.com/reader/
  [2]: http://sourceforge.net/projects/itextsharp/
  [3]: https://github.com/MrRio/jsPDF
  [4]: http://pdfsharp.codeplex.com/

## PDFTK Server for pdf manipulation
Install PDFTK Server from
https://www.pdflabs.com/tools/pdftk-server/

PDFtk Server is a command line tool which can:

    •    Merge PDF Documents or Collate PDF Page Scans
    •    Split PDF Pages into a New Document
    •    Rotate PDF Documents or Pages
    •    Decrypt Input as Necessary (Password Required)
    •    Encrypt Output as Desired
    •    Fill PDF Forms with X/FDF Data and/or Flatten Forms
    •    Generate FDF Data Stencils from PDF Forms
    •    Apply a Background Watermark or a Foreground Stamp
    •    Report PDF Metrics, Bookmarks and Metadata
    •    Add/Update PDF Bookmarks or Metadata
    •    Attach Files to PDF Pages or the PDF Document
    •    Unpack PDF Attachments
    •    Burst a PDF Document into Single Pages
    •    Uncompress and Re-Compress Page Streams
    •    Repair Corrupted PDF (Where Possible)

PDFtk Server does not require Adobe Acrobat or Reader, and it runs on Windows, Mac OS X and Linux.

Collate scanned pages

    pdftk A=even.pdf B=odd.pdf shuffle A B output collated.pdf

or if odd.pdf is in reverse order:

    pdftk A=even.pdf B=odd.pdf shuffle A Bend-1 output collated.pdf

Decrypt a PDF

    pdftk secured.pdf input_pw foopass output unsecured.pdf

Encrypt a PDF using 128-bit strength (the default), withhold all permissions (the default)

    pdftk 1.pdf output 1.128.pdf owner_pw foopass

Same as above, except password baz must also be used to open output PDF

    pdftk 1.pdf output 1.128.pdf owner_pw foo user_pw baz

Same as above, except printing is allowed (once the PDF is open)

    pdftk 1.pdf output 1.128.pdf owner_pw foo user_pw baz allow printing

Join in1.pdf and in2.pdf into a new PDF, out1.pdf

    pdftk in1.pdf in2.pdf cat output out1.pdf

   or (using handles):

    pdftk A=in1.pdf B=in2.pdf cat A B output out1.pdf

   or (using wildcards):

    pdftk *.pdf cat output combined.pdf

Remove page 13 from in1.pdf to create out1.pdf

    pdftk in.pdf cat 1-12 14-end output out1.pdf

   or:

    pdftk A=in1.pdf cat A1-12 A14-end output out1.pdf










## Code sample from pdfsharp.net

   [Code source][1]   [View the output here][2]

    using System;
    using System.Diagnostics;
    using System.IO;
    using PdfSharp;
    using PdfSharp.Drawing;
    using PdfSharp.Pdf;
    using PdfSharp.Pdf.IO;
     
    namespace HelloWorld
    {
      /// <summary>
      /// This sample is the obligatory Hello World program.
      /// </summary>
      class Program
      {
        static void Main(string[] args)
        {
          // Create a new PDF document
          PdfDocument document = new PdfDocument();
          document.Info.Title = "Created with PDFsharp";
     
          // Create an empty page
          PdfPage page = document.AddPage();
     
          // Get an XGraphics object for drawing
          XGraphics gfx = XGraphics.FromPdfPage(page);
     
          // Create a font
          XFont font = new XFont("Verdana", 20, XFontStyle.BoldItalic);
     
          // Draw the text
          gfx.DrawString("Hello, World!", font, XBrushes.Black,
            new XRect(0, 0, page.Width, page.Height),
            XStringFormats.Center);
     
          // Save the document...
          const string filename = "HelloWorld.pdf";
          document.Save(filename);
          // ...and start a viewer.
          Process.Start(filename);
        }
      }
    }

  [1]: http://www.pdfsharp.net/wiki/HelloWorld-sample.ashx
  [2]: http://www.pdfsharp.net/wiki/GetFile.aspx?File=%2FHelloWorld%2FHelloWorld.pdf

