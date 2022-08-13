---
title: "Create New Word Document with Open XML"
slug: "create-new-word-document-with-open-xml"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

The OpenXML document markup standard is an XML based format which enables solutions on many software platforms and operating systems.

## Hello World
First, create a new console project using Visual Studio and add the following .dlls to your project:

    DocumentFormat.OpenXml
    WindowsBase

Next, compile and execute the following code:

    static void Main(string[] args)
    {
        // Create a Wordprocessing document. 
        using ( WordprocessingDocument package = WordprocessingDocument.Create("HelloWorld.docx", WordprocessingDocumentType.Document))
        {
            // Add a new main document part. 
            package.AddMainDocumentPart();

            // Create the Document DOM. 
            package.MainDocumentPart.Document =
                new Document(
                    new Body(
                        new Paragraph(
                            new Run(
                                new Text("Hello World!")))));

            // Save changes to the main document part. 
            package.MainDocumentPart.Document.Save();
        }
    }
Under your `\bin\Debug` folder you should have your first WordprocessingML document:

[![enter image description here][1]][1]


The text that we added in the above example is stored under the main document part. Inside the main document part there is the **document** element which allows a child element **body** to store the text which makes our document. There are two main groups of content for the document body, block level *(paragraphs and tables)* and inline content *(runs and text)*. The block level content provides the main structure and contains inline content. To understand the example above, we first need to understand the text hierarchy in WordprocessingML. A paragraph is split into different runs. A run is the lowest level element to which formatting can be applied. The run is split up again into various text elements.

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/i7R6a.png
  [2]: https://i.stack.imgur.com/Xjoj4.png

