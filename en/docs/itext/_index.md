---
title : itext Tutorial
slug : itext-tutorial
weight : 9827
draft : false
images : []
type : docs
---

If you look at PDF creation, you'll find two different approaches:

* Graphical designers use desktop applications such as Adobe Acrobat or Adobe InDesign to create a document in a manual or semimanual process.
* In another context, PDF documents are created programmatically, using an API to produce PDFs directly from software applications, without —or with minimal— human intervention. Sometimes the document is created in an intermediary format first (e.g. XML, HTML,...), and then converted to PDF.

These different approaches demand different software products.

The same goes for PDF manipulation.

* You can update a PDF manually in tools such as Adobe Acrobat,
* There are also tools that allow forms to be filled out automatically based on information from a database.

iText is a tool that focuses on the automation side of things.

# What is iText?

iText is an SDK that was developed to allow developers to do the following (and much more):

* Generate documents and reports based on data from an XML file or a database
* Create maps and books, exploiting numerous interactive features available in PDF
* Add bookmarks, page numbers, watermarks, and other features to existing PDF documents
* Split or concatenate pages from existing PDF files
* Fill out interactive forms
* Digitally sign PDF documents
* Serve dynamically generated or manipulated PDF documents to a web browser

iText is not an end-user tool. You have to build iText into your own applications so that you can automate the PDF creation and manipulation process.

# When to use iText?

Typically, iText is used in projects that have one of the following requirements:

* The content isn't available in advance: it's calculated based on user input or real-time database information.
* The PDF files can't be produced manually due to the massive volume of content: a large number of pages or documents.
* Documents need to be created in unattended mode, in a batch process.
* The content needs to be customized or personalized; for instance, the name of the end user has to be stamped on a number of pages.

Often you'll encounter these requirements in web applications, where content needs to be served dynamically to a browser. Normally, you'd serve this information in the form of HTML, but for some documents, PDF is preferred over HTML for better printing quality, for identical presentation on a variety of platforms, for security reasons, to comply with specific industry standards (such as PAdES, PDF/A, or PDF/UA), or to reduce the file size.

