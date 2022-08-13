---
title: "Getting started with itext"
slug: "getting-started-with-itext"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
This is a very simple program to create a PDF using iText 7 / Java:

    //Initialize writer
    PdfWriter writer = new PdfWriter(dest);

    //Initialize document
    PdfDocument pdfDoc = new PdfDocument(writer);
    Document doc = new Document(pdfDoc);

    //Add paragraph to the document
    doc.add(new Paragraph("Hello World!"));

    //Close document
    doc.close();

*([Listing_01_01_HelloWorld.java][1])*

You can navigate to many other examples from that page.

---

And this is a very simple program to create a PDF using the precursor iText 5.5.x / Java:

    // step 1
    Document document = new Document();
    // step 2
    PdfWriter.getInstance(document, new FileOutputStream(filename));
    // step 3
    document.open();
    // step 4
    document.add(new Paragraph("Hello World!"));
    // step 5
    document.close();

*([HelloWorld.java][2])*

There are many more examples to navigate from this page, too.

---

These two examples look pretty similar. The advantages of the re-designed iText 7 API will become apparent, though, as soon as one starts to look closer at less trivial examples. Thus, simply navigate through the example source code from the links above and compare.



  [1]: http://gitlab.itextsupport.com/itext7/samples/blob/develop/publications/book/src/test/java/com/itextpdf/samples/book/part1/chapter01/Listing_01_01_HelloWorld.java#L30
  [2]: https://github.com/itext/book/blob/develop/src/part1/chapter01/HelloWorld.java#L44

## Installation or Setup
# iText for Java

Importing the iText jars from the Central Maven Repository is the best way to install iText 7. These simple videos explain how to do this using different IDEs:

 * [How to import iText 7 in Eclipse to create a Hello World PDF?](https://www.youtube.com/watch?v=sxArv-GskLc&)
 * [How to import iText 7 in Netbeans to create a Hello World PDF?](https://www.youtube.com/watch?v=VcOi99zW7O4)
 * [How to import iText 7 in IntelliJ IDEA to create a Hello World PDF?](https://www.youtube.com/watch?v=6WxITuCgpHQ)

In these tutorials, we only define the `kernel` and the `layout` projects as dependencies. Maven also automatically imports the `io` jar because the `kernel` packages depend on the `io` packages.

This is the basic list of dependencies for standard use of iText 7:

    <dependencies>
        <dependency>
            <groupId>com.itextpdf</groupId>
            <artifactId>kernel</artifactId>
            <version>7.0.0</version>
            <scope>compile</scope>
        </dependency>
        <dependency>
            <groupId>com.itextpdf</groupId>
            <artifactId>io</artifactId>
            <version>7.0.0</version>
            <scope>compile</scope>
        </dependency>
        <dependency>
            <groupId>com.itextpdf</groupId>
            <artifactId>layout</artifactId>
            <version>7.0.0</version>
            <scope>compile</scope>
        </dependency>
        <dependency>
            <groupId>com.itextpdf</groupId>
            <artifactId>forms</artifactId>
            <version>7.0.0</version>
            <scope>compile</scope>
        </dependency>
        <dependency>
            <groupId>com.itextpdf</groupId>
            <artifactId>pdfa</artifactId>
            <version>7.0.0</version>
            <scope>compile</scope>
        </dependency>
        <dependency>
            <groupId>com.itextpdf</groupId>
            <artifactId>pdftest</artifactId>
            <version>7.0.0</version>
            <scope>compile</scope>
        </dependency>
        <dependency>
            <groupId>org.slf4j</groupId>
            <artifactId>slf4j-log4j12</artifactId>
            <version>1.7.18</version>
        </dependency>
    </dependencies>

Every dependency corresponds with a jar in Java and with a DLL in C#.

* `kernel` and `io`: contain low-level functionality.
* `layout`: contains high-level functionality.
* `forms`: needed for all the AcroForm examples.
* `pdfa`: needed for PDF/A-specific functionality.
* `pdftest`: needed for the examples that are also a test.

For more specific use of iText 7, you may need additional jars:

* `barcodes`: use this if you want to create bar codes.
* `hyph`: use this if you want text to be hyphenated.
* `font-asian`: use this is you need CJK functionality (Chinese / Japanese / Korean)
* `sign`: use this if you need support for digital signatures.

All the jars listed above are available under the AGPL license. You can also download these jars in a ZIP-file hosted on Github: https://github.com/itext/itext7/releases

If you want to use these jars, you have to add them to your CLASSPATH, just like you would add any other jar.

Additional iText 7 functionality is available through add-ons, which are delivered as jars under a commercial license. 
If you want to use any of these add-ons, or if you want to use iText 7 with your proprietary code, you need to obtain a commercial license key for iText 7 (see the [legal section of the iText web site](http://itextpdf.com/legal)).

You can import such a license key using the license-key module. You can get the license-key jar like this:

    <dependency>
        <groupId>com.itextpdf</groupId>
        <artifactId>itext-licensekey</artifactId>
        <version>2.0.0</version>
        <scope>compile</scope>
    </dependency>

Some functionality in iText is closed source. For instance, if you want to use **PdfCalligraph**, you need the `typography` module. This module won't work without an official license key.

# iText for C#

You can download a ZIP-file containing all the DLLs that are available under the AGPL. For more info about these DLLs, please read the Java documentation.

