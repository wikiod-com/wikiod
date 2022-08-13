---
title: "Insert image into an inline inline  shape in word documents"
slug: "insert-image-into-an-inline-inline--shape-in-word-documents"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Insert an image into an MS Word document shapes such as Rectangles and ovals.

This documentation assumes you know how to insert an image into a word document, open and close a word document using OpenXML 

 

## Add the following OpenXML namespaces to your class
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using DocumentFormat.OpenXml;
    using A = DocumentFormat.OpenXml.Drawing;
    using DW = DocumentFormat.OpenXml.Drawing.Wordprocessing;
    using PIC = DocumentFormat.OpenXml.Drawing.Pictures;
    using DocumentFormat.OpenXml.Drawing.Wordprocessing;
    using Wps = DocumentFormat.OpenXml.Office2010.Word.DrawingShape;

## Open the document and add imagePart object to reference the picture you want to insert into the shape
Now open the document using OpenXML, you must add an imagePart that references the picture object to the MainDocumentPart object by using a file stream, and get the ID of the image

        string temp;
    MainDocumentPart mainPart = document.MainDocumentPart;
                                   ImagePart imagePart = mainPart.AddImagePart(ImagePartType.Bmp);
    
                                   using (FileStream stream = new FileStream(barcodepath, FileMode.Open))
                                   {
                                      imagePart.FeedData(stream);
                                   }
    
                                   temp = mainPart.GetIdOfPart(imagePart);



## Get a reference of a Blip object
In office OpenXML, a picture that is inserted into a word document is considered a "Blip" Object or element. The class is derived from the [DocumentFormat.OpenXml.Drawing][1] the Blip must have an Embed value that is an imagePart ID. The Blip object then goes inside a [BlipFill][2] Object/element, and that also goes inside a [graphicData][3] Object/element and that in turn goes into a [graphic][4] object element. I'm pretty sure by now you've realized everything works like an XML tree. Sample Open XML tree below.

    <a:graphic xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">
                           <a:graphicData uri="http://schemas.microsoft.com/office/word/2010/wordprocessingShape">
                             <wps:wsp>
                               <wps:cNvSpPr>
                                 <a:spLocks noChangeArrowheads="1" />
                               </wps:cNvSpPr>
                               <wps:spPr bwMode="auto">
                                 <a:xfrm>
                                   <a:off x="0" y="0" />
                                   <a:ext cx="1234440" cy="1234440" />
                                 </a:xfrm>
                                 <a:prstGeom prst="roundRect">
                                   <a:avLst>
                                     <a:gd name="adj" fmla="val 16667" />
                                   </a:avLst>
                                 </a:prstGeom>
                                 <a:blipFill dpi="0" rotWithShape="1">
                                   <a:blip r:embed="Raade88ffea8d4c1b" />
                                   <a:stretch>
                                     <a:fillRect l="10000" t="10000" r="10000" b="10000" />
                                   </a:stretch>
                                 </a:blipFill>
                               </wps:spPr>
                               <wps:bodyPr rot="0" vert="horz" wrap="square" lIns="91440" tIns="45720" rIns="91440" bIns="45720" anchor="t" anchorCtr="0" upright="1">
                                 <a:noAutofit />
                               </wps:bodyPr>
                             </wps:wsp>
                           </a:graphicData>
                         </a:graphic>





  [1]: https://msdn.microsoft.com/en-us/library/documentformat.openxml.drawing.blip_members(v=office.14).aspx
  [2]: https://msdn.microsoft.com/en-us/library/documentformat.openxml.drawing.chartdrawing(v=office.14).aspx
  [3]: https://msdn.microsoft.com/en-us/library/documentformat.openxml.drawing.graphicdata_members(v=office.14).aspx
  [4]: https://msdn.microsoft.com/en-us/library/documentformat.openxml.drawing.graphic_members(v=office.14).aspx

## Adding reference of the image the shapes in the template document.
Now you have a reference of the image. Insert the image into the shapes in the template document. To do this, you will have to use some LINQ to iterate through the document and get a reference to all the shapes in the document. The wps:spPr element you see in the above XML code is the xml element for the shapes in a document. The equivalent C# class is WordprocessingShape

    IEnumerable<DocumentFormat.OpenXml.Office2010.Word.DrawingShape.WordprocessingShape> shapes2 = document.MainDocumentPart.document.Body.Descendants<DocumentFormat.OpenXml.Office2010.Word.DrawingShape.WordprocessingShape>();



## With a collection reference of all the shapes, loop through the collection.
Now that you have a collection of all the shape references in the document. Loop through the collection with a foreach, and through each iteration  create a Blip object. Set the Blip object embed value to the picture ID reference you captured earlier from the image part. Also create a Stretch object, and FillRectangle object(these are not really necessary, they are just used them for proper alignment of the image). And append each to its parent object like the XML tree equivalent.

    foreach (DocumentFormat.OpenXml.Office2010.Word.DrawingShape.WordprocessingShape sp in shapes2)
                                  {
                                      //Wps.ShapeProperties shapeProperties1 = 
                                      A.BlipFill blipFill1 = new A.BlipFill() { Dpi = (UInt32Value)0U, RotateWithShape = true };
                                      A.Blip blip1 = new A.Blip() { Embed = temp };
    
                                      A.Stretch stretch1 = new A.Stretch();
                                      A.FillRectangle fillRectangle1 = new A.FillRectangle() { Left = 10000, Top = 10000, Right = 10000, Bottom = 10000 };
                                      Wps.WordprocessingShape wordprocessingShape1 = new Wps.WordprocessingShape();
    
    
    
                                          stretch1.Append(fillRectangle1);
                                          blipFill1.Append(blip1);
                                          blipFill1.Append(stretch1);
                                          Wps.ShapeProperties shapeProperties1 = sp.Descendants<Wps.ShapeProperties>().First();
                                          shapeProperties1.Append(blipFill1);
                                          
                                  }




