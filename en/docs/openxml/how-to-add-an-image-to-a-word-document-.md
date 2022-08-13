---
title: "How to add an image to a Word Document."
slug: "how-to-add-an-image-to-a-word-document"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Inserting an image in a word document using OpenXml require two actions: add the image inside the openxml and refer to the image in your Document


if you only add the image to the openxml structure without refering it in the Word Document, the next time you "open / save" your document, the image file will be deleted.

Word delete all orphan references. So make sure to add the image in the Word Document otherwise you will need to redo all the steps.

## Add the image to the OpenXml structure
    private string AddGraph(WordprocessingDocument wpd, string filepath)
    {
        ImagePart ip = wpd.MainDocumentPart.AddImagePart(ImagePartType.Jpeg);
        using (FileStream fs = new FileStream(filepath, FileMode.Open))
        {
            if (fs.Length == 0) return string.Empty;
            ip.FeedData(fs);
        }

        return wpd.MainDocumentPart.GetIdOfPart(ip);
    }

In this case we use a FileStream to retrieve the image, but feedData(Stream) is waiting for any kind of Stream.

## Refer to the image in the Word Document
    private void InsertImage(WordprocessingDocument wpd, OpenXmlElement parent, string filepath)
    {
        string relationId = AddGraph(wpd, filepath);
        if (!string.IsNullOrEmpty(relationId))
        {
            Size size = new Size(800, 600);

            Int64Value width = size.Width * 9525;
            Int64Value height = size.Height * 9525;

            var draw = new Drawing(
                new DW.Inline(
                    new DW.Extent() { Cx = width, Cy = height },
                    new DW.EffectExtent()
                    {
                        LeftEdge = 0L,
                        TopEdge = 0L,
                        RightEdge = 0L,
                        BottomEdge = 0L
                    },
                    new DW.DocProperties()
                    {
                        Id = (UInt32Value)1U,
                        Name = "my image name"
                    },
                    new DW.NonVisualGraphicFrameDrawingProperties(new A.GraphicFrameLocks() { NoChangeAspect = true }),
                    new A.Graphic(
                        new A.GraphicData(
                            new PIC.Picture(
                                new PIC.NonVisualPictureProperties(
                                    new PIC.NonVisualDrawingProperties()
                                    {
                                        Id = (UInt32Value)0U,
                                        Name = relationId
                                    },
                                    new PIC.NonVisualPictureDrawingProperties()),
                                    new PIC.BlipFill(
                                        new A.Blip(
                                            new A.BlipExtensionList(
                                                new A.BlipExtension() { Uri = "{28A0092B-C50C-407E-A947-70E740481C1C}" })
                                            )
                                        {
                                            Embed = relationId,
                                            CompressionState =
                                            A.BlipCompressionValues.Print
                                        },
                                            new A.Stretch(
                                                new A.FillRectangle())),
                                                new PIC.ShapeProperties(
                                                    new A.Transform2D(
                                                        new A.Offset() { X = 0L, Y = 0L },
                                                        new A.Extents() { Cx = width, Cy = height }),
                                                        new A.PresetGeometry(new A.AdjustValueList()) { Preset = A.ShapeTypeValues.Rectangle }))) { Uri = "http://schemas.openxmlformats.org/drawingml/2006/picture" })
                                                        )
                {
                    DistanceFromTop = (UInt32Value)0U,
                    DistanceFromBottom = (UInt32Value)0U,
                    DistanceFromLeft = (UInt32Value)0U,
                    DistanceFromRight = (UInt32Value)0U,
                    EditId = "50D07946"
                });

            parent.Append(draw);
        }
    }

In this example I set a static size of 800*600 but you can set any size you need

