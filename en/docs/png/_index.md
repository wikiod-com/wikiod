---
title : png Tutorial
slug : png-tutorial
weight : 9992
draft : false
images : []
type : docs
---

**Portable Network Graphics** is a raster graphics file format that supports lossless data compression. PNG was created as an improved, non-patented replacement for Graphics Interchange Format (GIF) and is the most widely used lossless image compression format on the Internet.

PNG supports palette-based images (24-bit RGB or 32-bit RGBA colors), grayscale images (with or without alpha channel), and full-color, non-paletted RGB[A] images with an optional alpha channel. PNG was designed for transferring images on the Internet, not for professional-quality print graphics, and does not support non-RGB color spaces such as CMYK.

PNG files nearly always use file extension PNG or png and are assigned MIME media type image/png. PNG was approved for use by the Internet Engineering Steering Group on 14 October 1996, and was published as an ISO/IEC standard in 2004.

----------

For image editing, either professional or otherwise, PNG provides a useful format for the storage of intermediate stages of editing. PNG compression is fully lossless and supports up to 48-bit truecolor or 16-bit grayscal. Therefore saving, restoring, and re-saving an image will not degrade its quality unlike standard JPEG (even at highest quality JPEG settings). Unlike TIFF, the PNG specification leaves no room for implementors to pick and choose what features to support; a PNG image saved in one app is readable in any other PNG-supporting application.

Note that for transmission of finished truecolor images, especially photographic ones, JPEG is almost always a better choice. JPEG's lossy compression can introduce visible artifacts, but these can be minimized and the savings in file size even at high quality levels are much better than generally possible with a lossless format like PNG. For black-and-white images, particularly of text or drawings, TIFF's Group 4 fax compression or the JBIG format are often far better than 1-bit grayscale PNG.

----------

PNG's compression is among the best that can be had without losing image information and without paying patent fees. But not all implementations take full advantage of the available power. Even those that do can be thwarted by unwise choices on the part of the user.

