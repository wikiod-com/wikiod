---
title: "HTML email composition and Outlook intricacies"
slug: "html-email-composition-and-outlook-intricacies"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Creating spacers
Some email clients (notably Windows Desktop Outlook) collapse tables with no content, even if there are dimensions or padding.

To prevent these clients from collapsing the spacer table, you can add an invisible `&nbsp;` that serves as content. Then zero out the character's `font-size` and `line-height` so it doesn't impact the `<table>`s dimensions. The height (or width) attribute is now the only thing affecting the spacer's dimensions.

    <table border="0" cellpadding="0" cellspacing="0" role="presentation">
        <tr>
            <td height="40" style="font-size: 0; line-height: 0;">
                &nbsp;
            </td>
        </tr>
    </table>

It _was_ common practice to use a 1x1 transparent `.gif` as a spacer, but this method is deprecated.

## Background images
# Full width table cell background images

    <html xmlns:v="urn:schemas-microsoft-com:vml">
        <head>
        <style>
        v:* { behavior: url(#default#VML); display: inline-block; }
        </style>
        </head>
        <body>
            <center>
            <table width="100%" height="20">
                <tr>
                    <td bgcolor="#dddddd" style="background-image:url('http://placekitten.com/g/500/300');background-repeat:no-repeat;background-position:center;" background="http://placekitten.com/g/500/300" width="100%" height="300">
                    <!--[if gte mso 9]>
                        <v:rect xmlns:v="urn:schemas-microsoft-com:vml" fill="true" stroke="false" style="mso-width-percent:1000;height:300px;">
                            <v:fill type="frame" src="http://placekitten.com/g/500/300" color="#ffffff" />
                        </v:rect>
                    <![endif]-->
                    </td>
                </tr>
                <tr>
                    <td bgcolor="#33cc99">
                        <table border="0" cellpadding="5" cellspacing="0"><tr><td height="5"><table border="0" cellpadding="0" cellspacing="0"><tr><td></td></tr></table></td></tr></table>
                    </td>
                </tr>
                <tr>
                    <td bgcolor="#ffff99" style="background-image:url('http://placekitten.com/g/500/300');background-repeat:no-repeat;background-position:center;" background="http://placekitten.com/g/500/300" width="100%" height="300">
                        <!--[if gte mso 9]>
                            <v:rect xmlns:v="urn:schemas-microsoft-com:vml" fill="true" stroke="false" style="mso-width-percent:1000;height:300px;">
                                <v:fill type="frame" src="http://placekitten.com/g/500/300" color="#ffffff" />
                            </v:rect>
                        <![endif]-->
                    </td>
                </tr>
            </table>
            </center>
        </body>
    </html>

 - Tested in Outlook 2010 thru Windows 7 VMWare on OSX 10.9.2

![enter image description here][1]

---

## Tiled full width table cell background images

        <html xmlns:v="urn:schemas-microsoft-com:vml">
            <head>
                <style>
                    v:* { behavior: url(#default#VML); display: inline-block; }
                </style>
            </head>

            <body>
    
            <center>
            <table width="100%">
                <tr>
                    <td bgcolor="#dddddd" style="background-image:url('http://placekitten.com/g/500/300');background-repeat:no-repeat;background-position:center;" background="http://placekitten.com/g/500/300" width="100%" height="300">
                        <!--[if gte mso 9]>
                            <v:rect xmlns:v="urn:schemas-microsoft-com:vml" fill="true" stroke="false" style="mso-width-percent:1000;height:300px;">
                                <v:fill type="tile" src="http://placekitten.com/g/500/300" color="#ffffff" />
                            </v:rect>
                        <![endif]-->
                    </td>
                </tr>
            </table>
            </center>

            </body>
        </html>

 - Tested in Outlook 2010 thru Windows 7 VMWare on OSX 10.9.2

![enter image description here][2]

---

## Specified width table cell background images

Here is an example, two rows, the first row has 3 columns with 3 separate background images, the second row spans all the way across as one background image.

        <table width="600" border="0" cellpadding="0" cellspacing="0" style="border-collapse: collapse;">
            <tr>
                <td style="width: 300px; height: 80px; background-image: url('http://placekitten.com/g/300/80');">
                <!--[if gte mso 9]>
                    <v:image xmlns:v="urn:schemas-microsoft-com:vml" id="theImage" style='behavior: url(#default#VML); display: inline-block; position: absolute; width: 300px; height: 80px; top: 0; left: 0; border: 0; z-index: 1;' src="http://placekitten.com/g/300/80" />
                    <v:shape xmlns:v="urn:schemas-microsoft-com:vml" id="theText" style='behavior: url(#default#VML); display: inline-block; position: absolute; width: 300px; height: 80px; top: -5; left: -10; border: 0; z-index: 2;'>
                    <div>
                <![endif]-->
                    <table width="300" border="0" cellspacing="0" cellpadding="0" style="border-collapse: collapse;">
                        <tr>
                            <td height="80" align="center" valign="top" style="color:#ffffff;font-size:20px;">
                                <span>Text</span>
                            </td>
                        </tr>
                    </table>
                <!--[if gte mso 9]>
                    </div>
                    </v:shape>
                <![endif]-->
                </td>
                <td style="width: 100px; height: 80px; background-image: url('http://placekitten.com/g/100/80');">
                <!--[if gte mso 9]>
                    <v:image xmlns:v="urn:schemas-microsoft-com:vml" id="theImage" style='behavior: url(#default#VML); display: inline-block; position: absolute; width: 100px; height: 80px; top: 0; left: 0; border: 0; z-index: 1;' src="http://placekitten.com/g/100/80" />
                    <v:shape xmlns:v="urn:schemas-microsoft-com:vml" id="theText" style='behavior: url(#default#VML); display: inline-block; position: absolute; width: 100px; height: 80px; top: -5; left: -10; border: 0; z-index: 2;'>
                    <div>
                <![endif]-->
                    <table width="80" border="0" cellspacing="0" cellpadding="0" style="border-collapse: collapse;">
                        <tr>
                            <td height="80" align="center" valign="top">
                                <span>Text</span>
                            </td>
                        </tr>
                    </table>
                <!--[if gte mso 9]>
                    </div>
                    </v:shape>
                <![endif]-->
                </td>
                <td style="width: 200px; height: 80px; background-image: url('http://placekitten.com/g/200/100');">
                <!--[if gte mso 9]>
                    <v:image xmlns:v="urn:schemas-microsoft-com:vml" id="theImage" style='behavior: url(#default#VML); display: inline-block; position: absolute; width: 200px; height: 80px; top: 0; left: 0; border: 0; z-index: 1;' src="http://placekitten.com/g/200/100" />
                    <v:shape xmlns:v="urn:schemas-microsoft-com:vml" id="theText" style='behavior: url(#default#VML); display: inline-block; position: absolute; width: 200px; height: 80px; top: -5; left: -10; border: 0; z-index: 2;'>
                    <div>
                <![endif]-->
                    <table width="200" border="0" cellspacing="0" cellpadding="0" style="border-collapse: collapse;">
                        <tr>
                            <td height="80" align="center" valign="top" style="color:#ffffff;font-size:20px;">
                                <span>Text</span>
                            </td>
                        </tr>
                    </table>
                <!--[if gte mso 9]>
                    </div>
                    </v:shape>
                <![endif]-->
                </td>
            </tr>
        </table>
        <table width="600" border="0" cellpadding="0" cellspacing="0" style="border-collapse: collapse;">
            <tr>
                <td style="width: 600px; height: 150px; background-image: url('http://placekitten.com/g/600/150');">
                <!--[if gte mso 9]>
                    <v:image xmlns:v="urn:schemas-microsoft-com:vml" id="theImage" style='behavior: url(#default#VML); display: inline-block; position: absolute; width: 600px; height: 150px; top: 0; left: 0; border: 0; z-index: 1;' src="http://placekitten.com/g/600/150" />
                    <v:shape xmlns:v="urn:schemas-microsoft-com:vml" id="theText" style='behavior: url(#default#VML); display: inline-block; position: absolute; width: 600px; height: 150px; top: -5; left: -10; border: 0; z-index: 2;'>
                    <div>
                <![endif]-->
                    <table width="600" border="0" cellspacing="0" cellpadding="0" style="border-collapse: collapse;">
                        <tr>
                            <td height="150" align="center" valign="top" style="color:#ffffff;font-size:20px;">
                                <span>Text</span>
                            </td>
                        </tr>
                    </table>
                <!--[if gte mso 9]>
                    </div>
                    </v:shape>
                <![endif]-->
                </td>
            </tr>
        </table>

 - Tested in Outlook 2010 thru Windows 7 VMWare on OSX 10.9.2

![enter image description here][3]


  [1]: http://i.stack.imgur.com/Mtjm8.jpg
  [2]: http://i.stack.imgur.com/uvgFK.png
  [3]: http://i.stack.imgur.com/pNHgr.png

## <table> Element Spacing
Outlook can sometimes add a bit of spacing on the left and right side of a <table> element that can cause some layout-related headaches. By using the vendor-specific mso-table-lspace and mso-table-rspace CSS properties, you can be rid of those spaces and continue on to tackle the million other problems caused by Outlook.

    table{
        mso-table-lspace:0pt;
        mso-table-rspace:0pt;
    }



## Image Resizing
Using width or height tags to resize images in your markup can create a problem in Internet Explorer browsers. If your reader is viewing an email in-browser, and that email happens to have fluid images in it, they’ll look pretty ugly as they resize. Using -ms-interpolation-mode:bicubic; ensures that your images look a little better.

    img{
        -ms-interpolation-mode:bicubic;
    }

