---
title: "Bulletproof Buttons"
slug: "bulletproof-buttons"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Buttons can be one of the most frustrating parts of email development, but they _can_ be built with code instead of images and still display well in all major email clients. When built in HTML/CSS, a button will display in all email clients even with images off. Both the content and style of each button can be quickly edited in HTML/CSS template.

**Progressive Enhancement with Media Queries**

For clients that support media queries, any of the examples above can be progressively enhance the button.

Eg.
    `@media screen and (-webkit-min-device-pixel-ratio: 0) { /* Insert -webkit-targeted CSS here */ }`

---

**Add more horizontal padding in Outlook**

Conditionally adding non-breaking space on each side of a button link can increase the horizontal padding for Outlook. Especially useful for border-based buttons.

    `<!--[if mso]>&nbsp;&nbsp;&nbsp;<![endif]-->Link Text<!--[if mso]>&nbsp;&nbsp;&nbsp;<![endif]-->`


## Buttons.cm Button
Campaign Monitor built [a nifty tool](https://buttons.cm/) that generates code for buttons in HTML email. It uses a cominbation os CSS and VML (for Microsoft Outlook) to display background images or patterns in most major email clients.

    <div><!--[if mso]>
      <v:roundrect xmlns:v="urn:schemas-microsoft-com:vml" xmlns:w="urn:schemas-microsoft-com:office:word" href="http://www.stackoverflow.com" style="height:38px;v-text-anchor:middle;width:200px;" arcsize="8%" strokecolor="#7ea613" fill="t">
        <v:fill type="tile" src=""https://imgur.com/clZqdfM.gif"" color="#99c739" />
        <w:anchorlock/>
        <center style="color:#ffffff;font-family:sans-serif;font-size:13px;font-weight:bold;">Stack Overflow</center>
      </v:roundrect>
    <![endif]--><a href="http://www.stackoverflow.com"
    style="background-color:#99c739;background-image:url("https://imgur.com/clZqdfM.gif");border:1px solid #7ea613;border-radius:3px;color:#ffffff;display:inline-block;font-family:sans-serif;font-size:13px;font-weight:bold;line-height:38px;text-align:center;text-decoration:none;width:200px;-webkit-text-size-adjust:none;mso-hide:all;">Stack Overflow</a></div>

Will display something like this:
[![enter image description here][1]][1]

***

**note**: Since the clickable hyperlink in Outlook is inside a `VML` tag, some email service providers have trouble tracking clicks on this link.

    <v:roundrect xmlns:v="urn:schemas-microsoft-com:vml" xmlns:w="urn:schemas-microsoft-com:office:word" href="http://www.stackoverflow.com" style="height:38px;v-text-anchor:middle;width:200px;" arcsize="8%" strokecolor="#7ea613" fill="t">


  [1]: https://i.stack.imgur.com/Nr2zo.png

## Padding-Based Buttons
This technique relies on padding at the table cell level to structure the button, and both HTML attributes and CSS to style the button. No `VML` means simpler code, but no background images in Outlook.

    <table width="100%" border="0" cellspacing="0" cellpadding="0">
      <tr>
        <td>
          <table border="0" cellspacing="0" cellpadding="0">
            <tr>
              <td bgcolor="#0095FF" style="padding: 12px 18px 12px 18px; -webkit-border-radius:3px; border-radius:3px" align="center"><a href="http://www.stackoverflow.com" target="_blank" style="font-size: 14px; font-family: sans-serif; font-weight: normal; color: #ffffff; text-decoration: none; display: inline-block;">Stack Overflow</a></td>
            </tr>
          </table>
        </td>
      </tr>
    </table>

[![enter image description here][1]][1]

The drawback of padding-based-buttons is that the entire area of the button is not clickable, only the link within the table cell is.


  [1]: https://i.stack.imgur.com/lVUYu.jpg

## Border-Based Buttons
This technique relies on adding thick borders to the link itself to build the button's CTA. Using borders is universally understood by email clients, but limit button appearance solid colors.

    <table width="100%" border="0" cellspacing="0" cellpadding="0">
      <tr>
        <td>
          <table border="0" cellspacing="0" cellpadding="0">
            <tr>
              <td bgcolor="#0095FF" style="padding: 12px 18px 12px 18px; -webkit-border-radius:3px; border-radius:3px" align="center"><a href="http://www.stackoverflow.com" target="_blank" style="font-size: 14px; font-family: sans-serif; font-weight: normal; color: #ffffff; text-decoration: none; display: inline-block;">Stack Overflow</a></td>
            </tr>
          </table>
        </td>
      </tr>
    </table>

[![enter image description here][1]][1]

The link tag is set to be a block-level element and that borders are used to provide the padding, so the entire button is hoverable + clickable, even in older desktop clients.

---

The drawback of border-based-buttons is that Outlook will reduce the size of the borders by a small amount and, as mentioned above, background images are not supported.


  [1]: https://i.stack.imgur.com/bE0S2.jpg

## Padding + Border-Based Buttons
This technique uses a combination of border-based and padding-based buttons, styling the link with both padding and at least a solid 1px border. The background color needs to be applied to the <td> instead of the <a> in this instance because Outlook does recognize horizontal padding on the <a> tag (since it's not a block level tag).

    <table width="100%" border="0" cellspacing="0" cellpadding="0">
      <tr>
        <td>
          <table border="0" cellspacing="0" cellpadding="0">
            <tr>
              <td align="center" style="-webkit-border-radius: 3px; -moz-border-radius: 3px; border-radius: 3px;" bgcolor="#0095FF"><a href="https://www.stackoverflow.com" target="_blank" style="font-size: 14px; font-family: sans-serif; color: #ffffff; text-decoration: none; text-decoration: none; -webkit-border-radius: 3px; -moz-border-radius: 3px; border-radius: 3px; padding: 12px 18px; border: 1px solid #0095FF; display: inline-block;">Stack Overflow</a></td>
            </tr>
          </table>
        </td>
      </tr>
    </table>

[![enter image description here][1]][1]

---

The drawback of padding+border-based-buttons is that it separates the styling between a `<td>` and `<a>` tag, so it can be difficult to maintain. But only one button is used, specific dimensions are not required, and background images can be supported.


  [1]: https://i.stack.imgur.com/mAB7b.jpg

## Bullet-proof button with spacers
This button derives from [this example](https://www.emailonacid.com/blog/article/email-development/simple-code-based-bulletproof-button) by Email on Acid. It is entirely code-based, so it will display without images downloaded, and the entire button is hoverable + clickable.

Additionally, this example also includes spacers to help control how much vertical space appears before and after the button.

    <table width="100%">
        <tr>
          <td>
            <!-- Top Spacer : BEGIN -->
            <table border="0" cellpadding="0" cellspacing="0"> 
              <tr>
                <td height="20" width="100%" style="font-size: 20px; line-height: 20px;">
                &nbsp;
                </td>
              </tr>
            </table>
            <!-- Top Spacer : END -->
            <!-- Button : BEGIN -->
            <table border="0" align="left" cellpadding="0" cellspacing="0">
              <tbody>
              <tr>
                <td align="left">
                  <table border="0" cellpadding="0" cellspacing="0" width="150">
                    <tr>
                      <td align="center" bgcolor="#0077CC" width="150" style="-moz-border-radius: 4px; -webkit-border-radius: 4px; border-radius: 4px;">
                        <a href="http://www.stackoverflow.com" style="padding: 10px; width:150px; display: block;text-decoration: none; border:0;text-align: center; font-weight: bold; font-size: 14px; font-family: sans-serif; color: #ffffff; background: #0095FF;border: 1px solid #0077CC; -moz-border-radius: 4px; -webkit-border-radius: 4px; border-radius: 4px; line-height:17px;" class="button_link">Stack Overflow</a>
                      </td>
                    </tr>
                  </table>
                </td>
              </tr>
              </tbody>
            </table>
            <!-- Button : END -->
            <!-- Bottom Spacer : BEGIN -->
            <table border="0" cellpadding="0" cellspacing="0"> 
              <tr>
                <td height="20" width="100%" style="font-size: 20px; line-height: 20px;">
                &nbsp;
                </td>
              </tr>
            </table>
            <!-- Bottom Spacer : END -->
          </td>
        </tr>
      </table>

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/4KXdA.jpg

