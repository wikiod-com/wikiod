---
title: "Getting started with email"
slug: "getting-started-with-email"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## HTML and Plain Text
HTML email is the use of a subset of HTML and CSS to format an email message like a web page using colors, graphics, table columns and links.

When you send an email it’s important to send both plain text and HTML. You do this by sending your email as multi-part MIME. Most email service providers have tools to automatically construct the MIME for you. Some will generate a plain text version based on your HTML version, and there are [third party tools](https://templates.mailchimp.com/resources/html-to-text/) as well.

Example of a text-only email:

    # EMAIL TITLE GOES HERE

    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi commodo elit sed quam auctor, ut facilisis odio aliquam. In hac habitasse platea dictumst.

    Learn more: http://www.website.com/landing-page-1

    ---

    ## Subheader goes here

    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi commodo elit sed quam auctor, ut facilisis odio aliquam. In hac habitasse platea dictumst.

    Learn more: http://www.website.com/landing-page-2

    ---

    Footer and Sender information

    Unsubscribe here: http://www.website.com/unsubscribe


[![enter image description here][1]][1]

Note that with the recent launch of Apple Watch, a new MIME part has surfaced: `text/watch-html`. This content will only be displayed in the Apple Watch (and any other clients that support this MIME type).


  [1]: https://i.stack.imgur.com/bTE1s.png

## Email Clients and Rendering Engines
Email clients use different rendering engines to render HTML emails:

* Apple Mail, Outlook for Mac, Android Mail and iOS Mail use WebKit
* Outlook 2000/02/03 use Internet Explorer 6
* Outlook 2007/10/13 use Microsoft Word
* Web clients use their browser’s respective engine (e.g. Safari uses WebKit, Chrome uses Blink)

Some clients will also add their own styles on top of yours e.g. Gmail sets all `<td>` fonts to `font-family: arial,sans-serif;`, which could impact how an email renders.

Prior to 2016, all Gmail products stripped out `<link>` tags, any CSS inside `<style>` tags, and any other CSS that isn’t inlined. In late 2016, Gmail began supporting the `<style>` tag and media-queries in most of its products. [Here is an update as of 23-Nov-2016](https://litmus.com/blog/gmail-to-support-responsive-email-design?utm_campaign=podcast_40&utm_source=litmusblog&utm_medium=blog):

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/T4FVl.png

## The Basics (Tables and Inline CSS)
***
**Tables for Layout**

The structure of an HTML email file is similar to that of a web page:

    <!DOCTYPE html>
    <html lang="en">
        <head>
        <meta charset="UTF-8">
            <title>Hello!</title>
        </head>
        <body>
            <h1>Hello World!</h1>
            <p>This is a simple paragraph.</p>
        </body>
    </html>

However a `<div>`-based CSS layout doesn’t work in email as it does on the web. Major email clients either offer no support for things like floats and flexbox, or mangle it in different ways. `<div>`s also have positioning and box model issues in different clients, particularly Outlook. There are a [few](https://medium.freecodecamp.com/the-fab-four-technique-to-create-responsive-emails-without-media-queries-baf11fdfa848#.sr6gvlk7r) [techniques](https://medium.com/cm-engineering/coding-mobile-first-emails-1513ac4673e#.wvj401pe5) to code an email using only `<div>`s, but it’s safer to stick with tables for layout.

    <!DOCTYPE html>
    <html lang="en">
        <head>
            <meta charset="UTF-8">
            <title>Hello!</title>
        </head>
        <body>
            <table cellspacing="0" cellpadding="0" border="0" width=”600” role=”presentation”>
                <tr>
                    <td width="200">
                        <h1>Hello World!</h1>
                    </td>
                    <td width="400">
                        <p>This is a simple paragraph.</p>
                    </td>
                </tr>
            </table>
        </body>
    </html>

**Inline CSS**

Applying a style inline gives it priority over styles further away (such as webmail client styles), and also works around the email clients that strip out CSS from the head or external CSS files. Inline CSS is the best way to ensure consistent display in every email client.

    <!DOCTYPE html>
    <html lang="en">
        <head>
            <meta charset="UTF-8">
            <title>Hello!</title>
        </head>
        <body style="background: #000000;">
            <table cellspacing="0" cellpadding="0" border="0" width=”600” role=”presentation” style="margin: auto; background: #ffffff;">
                <tr>
                    <td width="200" style="padding: 20px; background-color: #eeeeee;">
                        <h1 style="font-weight: bold; color: #444444; font-family: Georgia, serif;">Hello World!</h1>
                    </td>
                    <td width="400" style="padding: 20px;">
                        <p style="color: #444444; font-family: arial, sans-serif; margin: 0;">This is a simple paragraph.</p>
                    </td>
                </tr>
            </table>
        </body>
    </html>

You have a couple of options for inlining CSS:

* Write CSS inline as you go
* Use local [snippets](http://www.hongkiat.com/blog/sublime-code-snippets/) in your editor
* Use a web-based CSS inliner [like this one](https://inliner.cm/)
* Use a programmatic CSS inliner [like this one](https://github.com/premailer/premailer)
* Use a build process [like this one](https://github.com/leemunroe/grunt-email-workflow) or a templating language
* Let your email service provider handle the inlining for you (if they support it)

## Coding emails for all email clients
Coding method used: Hybrid/Spongy

It has been forever a myth that div's can not be used in emails. There are email clients (unlike outlook) that can render div's properly. The example below will illustrate how an email can be coded that will work on Gmail app (with updates not rolled out yet), Samsung device's and other email clients that don't read media queries.

**Introducing Outlook conditional statements**

Outlook conditional statements are very useful when it come to rendering emails or showing something specific like fall backs for outlook.

    <!--[if (gte mso 9)|(IE)]>
    <![endif]-->

The above code reads *if greater than microsoft outlook 9 or IE*

    
    Outlook 2000 - Version 9
    Outlook 2002 - Version 10
    Outlook 2003 - Version 11
    Outlook 2007 - Version 12
    Outlook 2010 - Version 14
    Outlook 2013 - Version 15

Versions for conditional statements listed.

**Starting a hybrid email template**

Each step has been explained in a way that it should be easy for anyone with basic HTML knowledge to understand.

First we start with a wrapper table which will span all the way across the screen and with a class of container.

    <table width="100%" border="0" cellspacing="0" cellpadding="0">
      <tbody>
        <tr>
          <td>[CONTENT GOES HERE]</td>
        </tr>
      </tbody>
    </table>

After that we add in a media query for email clients that dont read max width but read the CSS in the header. The media query will be targeting all screens and showing the container at 700 pixels width.

    @media only screen and (max-width : 700px) {
    .container{width:700px;}
    }


Next we add an outlook conditional statement that keeps the table (with class container) to be at a width of 700 pixels. 

    

    <!--[if (gte mso 9)|(IE)]>
        <table width="700" align="center" cellpadding="0" cellspacing="0" border="0">
            <tr>
                <td align="left" valign="top" width="700">
        <![endif]-->
        
            <table class="container" width="100%" border="0" cellspacing="0" cellpadding="0" style="width: 100%; max-width: 700px;">
                  <tbody>
                    <tr>
                      <td valign="top" bgcolor="#FFFFFF" style="padding:0px;text-align: center; vertical-align: top; font-size: 0px;">[CONTENT GOES HERE]</td>
                    </tr>
                  </tbody>
            </table>
    
        <!--[if (gte mso 9)|(IE)]>
                </td>
            </tr>
        </table>
    <![endif]-->


The above code should now hold your template in outlook at 700px wide. 

Now for the columns. Below code will create two equal columns on the template both at 350px wide.

    <!--[if (gte mso 9)|(IE)]>
    <table width="700" align="center" cellpadding="0" cellspacing="0" border="0">
        <tr>
            <td align="left" valign="top" width="350">
    <![endif]-->
    <div style="width: 350px; display: inline-block; vertical-align: top;">
    <table width="100%" border="0" cellspacing="0" cellpadding="0">
      <tbody>
        <tr>
          <td>[COLUMN CONTENT HERE]</td>
        </tr>
      </tbody>
    </table>
    </div>
                                      
    <!--[if (gte mso 9)|(IE)]>
    </td><td align="left" valign="top" width="300">
    <![endif]-->
    
    <div style="width: 350px; display: inline-block; vertical-align: top;">
    <table width="100%" border="0" cellspacing="0" cellpadding="0">
      <tbody>
        <tr>
          <td>[COLUMN CONTENT HERE]</td>
        </tr>
      </tbody>
    </table>
    </div>
    
     <!--[if (gte mso 9)|(IE)]>
            </td>
        </tr>
    </table>
    <![endif]-->

After this the limit is only your imagination or the designer. When designs are done it is important to be involved in the wire framing stage so there are no suprises once the design is in coding stage.

**Note:**

* Retina images will need images set at the element level not on its style <img src="" width="" ...
* You can still choose to do in-line CSS or you can choose to CSS in head only **if** all your clients support CSS in the head.



