---
title: "HTML Email Reset"
slug: "html-email-reset"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

As with web development, it's good to provide some reset CSS to the emails. As when building a web page, adding a CSS reset to an email helps to normalize how the code gets rendered across multiple email clients (since each has it's own default styles and way of interpreting code).

## Resetting Email CSS in <style> Tag
CSS resets for email are either **general resets** or **client-specific resets**. General resets give all email clients and browsers a common base to work on. Client-specific resets target some of the quirks inherent in select email clients.

A CSS reset like the one below can be placed in an email's `<style>` tag and _does not_ have to be inlined.

    <!DOCTYPE html>
    <html lang="en">
        <head>
        <meta charset="UTF-8">
            <title>Hello!</title>

            <style>
                /* Remove spaces around the email design added by some email clients. */
                html,
                body {
                    margin: 0 auto !important;
                    padding: 0 !important;
                    height: 100% !important;
                    width: 100% !important;
                }
                
                /* Stops email clients resizing small text. */
                * {
                    -ms-text-size-adjust: 100%;
                    -webkit-text-size-adjust: 100%;
                }
                
                /* What is does: Centers email on Android 4.4 */
                div[style*="margin: 16px 0"] {
                    margin:0 !important;
                }
                
                /* Stops Outlook from adding extra spacing to tables. */
                table,
                td {
                    mso-table-lspace: 0pt !important;
                    mso-table-rspace: 0pt !important;
                }
                        
                /* Fixes webkit padding issue. Fix for Yahoo mail table alignment bug. Applies table-layout to the first 2 tables then removes for anything nested deeper. */
                table {
                    border-spacing: 0 !important;
                    border-collapse: collapse !important;
                    table-layout: fixed !important;
                    margin: 0 auto !important;
                }
                table table table {
                    table-layout: auto; 
                }
                
                /* Uses a better rendering method when resizing images in IE. */
                img {
                    -ms-interpolation-mode:bicubic;
                }
                
                /* A work-around for iOS meddling in triggered links. */
                .mobile-link--footer a,
                a[x-apple-data-detectors] {
                    color:inherit !important;
                    text-decoration: underline !important;
                }

            </style>

        </head>
        <body>
            <!-- Email Content -->
        </body>
    </html>


