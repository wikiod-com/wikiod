---
title: "Hybrid Email Coding"
slug: "hybrid-email-coding"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

When an email client doesn't support `media-queries`, a hybrid approach can be used to to reconfigure the layout for different screen sizes for email clients regardless of media query support. It uses fluid tables and max-width and min-width to impose rigid baselines (allowing some movement) and imposes a fixed, wide width for Outlook who is shackled to desktop anyway. Once a mobile-friendly baseline is set, media queries progressively enhance the email further in clients that support it.

## Single Column, Fluid Layout
The basic setup of hybrid coding is a single, fluid column. Since most email clients support `max-width`, we can use that to set the `<table>`'s width to 100% (fluid), but not exceed a max width (`660px` in this case). Just like on the web.

However, Windows Desktop Microsoft doesn't support `max-width` and would render this table at 100% width, so we use Microsoft conditional tags to set up "[Ghost Tables](https://www.codeschool.com/blog/2016/02/12/alternative-table-structure-for-flexible-html-emails/)" that give the `<table>` a hard width of `660px`. Windows Desktop Microsoft is desktop-only, so it needn't be fluid at small sizes. And since Windows Desktop Microsoft is the only email client that supports these Microsoft conditional tags, no other client will be inadvertently constrained to a `660px` desktop width.

    <table cellpadding="0" cellspacing="0" border="0" width="100%" style="">
        <tr>
            <td align="center" height="100%" valign="top" width="100%">
                <!--[if mso]>
                <table align="center" border="0" cellspacing="0" cellpadding="0" width="660">
                <tr>
                <td align="center" valign="top" width="660">
                <![endif]-->
                <table align="center" border="0" cellpadding="0" cellspacing="0" width="100%" style="max-width:660px;">
                    <tr>
                        <td align="center" valign="top">
                            <p>Content Goes Here.</p>
                        </td>
                    </tr>
                </table>
                <!--[if mso]>
                </td>
                </tr>
                </table>
                <![endif]-->
            </td>
        </tr>
    </table>

