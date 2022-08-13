---
title: "Meta Information"
slug: "meta-information"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

As with web development, `<meta>` tags can be included in the `<head>` tag of an email. `<meta>` tags tell emails clients how to interpret and display email code. These are `<meta>` tags that are regularly used in email.

## Content-Type
The `Content-Type` meta tag is for telling the destination rendering engine how to process text and special characters. You should encode all special characters anyway (e.g., `&` becomes `&amp;` for an ampersand) to be safe, but it’s worth keeping this line in there anyway.

`utf-8` works for most cases

    <head>
        <meta charset="utf-8">
    </head>

or 

    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    </head>

depending on your `doctype`.

## Viewport
The `viewport` meta tag tells a device to set the viewable area to the width of the device screen. It also sets the initial scale to **normal** (not zoomed in or out).

    <meta name="viewport" content="width=device-width">

or

    <meta name="viewport" content="width=device-width, initial-scale=1.0">

Though forcing initial-scale shouldn't be necessary.

If this is not specified, many smartphones may scale the content down so its content fits within the viewable area, but not any of its padding or margins. This can result in text and images butting right up against the edge of the screen.

## Display EdgeHTML mode
The `IE=Edge` meta tag is for telling the destination rendering engine to use the latest (edge) version of IE rendering engine, which enables responsive behavior in Windows phones.

    <meta http-equiv="X-UA-Compatible" content="IE=edge">

Occasionally this breaks images in Live Mail, so since the `IE=Edge` tag is specific to Microsoft, it can be wrapped in a conditional comment to hide it from Live Mail: 

    <!--[if !mso]><!-- -->
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <!--<![endif]-->

## Disable auto-scale in iOS 10 Mail
Apple iOS 10 Mail [doesn't always auto-scale non-responsive emails](https://github.com/hteumeuleu/email-bugs/issues/18). The `auto-scale` meta tag can be used to disable auto-scale feature in iOS 10 Mail entirely.

    <meta name="x-apple-disable-message-reformatting">


