---
title: "Sending an email with SAS"
slug: "sending-an-email-with-sas"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

There are several reasons you might come across for needing email capabilities in SAS. You could be sending an email to notify someone that a process passed/failed, you could be sending an email containing Macro Variables that show how many records have been loaded at the end of your data feed, or maybe you need to send some files that contain reports. Whatever your need is, there are several ways to go about sending emails and files in SAS.

## Parameters
| Tag/Attribute  | Value   |
| -------------- | ------- |
| LRECL | This parameter is used to define record length when reading and writing files. I've solved many issues by just setting this to its max value, which is 32767. It's very possible that setting something like this to its max value is less efficient, but at the end of the day it gets the job done for me without any felt performance loss. (the range for LRECL is 1-32767)|

## Sending a basic text email with SAS
    Filename myEmail EMAIL
        Subject = "My Email Subject"
        From    = "myFromAddress@email.com"
        To      = 'toAddress@email.com'
        CC      = 'ccAddress@email.com'
        Type    = 'Text/Plain';
    
    
    Data _null_; File myEmail;
        PUT "Email content";
        PUT "&recordsCount loaded to your favorite table today!";
    RUN;



## Attaching an excel file to your SAS email
    Filename myEmail EMAIL
        Subject = "My Email Subject"
        From    = "myFromAddress@email.com"
        To      = 'toAddress@email.com'
        CC      = 'ccAddress@email.com'
        Type    = 'Text/Plain'
        ATTACH = ("my/excel/file/path/file.extension" content_type="application/vnd.ms-excel" LRECL= 32767);
    
    
    Data _null_; File myEmail;
        PUT "Email contentent";
        PUT "&recordsCount loaded to your favorite table today!";
    RUN;



## Sending a SAS email with an HTML body
> Take note of the email type: Type    = 'text/html';

    Filename myEmail EMAIL
        Subject = "My Email Subject"
        From    = "myFromAddress@email.com"
        To      = 'toAddress@email.com'
        CC      = 'ccAddress@email.com'
        Type    = 'text/html';

    Data _null_; File myEmail;
    PUT "
    <html>
        <head>
            <style>
                table, th, td {
                    border: 1px solid black;
                       border-collapse: collapse;
                }
            </style>
        </head>
        <body>
            <p>Here is your email</p>
            <p>Go ahead, organize your data within an HTML table tag here!</p>
            <table>
                <tr>
                    <th>
                        column 1
                    </th>
                    <th>
                        column 2
                    </th>
                </tr>
                <tr>
                    <td>
                        &countOfRecords1
                    </td>
                    <td>
                        &countOfRecords2
                    </td>
                </tr>
            </table>
        </body>
    </html>
    ";
    RUN;
    

>It is very possible that after building out an HTML email in SAS, you find the HTML is distorted when you receive the email. This is a result of SAS putting breaks to the next line in the text of your PUT. A break was probably placed right in the middle of one of your tag's text. *Should this happen to you, try moving your HTML tags around. It may not be pretty, but you may have to have some tags share a line to avoid this happening.* This happened to me, and this is exactly how I fixed that issues.

