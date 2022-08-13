---
title: "Getting started with outlook-vba"
slug: "getting-started-with-outlook-vba"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction
There are currently three topics introducing Outlook VBA and at least three more are planned.

Part 1 describes how to get access to the Visual Basic Editor.

If you are a user of Outlook 2003 and a user of Excel VBA, you will learn little for this part since accessing the Outlook Visual Basic Editor is the same as accessing the Excel Visual Basic Editor.

With Outlook 2007 and later, the <kbd>Developer</kbd> tab. which gives access to the Visual Basic Editor, is not displayed for a new installation.  To display the <kbd>Developer</kbd> tab, you must perform an number of steps which are described in this part. There is no code in this part.

Parts 2 and 3 describe stores and folders which are where Outlook stores data.  You could think of them as the equivalent of Excel's workbooks and worksheets.  The division between part 2 and 3 is somewhat arbitrary. Part 2 describes stores and folders and includes macros to display the names of all accessible stores and the top level folders within those stores.  Part 3 includes macro for accessing lower level folders.  One pair of macros uses recursion which a new programmer may find difficult to understand.  The reader should aim to understand all the code in Part 2. It would however be legitimate to understand what that pair of macros does but not understand how they achieve their objective.

Part 4, the next part to be written, will introduce `MailItems` which hold emails. Part 3 includes a macro to move a folder from one parent to another but most macros operate on the objects contained within folders not folders themselves. Judging from the questions on Stack overflow, `MailItems` are of most interest to programmers.

Part 5 will introduce `CalendarItems` which hold appointments.  Part 6 will introduce the creation of new Excel workbooks from Outlook and the reading and updating of existing workbooks.  Part 7 will introduce Events unless some more immediately important topic is identified.

It is important to understand this is an introduction to Outlook VBA not an introduction to VBA. Part 2 gives some guidance on where to get information on VBA but since the language is the same across all Office products, a description of it belongs outside this introduction to Outlook VBA. 

## Outlook Visual Basic for Applications
Visual Basic for Applications (VBA) is the macro language behind all Microsoft Office products and is essentially identical across all Office products.  What differs from product to product is the Object model.  Excel has workbooks, worksheets and cells.  Access has tables and attributes.  Outlook has folders, emails and appointments.  It is the Object Model that makes Excel VBA different from Outlook VBA.


## Advanced topics
The various parts of the introduction aim to give the information that any programmer new to Outlook VBA would need. Much of the code was originally developed with Outlook 2003 and has been tested with Outlook 2016.  It should work unchanged with any intermediate version.

New functionality has been introduced since Outlook 2003 which programmers will wish/need to access. It is envisaged that "advanced topics" will be written to describe this functionality.

 

