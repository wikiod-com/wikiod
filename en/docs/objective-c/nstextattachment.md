---
title: "NSTextAttachment"
slug: "nstextattachment"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Syntax
 1. NSTextAttachment *attachmentName = [[NSTextAttachment alloc] init];

`NSTextAttachment` objects are used by the `NSAttributedString` class cluster as the values for attachment attributes. The objects you create with this class are referred to as text attachment objects, or when no confusion will result, as text attachments or merely attachments.

## NSTextAttachment Example
    NSTextAttachment *attachment = [[NSTextAttachment alloc] init];
    attachment.image = [UIImage imageNamed:@"imageName"];
    attachment.bounds = CGRectMake(0, 0, 35, 35);
    NSAttributedString *attachmentString = [NSAttributedString attributedStringWithAttachment:attachment];

