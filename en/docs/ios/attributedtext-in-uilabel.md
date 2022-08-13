---
title: "attributedText in UILabel"
slug: "attributedtext-in-uilabel"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

The current styled text that is displayed by the label.

You can add `HTML` text in `UILabel` using attributedText property or customized single `UILabel` text with different property

## HTML text in UILabel
    NSString * htmlString = @"<html><body> <b> Example bold text in HTML </b> </body></html>";
    NSAttributedString * attrStr = [[NSAttributedString alloc] initWithData:[htmlString dataUsingEncoding:NSUnicodeStringEncoding] options:@{ NSDocumentTypeDocumentAttribute: NSHTMLTextDocumentType } documentAttributes:nil error:nil];

    UILabel * yourLabel = [[UILabel alloc] init];
    yourLabel.attributedText = attrStr;

## Set different property to text in single UILabel
The first step you need to preform is to create a `NSMutableAttributedString` object. The reason we create a `NSMutableAttributedString` instead of `NSAttributedString` is because it enables us to append string to it.



    NSString *fullStr = @"Hello World!";
    NSMutableAttributedString *attString =[[NSMutableAttributedString alloc]initWithString:fullStr];
    
    // Finding the range of text.
    NSRange rangeHello = [fullStr rangeOfString:@"Hello"];
    NSRange rangeWorld = [fullStr rangeOfString:@"World!"];
    
    // Add font style for Hello
    [attString addAttribute: NSFontAttributeName
                      value: [UIFont fontWithName:@"Copperplate" size:14]
                      range: rangeHello];
    // Add text color for Hello
    [attString addAttribute: NSForegroundColorAttributeName
                      value: [UIColor blueColor]
                      range: rangeHello];
    
    // Add font style for World!
    [attString addAttribute: NSFontAttributeName
                      value: [UIFont fontWithName:@"Chalkduster" size:20]
                      range: rangeWorld];
    // Add text color for World!
    [attString addAttribute: NSForegroundColorAttributeName
                      value: [UIColor colorWithRed:(66.0/255.0) green:(244.0/255.0) blue:(197.0/255.0) alpha:1]
                      range: rangeWorld];
    
    // Set it to UILabel as attributedText
    UILabel * yourLabel = [[UILabel alloc] initWithFrame:CGRectMake(10, 150, 200, 100)];
    yourLabel.attributedText = attString;
    [self.view addSubview:yourLabel];

Output : 
 
[![Output][1]][1]


  [1]: https://i.stack.imgur.com/0sm4A.png

