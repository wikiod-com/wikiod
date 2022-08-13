---
title: "UILabel text underlining"
slug: "uilabel-text-underlining"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Underlining a text in a UILabel using Objective C

 

    UILabel *label=[[UILabel alloc]initWithFrame:CGRectMake(0, 0, 320, 480)];
    label.backgroundColor=[UIColor lightGrayColor];
    NSMutableAttributedString *attributedString;
    attributedString = [[NSMutableAttributedString alloc] initWithString:@"Apply Underlining"];
    [attributedString addAttribute:NSUnderlineStyleAttributeName value:@1 range:NSMakeRange(0, [attributedString length])];
    [label setAttributedText:attributedString];




## Underlining a text in UILabel using Swift
     let label = UILabel.init(frame: CGRect(x: 0, y:0, width: 100, height: 40))
     label.backgroundColor = .lightGray
     let attributedString = NSMutableAttributedString.init(string: "Apply UnderLining")
     attributedString.addAttribute(NSUnderlineStyleAttributeName, value: 1, range: NSRange.init(location: 0, length: attributedString.length))
     label.attributedText = attributedString

