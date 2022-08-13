---
title: "MyLayout"
slug: "mylayout"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

MyLayout is a simple and easy objective-c framework for iOS view layout. MyLayout provides some simple functions to build a variety of complex interface. It integrates the functions including: Autolayout and SizeClass of iOS, five layout classes of Android, float and flex-box and bootstrap of HTML/CSS. you can visit from:

Objective-C: https://github.com/youngsoft/MyLinearLayout
Swift: https://github.com/youngsoft/TangramKit


## A Simple Demo to use MyLayout
 1. There is a container view S which width is 100 and height is wrap to
    all subviews height. there are four subviews A,B,C,D arranged from
    top to bottom.
 2. Subview A's left margin is 20% width of S, right margin is 30% width
    of S, height is equal to width of A.
 3. Subview B's left margin is 40, width is filled in to residual width
    of S,height is 40. Subview C's width is filled in to S, height is
    40.
 4. Subview D's right margin is 20, width is 50% width of S, height is
    40

like below figure:

![demo](https://raw.githubusercontent.com/youngsoft/TangramKit/master/TangramKitDemo/Support%20Files/usagedemo.png)

```objective-c

    MyLinearLayout *S = [MyLinearLayout linearLayoutWithOrientation:MyLayoutViewOrientation_Vert];
    S.subviewSpace = 10;
    S.widthSize.equalTo(@100);
    
    UIView *A = UIView.new;
    A.leftPos.equalTo(@0.2);
    A.rightPos.equalTo(@0.3);
    A.heightSize.equalTo(A.widthSize);
    [S addSubview:A];
    
    UIView *B = UIView.new;
    B.leftPos.equalTo(@40);
    B.widthSize.equalTo(@60);
    B.heightSize.equalTo(@40);
    [S addSubview:B];
    
    UIView *C = UIView.new;
    C.leftPos.equalTo(@0);
    C.rightPos.equalTo(@0);
    C.heightSize.equalTo(@40);
    [S addSubview:C];
    
    UIView *D = UIView.new;
    D.rightPos.equalTo(@20);
    D.widthSize.equalTo(S.widthSize).multiply(0.5);
    D.heightSize.equalTo(@40);
    [S addSubview:D];
    

```

