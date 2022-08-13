---
title: "Localization"
slug: "localization"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

**Localization** is feature provided by iOS which translates your app into multiple language.For **Localisation**,**Internationalization** is necessary.**Internationalization** is process of making iOS app able to adapt different culture,language and regions.




## Localization in iOS
Create an individual `Localizable.strings` file for each language. The right side would be different for each language. Think of it as a key-value pair:

    "str" = "str-language";

Access str in Objective-C:
    
    //Try to provide description on the localized string to be able to create a proper documentation if needed
    NSString *str = NSLocalizedString(@"string", @"description of the string");
  
Access str in Swift:

    let str = NSLocalizedString("string", comment: "language");
    





