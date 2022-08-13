---
title: "Categories"
slug: "categories"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

Categories can be used to override methods of a class. Even if the method is actually private. The overridden method cannot be accessed from the category or anywhere else. So it's important to make sure when adding methods to an existing class, that those methods don't exist already.

## Create a Category
Categories provide the ability to add some extra functionality to an object without subclassing or changing the actual object.

For example we want to set some custom fonts.
Lets create a category that add functionality to `UIFont` class. Open your Xcode project, click on File -> New -> File and choose Objective-C file , click Next enter your category name say "CustomFont" choose file type as Category and Class as UIFont then Click "Next" followed by "Create."

[![enter image description here][1]][1]
 


[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/4bC0S.png
  [2]: http://i.stack.imgur.com/6mGC1.png

**Declare the Category Method :-**

 Click "UIFont+CustomFonts.h" to view the new category's header file. Add the following code to the interface to declare the method.

    @interface UIFont (CustomFonts)

    +(UIFont *)productSansRegularFontWithSize:(CGFloat)size;
    
    @end

**Now Implement the Category Method:-**

Click "UIFont+CustomFonts.m" to view the category's implementation file. Add the following code to create a method that will set ProductSansRegular Font.

    +(UIFont *)productSansRegularFontWithSize:(CGFloat)size{
        
        return [UIFont fontWithName:@"ProductSans-Regular" size:size];
        
    }


**Import your category**

    #import "UIFont+CustomFonts.h"

Now set the Label font

    [self.label setFont:[UIFont productSansRegularFontWithSize:16.0]];



