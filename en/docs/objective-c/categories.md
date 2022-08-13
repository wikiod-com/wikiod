---
title: "Categories"
slug: "categories"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Syntax
- @interface ClassName (categoryName) // ClassName is the class to be extended

- // Method and property declarations

- @end


To avoid method name clashes, it is recommended to use prefixes (like `xyz_` in the example). If methods with the same name exist, it is undefined which one will be used in the runtime.

## Conforming to protocol
You can add protocols to standard classes to extends their functionality:

    @protocol EncodableToString <NSObject>
    - (NSString *)toString;
    @end
    
    @interface NSDictionary (XYZExtended) <EncodableToString>
    @end
    
    @implementation NSDictionary (XYZExtended)
    - (NSString *)toString {
        return self.description;
    }
    @end

where `XYZ` your project's prefix

## Simple Category
Interface and implementation of a simple category on NSArray, named Filter, with a single method that filters numbers. 

It is good practice to add a prefix (`PF`) to the method to ensure we don't overwrite any future `NSArray` methods.

    @interface NSArray (PFFilter)

    - (NSArray *)pf_filterSmaller:(double)number;

    @end

    @implementation NSArray (PFFilter)

    - (NSArray *)pf_filterSmaller:(double)number
    {
        NSMutableArray *result = [NSMutableArray array];
        for (id val in self)
        {
            if ([val isKindOfClass:[NSNumber class] && [val doubleValue] >= number)
            {
                [result addObject:val];
            }
        }
        return [result copy];
    }

    @end

## Declaring a class method
Header file `UIColor+XYZPalette.h`:


    @interface UIColor (XYZPalette)

    +(UIColor *)xyz_indigoColor;

    @end


and implementation `UIColor+XYZPalette.m`:

    @implementation UIColor (XYZPalette)

    +(UIColor *)xyz_indigoColor
    {
        return [UIColor colorWithRed:75/255.0f green:0/255.0f blue:130/255.0f alpha:1.0f];
    }

    @end


## Adding a property with a category
Properties can be added with categories using associated objects, a feature of the Objective-C runtime.

Note that the property declaration of `retain, nonatomic` matches the last argument to `objc_setAssociatedObject`. See [Attach object to another existing object](https://www.wikiod.com/objective-c/low-level-runtime-environment#Attach object to another existing object (association)) for explanations.

    #import <objc/runtime.h>

    @interface UIViewController (ScreenName)

    @property (retain, nonatomic) NSString *screenName;

    @end
    
    @implementation UIViewController (ScreenName)
    
    @dynamic screenName;
    
    - (NSString *)screenName {
        return objc_getAssociatedObject(self, @selector(screenName));
    }
    
    - (void)setScreenName:(NSString *)screenName {
        objc_setAssociatedObject(self, @selector(screenName), screenName, OBJC_ASSOCIATION_RETAIN_NONATOMIC);
    }
    
    @end

## Create a Category on XCode
Categories provide the ability to add some extra functionality to an object without subclassing or changing the actual object.

For example we want to set some custom fonts.
Let's create a category that add functionality to `UIFont` class. Open your XCode project, click on `File` -> `New` -> `File` and choose `Objective-C file`, click Next enter your category name say "CustomFont" choose file type as Category and Class as UIFont then Click "Next" followed by "Create."

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



