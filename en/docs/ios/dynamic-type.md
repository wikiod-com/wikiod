---
title: "Dynamic Type"
slug: "dynamic-type"
draft: false
images: []
weight: 9936
type: docs
toc: true
---

    // Content size category constants
    UIContentSizeCategoryExtraSmall
    UIContentSizeCategorySmall
    UIContentSizeCategoryMedium
    UIContentSizeCategoryLarge
    UIContentSizeCategoryExtraLarge
    UIContentSizeCategoryExtraExtraLarge
    UIContentSizeCategoryExtraExtraExtraLarge
    
    // Accessibility sizes
    UIContentSizeCategoryAccessibilityMedium
    UIContentSizeCategoryAccessibilityLarge
    UIContentSizeCategoryAccessibilityExtraLarge
    UIContentSizeCategoryAccessibilityExtraExtraLarge
    UIContentSizeCategoryAccessibilityExtraExtraExtraLarge

## Get the Current Content Size 

# Swift

    UIApplication.sharedApplication().preferredContentSizeCategory

# Objective-C

    [UIApplication sharedApplication].preferredContentSizeCategory;

This returns a content size category constant, or an accessibility content size category constant.

## Matching Dynamic Type Font Size in WKWebView
WKWebView resizes the fonts on web content so that a full-sized web page will fit on the device's form factor. If you want the web text in both portrait and landscape to be similar in size to the user's preferred reading size, you need to set it explicitly.

# Swift

    // build HTML header for dynamic type and responsive design
    func buildHTMLHeader() -> String {
            
        // Get preferred dynamic type font sizes for html styles
        let bodySize = UIFont.preferredFont(forTextStyle: UIFontTextStyle.body).pointSize
        let h1Size = UIFont.preferredFont(forTextStyle: UIFontTextStyle.title1).pointSize
        let h2Size = UIFont.preferredFont(forTextStyle: UIFontTextStyle.title2).pointSize
        let h3Size = UIFont.preferredFont(forTextStyle: UIFontTextStyle.title3).pointSize
            
        // On iPad, landscape text is larger than preferred font size
        var portraitMultiplier = CGFloat(1.0)
        var landscapeMultiplier = CGFloat(0.5)
        
        // iPhone text is shrunken    
        if UIDevice.current.model.range(of: "iPhone") != nil {
            portraitMultiplier = CGFloat(3.0)
            landscapeMultiplier = CGFloat(1.5)
        }
            
        // Start HTML header text
        let patternText = "<html> <head> <style> "
            
        // Match Dynamic Type for this page.
        + "body { background-color: \(backgroundColor);} "
        + "@media all and (orientation:portrait) {img {max-width: 90%; height: auto;} "
        + "p, li { font: -apple-system-body; font-family: Georgia, serif; font-size:calc(\(bodySize * portraitMultiplier)px + 1.0vw); font-weight: normal; color: \(fontColor) } "
        + "h1 { font: -apple-system-headine; font-family: Verdana, sans-serif; font-size:calc(\(h1Size * portraitMultiplier)px + 1.0vw); font-weight: bold; color: \(headFontColor) } "
        + "h2 { font: -apple-system-headine; font-family: Verdana, sans-serif; font-size:calc(\(h2Size * portraitMultiplier)px + 1.0vw); font-weight: bold; color: \(headFontColor) } "
        + "h3, h4 { font: -apple-system-headine; font-family: Verdana, sans-serif; font-size:calc(\(h3Size * portraitMultiplier)px + 1.0vw); font-weight: bold; color: \(headFontColor) } } "
        + "@media all and (orientation:landscape) {img {max-width: 65%; height: auto;}"
        + "p, li { font: -apple-system-body; font-family: Georgia, serif; font-size:calc(\(bodySize * landscapeMultiplier)px + 1.0vw); font-weight: normal; color: \(fontColor) }"
        + "h1 { font: -apple-system-headine; font-family: Verdana, sans-serif; font-size:calc(\(h1Size * landscapeMultiplier)px + 1.0vw); font-weight: bold; color: \(headFontColor) } "
        + "h2 { font: -apple-system-headine; font-family: Verdana, sans-serif; font-size:calc(\(h2Size * landscapeMultiplier)px + 1.0vw); font-weight: bold; color: \(headFontColor) } "
        + "h3, h4 { font: -apple-system-headine; font-family: Verdana, sans-serif; font-size:calc(\(h3Size * landscapeMultiplier)px + 1.0vw); font-weight: bold; color: \(headFontColor) } } </style>"
        + "</head><body>"
        + "<meta name=\"viewport\" content=\"width: device-width\">"
            
        return patternText
    }



## Text Size Change Notification
You can register for notifications of when the device text size is changed.

# Swift 

    NSNotificationCenter.defaultCenter().addObserver(self, selector: #selector(updateFont), name: name:UIContentSizeCategoryDidChangeNotification, object: nil)

# Objective-C

    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(updateFont) name:UIContentSizeCategoryDidChangeNotification object:nil];


The notification `userInfo` object contains the new size under  `UIContentSizeCategoryNewValueKey`.

## Handling Preferred Text Size Change Without Notifications on iOS 10
`UILabel`, `UITextField`, & `UITextView` classes have a new property starting from iOS 10 for automatically resizing their font when a user changes their preferred reading size named `adjustsFontForContentSizeCategory`.

# Swift
    @IBOutlet var label:UILabel!

    if #available(iOS 10.0, *) {
        label.adjustsFontForContentSizeCategory = true
    } else {
        // Observe for UIContentSizeCategoryDidChangeNotification and handle it manually
        // since the adjustsFontForContentSizeCategory property isn't available.
    }

