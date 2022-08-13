---
title: "Prompting the user for a file"
slug: "prompting-the-user-for-a-file"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

NSOpenPanel provides an API for prompting the user for a file to open. This menu is the standard UI presented by the Open (⌘O) menu item. 

## Opening files

# Opening any file
```Objective-C
NSOpenPanel *openPanel = [NSOpenPanel openPanel];
[openPanel beginWithCompletionHandler:^(NSInteger result) {
    NSURL *url = openPanel.URL;
    if (result == NSFileHandlingPanelCancelButton || !url) {
        return;
    }
    // do something with a URL
}];
```

# Allowing opening multiple files
```Objective-C
NSOpenPanel *openPanel = [NSOpenPanel openPanel];
openPanel.allowsMultipleSelection = YES;
[openPanel beginWithCompletionHandler:^(NSInteger result) {
    NSArray <NSURL *>*urls = openPanel.URLs;
    // do things
}];
```

# Limiting to specific file types 
```Objective-C
NSOpenPanel *openPanel = [NSOpenPanel openPanel];
openPanel.allowedFileTypes = @[@".png", @".jpg"];
[openPanel beginWithCompletionHandler:^(NSInteger result) {
    NSURL *url = openPanel.URL;
    if (result == NSFileHandlingPanelCancelButton || !url) {
        return;
    }
    // do something with a picture
}];
```

