---
title: "File association"
slug: "file-association"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Set my app as default app for a file type
    - (NSString *) UTIforFileExtension:(NSString *) extension {
        NSString * UTIString = (NSString *)UTTypeCreatePreferredIdentifierForTag(kUTTagClassFilenameExtension, 
                                                                           (CFStringRef)extension, 
                                                                           NULL);
    
        return [UTIString autorelease];
    }
    
    - (BOOL) setMyselfAsDefaultApplicationForFileExtension:(NSString *) fileExtension {
        OSStatus returnStatus = LSSetDefaultRoleHandlerForContentType (
                                                                       (CFStringRef) [self UTIforFileExtension:fileExtension],
                                                                       kLSRolesAll,
                                                                       (CFStringRef) [[NSBundle mainBundle] bundleIdentifier]
                                                                       );
    
        if (returnStatus != 0) {
            NSLog(@"Got an error when setting default application - %d", returnStatus);
            // Please see the documentation or LSInfo.h
            return NO;
        }
    
        return YES;
    }

[source][1]


  [1]: https://stackoverflow.com/a/8645445/1578528

## Create association with new/custom file types via Info.plist
    <key>CFBundleDocumentTypes</key>
    <array>
        <dict>
            <key>CFBundleTypeIconFile</key>
            <string>Icon file for associated file</string>
            <key>CFBundleTypeName</key>
            <string>My file format</string>
            <key>CFBundleTypeRole</key>
            <string>Viewer</string> <!-- The value can be Editor, Viewer, Shell, or None. This key is required.  -->
            <key>LSItemContentTypes</key>
            <array>
                <string>UTI of the file</string> <!-- Existing UTI or create a UTI for your new file type -->
            </array>
            <key>LSHandlerRank</key>
            <string>Owner</string>
        </dict>
    </array>

[source][1]



  [1]: https://developer.apple.com/library/content/documentation/FileManagement/Conceptual/DocumentInteraction_TopicsForIOS/Articles/RegisteringtheFileTypesYourAppSupports.html

