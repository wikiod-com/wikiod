---
title: "Handling URL Schemes"
slug: "handling-url-schemes"
draft: false
images: []
weight: 9900
type: docs
toc: true
---

## Syntax
1. // **canOpenURL** method verifies if there is any app which can handle indicated  URL scheme.

2. // Swift
     
   UIApplication.sharedApplication().canOpenURL(_ aUrl: NSURL)
    
3. // Objective-C
    
   [[UIApplication sharedApplication] canOpenURL:(NSURL *)aUrl];

4. // **openURL** method tries to open a resource located by URL. YES/true if it was opened otherwise NO/false.

5. // Swift
     
   UIApplication.sharedApplication().openURL(_ aUrl: NSURL)
    
6. // Objective-C
    
   [[UIApplication sharedApplication] openURL:(NSURL *)aUrl];

## Parameters
| Parameter | Meaning |      
| ------ | ------ |
| aUrl   | a NSURL instance which stores a built-in or custom scheme string|


In iOS9 and above your app must list any URL schemes it will want to query. This is done by adding `LSApplicationQueriesSchemes` to Info.plist


----------


iOS has built-in support for the `tel`, `http`/`https` ,`sms`, `mailto`, `facetime` schemes. It also supports http–based URLs for `Youtube`, `Maps` and `iTunes` apps.

Examples of built-in URL schemes:

**tel**: `tel://123456890` or `tel:123456890`

**http**: `http://www.google.com`

**facetime**: `facetime://azimov@demo.com`

**mailto**: `mailto://azimov@demo.com`

**sms**: `sms://123456890` or `sms:123456890`

**Youtube**: `https://www.youtube.com/watch?v=-eCaif2QKfA`

**Maps**:

- Using address: `http://maps.apple.com/?address=1,Infinite+Loop,Cupertino,California`

- Using coordinates: `http://maps.apple.com/?ll=46.683155557,6.683155557`

**iTunes**: `https://itunes.apple.com/us/artist/randy-newman/id200900`

*Note*: Not all special characters are supported in `tel` scheme (for example `*` or `#`). This is done because of security concerns to prevent users from unauthorized redirect of calls, so in this case `Phone` app won't be opened.

## Using built-in URL scheme to open Mail app
# Swift:

    if let url = URL(string: "mailto://azimov@demo.com") {
        if UIApplication.shared.canOpenURL(url) {
            UIApplication.shared.openURL(url)
        } else {
            print("Cannot open URL")
        }
    }

# Objective-C:

    NSURL *url = [NSURL URLWithString:@"mailto://azimov@demo.com"];
    if ([[UIApplication sharedApplication] canOpenURL:url]) {
        [[UIApplication sharedApplication] openURL:url];
    } else {
        NSLog(@"Cannot open URL");
    }

## Apple URL Schemes
These are URL schemes supported by native apps on iOS, OS X, and watchOS 2 and later.

**Opening link in Safari:**

Objective-C

    NSString *stringURL = @"http://stackoverflow.com/";
    NSURL *url = [NSURL URLWithString:stringURL];
    [[UIApplication sharedApplication] openURL:url];

Swift:

    let stringURL = "http://stackoverflow.com/"
    if let url = URL(string: stringURL) {
        UIApplication.shared.openURL(url)
    }

**Starting a phone conversation**

Objective-C

    NSString *stringURL = @"tel:1-408-555-5555";
    NSURL *url = [NSURL URLWithString:stringURL];
    [[UIApplication sharedApplication] openURL:url];

Swift:

    let stringURL = "tel:1-408-555-5555"
    if let url = URL(string: stringURL) {
        UIApplication.shared.openURL(url)
    }

HTML

    <a href="tel:1-408-555-5555">1-408-555-5555</a>

**Starting a FaceTime conversation**

Objective-C

    NSString *stringURL = @"facetime:14085551234";
    NSURL *url = [NSURL URLWithString:stringURL];
    [[UIApplication sharedApplication] openURL:url];

Swift:

    let stringURL = "facetime:14085551234"
    if let url = URL(string: stringURL) {
        UIApplication.shared.openURL(url)
    }

HTML

    <a href="facetime:14085551234">Connect using FaceTime</a>
    <a href="facetime:user@example.com">Connect using FaceTime</a>

**Opening Messages App to compose an sms to recipient:**

Objective-C

    NSString *stringURL = @"sms:1-408-555-1212";
    NSURL *url = [NSURL URLWithString:stringURL];
    [[UIApplication sharedApplication] openURL:url];

Swift:

    let stringURL = "sms:1-408-555-1212"
    if let url = URL(string: stringURL) {
        UIApplication.shared.openURL(url)
    }

HTML

    <a href="sms:">Launch Messages App</a>
    <a href="sms:1-408-555-1212">New SMS Message</a>

**Opening Mail app to compose an email to recipient:**

Objective-C

    NSString *stringURL = @"mailto:foo@example.com";
    NSURL *url = [NSURL URLWithString:stringURL];
    [[UIApplication sharedApplication] openURL:url];

Swift:

    let stringURL = "mailto:foo@example.com"
    if let url = URL(string: stringURL) {
        UIApplication.shared.openURL(url)
    }

HTML

    <a href="mailto:frank@wwdcdemo.example.com">John Frank</a>

You can also include a subject field, a message, and multiple recipients in the To, Cc, and Bcc fields. (In iOS, the from attribute is ignored.) The following example shows a mailto URL that includes several different attributes:

    mailto:foo@example.com?cc=bar@example.com&subject=Greetings%20from%20Cupertino!&body=Wish%20you%20were%20here!

*Note:* Compose email dialog can also be presented within app using `MFMailComposeViewController`.

