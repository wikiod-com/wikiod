---
title: "NSUserDefaults"
slug: "nsuserdefaults"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Simple example
For example:
  
FOR SAVING:
    
      NSUserDefaults *prefs = [NSUserDefaults standardUserDefaults];

      // saving an NSString
      [prefs setObject:txtUsername.text forKey:@"userName"];
      [prefs setObject:txtPassword.text forKey:@"password"];

      [prefs synchronize];

FOR RETRIEVING

      NSUserDefaults *prefs = [NSUserDefaults standardUserDefaults];

      // getting an NSString
     NSString *savedUsername = [prefs stringForKey:@"userName"];
     NSString *savedPassword = [prefs stringForKey:@"password"];

## Clear NSUserDefaults
    NSString *appDomain = [[NSBundle mainBundle] bundleIdentifier];
    [[NSUserDefaults standardUserDefaults] removePersistentDomainForName:appDomain];

