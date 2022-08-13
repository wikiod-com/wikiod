---
title: "Navigation Bar"
slug: "navigation-bar"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## SWIFT Example
    navigationController?.navigationBar.titleTextAttributes = [NSForegroundColorAttributeName: UIColor.white, NSFontAttributeName:UIFont(name: "HelveticaNeue-CondensedBold", size: 17)!,]
    navigationController?.navigationBar.tintColor = .white
    navigationController?.navigationBar.barTintColor = .red
    navigationController?.navigationBar.isTranslucent = false
    navigationController?.navigationBar.barStyle = .black

## Customize default navigation bar appearance.
    // Default UINavigationBar appearance throughout the app
    [[UINavigationBar appearance] setTitleTextAttributes:@{NSForegroundColorAttributeName: [UIColor whiteColor],
                                                           NSFontAttributeName : [UIFont fontWithName:@"HelveticaNeue-CondensedBold" size:17],
                                                           }];
    
    [[UINavigationBar appearance] setTintColor:[UIColor whiteColor]];
    [[UINavigationBar appearance] setBarTintColor:[UIColor KNGRed]];
    [[UINavigationBar appearance] setTranslucent:NO];
    [[UINavigationBar appearance] setBarStyle:UIBarStyleBlack];
    [[UIBarButtonItem appearanceWhenContainedIn: [UISearchBar class], nil] setTintColor:[UIColor KNGGray]];


