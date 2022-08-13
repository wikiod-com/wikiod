---
title: "LinkManager"
slug: "linkmanager"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
- public static string GetItemUrl(Item item)
- public static string GetItemUrl(Item item, UrlOptions options);

## Obtaining a url for an Item
Given a simple sitecore item:

<!-- language: c# -->

    Item item;

The item itself does not contain it's url. To obtain a url for an item you need to make a call to the `static` class `Sitecore.Links.LinkManager`

<!-- language: c# -->

    string url = LinkManager.GetItemUrl(item);

an overload of this accepts a `UrlOptions` class:

<!-- language: c# -->

    UrlOptions options = new UrlOptions
    {
         AddAspxExtension = false
         ....
    };
    string url = LinkManager.GetItemUrl(item, options);


