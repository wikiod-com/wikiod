---
title: "Using JavaScript in WebView"
slug: "using-javascript-in-webview"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

This document shows you how you can use JavaScript in WebView.

This document cover: Getting HTML from the WebView,
Entering text in the text box on the website,
Simulate click to click a website button

## Syntax
- `await webView.InvokeScriptAsync("eval", new string[] { functionString })` - to use JavaScript
- `.documentElement` - to get a reference to the root node of the document
- `.getElementsByClassName(Class_Name)` - to get elements usign Class Name
- `.getElementsByTagName(Tab_Name)` - to get elements using Tag Name
- `.getElementById(ID)` - to get element using ID
- `.nodeName` - to get the node name
- `.childNodes` - to get the child elements
- `.outerHTML` - to Get the Outer HTML
- `.innerHTML` - to Get the Inner HTML
- `.innerText` - to Get or Set InnerText
- `.click()` - to Simulate click



Here is a [Sample app to LogIn to StackOverFlow][1]


  [1]: https://github.com/Vijay-Nirmal/StackOverFlow-LogIn

## Getting HTML from the WebView
Use `.outerHTML` to get the HTML

Here is a code sample to get the entire HTML of the website

    private async void GetHTMLAsync()
    {
        var siteHtML = await webView.InvokeScriptAsync("eval", new string[] { "document.documentElement.outerHTML;" });
    }

## Entering text in the text box on the website
Use `.innerText` to set the value

Here is a code sample to enter text in Search Box on Bing website

    private async void EnterTextAsync(string enterText)
    {
        var functionString = string.Format(@"document.getElementsByClassName('b_searchbox')[0].innerText = '{0}';", enterText);
        await webView.InvokeScriptAsync("eval", new string[] { functionString });
    }

## Simulate click to click a website button
Use `.click()` to simulate click

Here is a code sample to click search button on Bing website

    private async void SimulateClickAsync()
    {
        var functionString = string.Format(@"document.getElementsByClassName('b_searchboxSubmit')[0].click();");
        await webView.InvokeScriptAsync("eval", new string[] { functionString });
    }

