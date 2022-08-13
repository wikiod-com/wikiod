---
title: "Barcode scanning using ZXing library in Xamarin Applications"
slug: "barcode-scanning-using-zxing-library-in-xamarin-applications"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Zxing library is well known for image processing. Zxing was based on java and .Net module is also available and it can be used in xamarin applications. click here to check official documentation.
http://zxingnet.codeplex.com/

I recently used this libarry.

Step1: Add ZXing.Net.Mobile component into solution.

step2: In whichever activity we need to show barcode scanner, in that activity initialise MobileBarcodeScanner.

Step3: Write below code when tapped on any view to start scanning.



## Sample Code
    button.Click +=async delegate 
    { 
    var MScanner = new MobileBarcodeScanner(); 
    var Result = await MScanner.Scan(); 
    if(Result == null) 
    { 
        return; 
    } 
    //get the bar code text here 
    string BarcodeText = Result.text; 
    }

