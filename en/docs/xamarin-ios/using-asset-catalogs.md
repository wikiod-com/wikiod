---
title: "Using Asset Catalogs"
slug: "using-asset-catalogs"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Adding image assets to asset catalog
This is how the Asset Catalog in Xamarin Studio looks like,

[![Asset Catalog options][1]][1]


As shown in above picture there are 5 types of assets you can create within the catalog.

I will cover only image set, because its the simplest one.

When you create a new image set. You will get options like this
[![New Image Set Options][2]][2]

To add images to the catalog you can simply click on the dashed squares and select the image you want to set for particular option.

In XCode you have options of 1x, 2x and 3x to cover the most recent iOS device screen sizes. But Xamarin has one extra option Vector using which you can upload PDF formatted Vector image which would be automatically scaled depending on the device your application is running on.

For iPhone images Xamarin retains the iOS7 special image size R4 which is used for 4-Inch screen sized iPhone (5, 5S and SE).

Please refer [Xamarin Documentation on how to add images to iOS application][3] for more information.


  [1]: http://i.stack.imgur.com/CD8Hv.png
  [2]: http://i.stack.imgur.com/gMke8.png
  [3]: https://developer.xamarin.com/guides/ios/application_fundamentals/working_with_images/displaying-an-image/#asset-catalogs

