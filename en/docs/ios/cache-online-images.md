---
title: "Cache online images"
slug: "cache-online-images"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## AlamofireImage
Caching Online Images Using `AlamofireImage`. It works on top of `Alamofire` in Swift.
Install `AlamofireImage` using `cocoapods` 

    pod 'AlamofireImage', '~> 3.1'

**SetUp:**
 1. Import `AlamofireImage` and `Alamofire`
 2. SetUp the Image cache: 
`let imageCache = AutoPurgingImageCache( memoryCapacity: 111_111_111, preferredMemoryUsageAfterPurge: 90_000_000)`
 3. Making a request and adding the Image to Cache:
 

    Alamofire.request(self.nameUrl[i]).responseImage { response in
                        if response.result.value != nil {
                            let image = UIImage(data: response.data!, scale: 1.0)!
                            imageCache.add(image, withIdentifier: self.nameUrl[i])
                        }
        }

 4. Retrieve Images From Cache:


    if let image = imageCache.image(withIdentifier: self.nameUrl[self.a])
            {
                self.localImageView.image = image
            }

For more Info follow [this link][1]


  [1]: https://github.com/Alamofire/AlamofireImage

