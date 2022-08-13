---
title: "Component Libraries"
slug: "component-libraries"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

In order to keep Alamofire focused specifically on core networking implementations, additional component libraries have been created by the Alamofire Software Foundation to bring additional functionality to the Alamofire ecosystem.

 - AlamofireImage 
 - AlamofireNetworkActivityIndicator 

## Image Response Serializers
    import AlamofireImage
    
    Alamofire.request("https://httpbin.org/image/png").responseImage { response in
        debugPrint(response)
    
        print(response.request)
        print(response.response)
        debugPrint(response.result)
    
        if let image = response.result.value {
            print("image downloaded: \(image)")
        }
    }

