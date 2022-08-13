---
title: "Alamofire"
slug: "alamofire"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

## Syntax
- response()
- responseData()
- responseString(encoding: NSStringEncoding)
- responseJSON(options: NSJSONReadingOptions)
- responsePropertyList(options: NSPropertyListReadOptions)

## Parameters
| Parameter | Details |
| --------- | ------- |
| Method | `.OPTIONS, .GET, .HEAD, .POST, .PUT, .PATCH, .DELETE, .TRACE, .CONNECT` |
| URLString | `URLStringConvertible` |
| parameters | `[String: AnyObject]?` |
| encoding | `ParameterEncoding` |
| headers | `[String: String]?` |


## Manual Validation
    Alamofire.request(.GET, "https://httpbin.org/get", parameters: ["foo": "bar"])
         .validate(statusCode: 200..<300)
         .validate(contentType: ["application/json"])
         .response { response in
             print(response)
         }

## Automatic Validation
    Alamofire.request("https://httpbin.org/get").validate().responseJSON { response in
    switch response.result {
    case .success:
        print("Validation Successful")
    case .failure(let error):
        print(error)
     }
    }

## Chained Response Handlers
    Alamofire.request(.GET, "https://httpbin.org/get")
         .validate()
         .responseString { response in
             print("Response String: \(response.result.value)")
         }
         .responseJSON { response in
             print("Response JSON: \(response.result.value)")
         }

## Making a Request
    import Alamofire

    Alamofire.request(.GET, "https://httpbin.org/get")

## Response Handling
    Alamofire.request(.GET, "https://httpbin.org/get", parameters: ["foo": "bar"])
         .responseJSON { response in
             print(response.request)  // original URL request
             print(response.response) // URL response
             print(response.data)     // server data
             print(response.result)   // result of response serialization

             if let JSON = response.result.value {
                 print("JSON: \(JSON)")
             }
         }

## Response Handler
    Alamofire.request(.GET, "https://httpbin.org/get", parameters: ["foo": "bar"])
         .validate()
         .response { request, response, data, error in
             print(request)
             print(response)
             print(data)
             print(error)
          }

