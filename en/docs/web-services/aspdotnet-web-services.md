---
title: "asp.net web-services"
slug: "aspnet-web-services"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Web Service is an application that is designed to interact directly with other applications over the internet. In simple sense, Web Services are means for interacting with objects over the Internet. The `Web serivce` consumers are able to invoke method calls on remote objects by using `SOAP` and `HTTP` over the Web. `WebService` is language independent and Web Services communicate by using standard web protocols and data formats, such as

 - HTTP
 - XML
 - SOAP



 



## Syntax
 1. **SOAP/WSDL**
    
    [Syntax: http://1111:22/HelloWorld] 
    
    [Response: WDSL]
    
        
       

## Parameters
 | Parameters| Descriptions|
| ------ | ------ |
| PathParam| Binds the parameter passed to method to a value in path.|
| QueryParam| Binds the parameter passed to method to a query parameter in path.|
| MatrixParam| Binds the parameter passed to method to a HTTP matrix parameter in path.|
| HeaderParam| Binds the parameter passed to method to a HTTP header.|
| CookieParam| Binds the parameter passed to method to a Cookie.|
| FormParam| Binds the parameter passed to method to a form value.|
| DefaultValue| Assigns a default value to a parameter passed to method.|
| Context| Context of the resource for example HTTPRequest as a context.|

Now run the application that look like as follows.

[![enter image description here][1]][1]


Now in the above we see our method that we are created in the webservice.cs file, so click on that method and provide input values and click on the "invoke" link as in.

[![enter image description here][2]][2]


The output will be as follows
[![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/vNres.png
  [2]: https://i.stack.imgur.com/pnhus.png
  [3]: https://i.stack.imgur.com/CNSc1.png

## Note
If you closely observe that ,there is no separate  web service template in .Framework 2010 as you see in 2008 while adding a project or web site it might be because of WCF.

So let us start using a different way to add a web service using a template

> 1. "Start" - "All Programs" - "Microsoft Visual Studio 2010"
> 2. "File" - "New Project" - "C#" - "Empty Web Application" (to avoid adding a master page)
> 3. Provide the web site a name such as  "agetodays" or another as you wish and specify the location
> 4. Then right-click on Solution Explorer - "Add New Item" - you see  the web service templates

[![enter image description here][1]][1]

Select Web Service Template and click on add button. then after that the Solution Explorer look like as follows.

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/o7AsF.png
  [2]: https://i.stack.imgur.com/6WvRA.png

Then open the Webservice.cs class and write the following method followed by [webMethod] attribute as in.

    [WebMethod]
        public string HelloWorld() {
            return "Hello World";
        }



