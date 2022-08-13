---
title: "MVC vs Web Forms"
slug: "mvc-vs-web-forms"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

Before you jump into ASP. NET MVC to develop your web application you should consider the advantages and disavantages of the framework and you should know that there is another web framework made and maintained by Microsoft that is ASP .NET Web Forms.

Which one should you choose is a matter of knowledge of both techonologies.

## Syntax
- The ASPX View Engine uses "<%= %>" or "<%: %>" to render server-side content.

- The Razor View Engine uses @ to render server-side content.

https://www.asp.net/web-forms

https://www.asp.net/mvc

## Advantages of ASP .NET Web Forms
- Pre build controls to handle Grids, Inputs, Graphs, Trees, and so on.
- It supports an event model that preserves state over HTTP, which benefits line-of-business Web application development. The Web Forms-based application provides dozens of events that are supported in hundreds of server controls.

- It uses a Page Controller pattern that adds functionality to individual pages. For more information, see Page Controller on the MSDN Web site.

- It uses view state or server-based forms, which can make managing state information easier.

- It works well for small teams of Web developers and designers who want to take advantage of the large number of components available for rapid application development.


- In general, it is less complex for application development, because the components (the Page class, controls, and so on) are tightly integrated and usually require less code than the MVC model.

- Easy development model for those developers coming from WindowsForm development.

[What is Web Forms][1]


  [1]: https://www.asp.net/web-forms/what-is-web-forms

## Advantages of an MVC-Based Web Application
- It makes it easier to manage complexity by dividing an application into the model, the view, and the controller (Separation of concerns).

- It does not use view state or server-based forms. This makes the MVC framework ideal for developers who want full control over the behavior of an application.

- It uses a Front Controller pattern that processes Web application requests through a single controller. This enables you to design an application that supports a rich routing infrastructure. For more information, see Front Controller on the MSDN Web site.

- It provides better support for test-driven development (TDD).

- It works well for Web applications that are supported by large teams of developers and Web designers who need a high degree of control over the application behavior.

[What is Web Forms][1]


  [1]: https://www.asp.net/web-forms/what-is-web-forms

## Disadvantages
Web Forms:
- Complex Page Life Cycle, whenever a Request is made to the server there are at least 5 methods to execute previous to the event handler.
- Dificult to work with Client-Side frameworks like JQuery or Angular.
- Hard to work with Asyncronous Javascript and XML (AJAX)
- Viewstate handling
- The page's client-side and the code behind are tightly coupled.

MVC:
- It takes more time to develop in comparision with Web Forms.
- Data is sent in clear text format to the server whereas in web forms view state data are encrypted by default.

## Razor View Engine VS ASPX View Engine
| Razor (MVC)| ASPX (Web Forms)|
| ------ | ------ |
| The namespace used by the Razor View Engine is System.Web.Razor| The namespace used by the ASPX View Engine is System.Web.Mvc.WebFormViewEngine   |
| The file extensions used by the Razor View Engine are different from a web form view engine. It uses cshtml with C# and vbhtml with vb for views, partial view, templates and layout pages.| The file extensions used by the Web Form View Engines are like ASP.Net web forms. It uses the ASPX extension to view the aspc extension for partial views or User Controls or templates and master extensions for layout/master pages.   |
| The Razor Engine supports Test Driven Development (TDD).  | Web Form view engine does not support Test Driven Development (TDD) because it depends on the System.Web.UI.Page class to make the testing complex.|

[ASPX View Engine VS Razor View Engine][1]

 


  [1]: http://www.c-sharpcorner.com/UploadFile/ff2f08/aspx-view-engine-vs-razor-view-engine/

