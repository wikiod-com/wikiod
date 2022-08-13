---
title: "Eylem Filtreleri"
slug: "eylem-filtreleri"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Özel Eylem Filtreleri
Çeşitli nedenlerle özel eylem filtreleri yazıyoruz. Herhangi bir eylemin yürütülmesinden önce günlüğe kaydetme veya veri tabanına veri kaydetme için özel bir eylem filtremiz olabilir. Veritabanından veri almak ve onu uygulamanın global değerleri olarak ayarlamak için de bir tane alabiliriz.



Özel bir eylem filtresi oluşturmak için aşağıdaki görevleri gerçekleştirmemiz gerekir:

1. Bir sınıf oluşturun
2. ActionFilterAttribute sınıfından devralın

**Aşağıdaki yöntemlerden en az birini geçersiz kılın:**

**OnActionExecuting** – Bu yöntem, bir denetleyici eylemi yürütülmeden önce çağrılır.

**OnActionExecuted** – Bu yöntem, bir denetleyici eylemi yürütüldükten sonra çağrılır.

**OnResultExecuting** – Bu yöntem, bir denetleyici eylemi sonucu yürütülmeden önce çağrılır.

**OnResultExecuted** – Bu yöntem, bir denetleyici eylemi sonucu yürütüldükten sonra çağrılır.


**Filtre aşağıdaki listede gösterildiği gibi oluşturulabilir:**

   

        using System;
        
        using System.Diagnostics;
        
        using System.Web.Mvc;
        
        
        
        namespace WebApplication1
        {
        
            public class MyFirstCustomFilter : ActionFilterAttribute
            {
                public override void OnResultExecuting(ResultExecutingContext filterContext)
                {
                    //You may fetch data from database here 
                    filterContext.Controller.ViewBag.GreetMesssage = "Hello Foo";
                    base.OnResultExecuting(filterContext);
                }
        
                public override void OnActionExecuting(ActionExecutingContext filterContext)
                {
                    var controllerName = filterContext.RouteData.Values["controller"];
                    var actionName = filterContext.RouteData.Values["action"];
                    var message = String.Format("{0} controller:{1} action:{2}", "onactionexecuting", controllerName, actionName);
                    Debug.WriteLine(message, "Action Filter Log");
                    base.OnActionExecuting(filterContext);
                }
            }
        }



