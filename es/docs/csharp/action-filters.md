---
title: "Filtros de acción"
slug: "filtros-de-accion"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Filtros de acción personalizados
Escribimos filtros de acción personalizados por varias razones. Es posible que tengamos un filtro de acción personalizado para iniciar sesión o para guardar datos en la base de datos antes de la ejecución de cualquier acción. También podríamos tener uno para obtener datos de la base de datos y configurarlos como los valores globales de la aplicación.



Para crear un filtro de acción personalizado, debemos realizar las siguientes tareas:

1. Crea una clase
2. Heredar de la clase ActionFilterAttribute

**Anule al menos uno de los siguientes métodos:**

**OnActionExecuting**: este método se llama antes de que se ejecute una acción del controlador.

**OnActionExecuted**: este método se llama después de ejecutar una acción del controlador.

**OnResultExecuting**: se llama a este método antes de que se ejecute el resultado de una acción del controlador.

**OnResultExecuted**: se llama a este método después de ejecutar el resultado de una acción del controlador.


**El filtro se puede crear como se muestra en el siguiente listado:**

   

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



