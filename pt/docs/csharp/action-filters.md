---
title: "Filtros de ação"
slug: "filtros-de-acao"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Filtros de ação personalizada
Escrevemos filtros de ação personalizados por vários motivos. Podemos ter um filtro de ação personalizado para registro ou para salvar dados no banco de dados antes da execução de qualquer ação. Também poderíamos ter um para buscar dados do banco de dados e defini-los como os valores globais do aplicativo.



Para criar um filtro de ação personalizado, precisamos realizar as seguintes tarefas:

1. Crie uma turma
2. Herdar da classe ActionFilterAttribute

**Modifique pelo menos um dos seguintes métodos:**

**OnActionExecuting** – Este método é chamado antes que uma ação do controlador seja executada.

**OnActionExecuted** – Este método é chamado após a execução de uma ação do controlador.

**OnResultExecuting** – Este método é chamado antes que um resultado de ação do controlador seja executado.

**OnResultExecuted** – Este método é chamado após a execução de um resultado de ação do controlador.


**O filtro pode ser criado conforme mostrado na listagem abaixo:**

   

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



