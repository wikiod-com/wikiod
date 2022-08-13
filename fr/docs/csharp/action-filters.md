---
title: "Filtres d'actions"
slug: "filtres-dactions"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Filtres d'action personnalisés
Nous écrivons des filtres d'action personnalisés pour diverses raisons. Nous pouvons avoir un filtre d'action personnalisé pour la journalisation ou pour enregistrer des données dans la base de données avant toute exécution d'action. Nous pourrions également en avoir un pour récupérer les données de la base de données et les définir comme valeurs globales de l'application.



Pour créer un filtre d'action personnalisé, nous devons effectuer les tâches suivantes :

1. Créer une classe
2. Héritez-le de la classe ActionFilterAttribute

**Remplacez au moins l'une des méthodes suivantes :**

**OnActionExecuting** – Cette méthode est appelée avant l'exécution d'une action de contrôleur.

**OnActionExecuted** – Cette méthode est appelée après l'exécution d'une action de contrôleur.

**OnResultExecuting** – Cette méthode est appelée avant l'exécution d'un résultat d'action de contrôleur.

**OnResultExecuted** – Cette méthode est appelée après l'exécution d'un résultat d'action de contrôleur.


**Le filtre peut être créé comme indiqué dans la liste ci-dessous :**

   

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



