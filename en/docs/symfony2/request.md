---
title: "Request"
slug: "request"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

API documentation links (master):
* [Request][1]
* [RequestStack][2]

Request object contains several significant data like current Locale and matched Controller. You can use and manage them by HttpKernel events. For reliable  understanding of Request-Responce live cycle read this [HttpKernel Component][3] doc page (very helpful!).


  [1]: http://api.symfony.com/master/Symfony/Component/HttpFoundation/Request.html
  [2]: http://api.symfony.com/master/Symfony/Component/HttpFoundation/RequestStack.html
  [3]: http://symfony.com/doc/current/components/http_kernel.html

## Access to Request in a Controller
    <?php
    
    use Symfony\Bundle\FrameworkBundle\Controller\Controller;
    use Symfony\Component\HttpFoundation\Request;
    
    class TestController extends Controller
    {
       //Inject Request HTTP Component in your function then able to exploit it
       public function myFunctionAction(Request $request)
       {
          //BASICS

          //retrieve $_POST variables from request
          $postRequest = $request->request->get('my_data');
          //retrieve $_GET variables from request
          $getRequest = $request->query->get('my_data');
          //get current locale
          $locale = $request->getLocale();
       }
    }

Note that injected Request object applies to current request (it may or may not equal to master request).

## Access to Request in a Twig or PHP template.
In Twig template, Request object is available at 
    
    {{ app.request }}

When you want display request method in Twig, try this:
    
    <p>Request method: {{ app.request.method }}</p>


In PHP template
    
    <p>Request method: <?php echo $app->getRequest()->getMethod() ?></p>

    

    

