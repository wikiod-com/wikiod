---
title: "Controllers"
slug: "controllers"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

A controller in Symfony is a PHP callable (a function, a method on an object, or a closure) that receives an HTTP request and returns an HTTP response. An HTTP response can contain anything: an HTML page, a JSON string, a file download, etc.

In order to tell Symfony which controller should handle a certain request, you need to [configure a route][1].

  [1]: https://www.wikiod.com/symfony/routing

## Syntax
 - $this->generateUrl('route_name', ['placeholder' => 'value']); // generates a URL for the route `route_name` with a placeholder
 - $this->render('template.html.twig'); // renders a Twig template and returns a Response object
 - $this->render('template.html.twig', ['parameter' => $value]); // renders a Twig template with a parameter
 - throw $this->createNotFoundException('Message'); // throws a NotFoundHttpException which will cause Symfony to return a 404 response

Controllers should be small and focus on handling HTTP requests: the actual business logic of your application should be delegated to different parts of your application, for instance your domain model.

## A simple controller class
    // src/AppBundle/Controller/HelloWorldController.php
    namespace AppBundle\Controller;
    
    use Symfony\Component\HttpFoundation\Response;
    
    class HelloWorldController
    {
        public function helloWorldAction()
        {
            return new Response(
                '<html><body>Hello World!</body></html>'
            );
        }
    }

## Rendering a Twig template
Most of the time, you will want to render HTML responses from a template instead of hard-coding the HTML in your controller. Also, your templates will not be static but will contain placeholders for application data. By default Symfony comes with Twig, a powerful templating language.

In order to use Twig in your controller, extend Symfony's base `Controller` class:

    // src/AppBundle/Controller/HelloWorldController.php
    namespace AppBundle\Controller;
    
    use Symfony\Bundle\FrameworkBundle\Controller\Controller;
    use Symfony\Component\HttpFoundation\Response;
    
    class HelloWorldController extends Controller
    {
        public function helloWorldAction()
        {
            $text = 'Hello World!';

            return $this->render('hello-world.html.twig', ['text' => $text]);
        }
    }

Create the Twig template (located in `app/Resources/views/hello-world.html.twig`):

    <html><body>{{ text }}</body></html>

Twig will automatically replace the `{{ text }}` placeholder with the value of the `text` parameter, passed by the controller. This will render the following HTML output:

    <html><body>Hello World!</body></html>

## Returning a 404 (Not Found) page
Sometimes you want to return a 404 (Not Found) response, because the requested resource does not exist. Symfony allows you to do so by throwing a [`NotFoundHttpException`][1].

The Symfony base Controller exposes a `createNotFoundException` method which creates the exception for you:

    public function indexAction()
    {
        // retrieve the object from database
        $product = ...;
    
        if (!$product) {
            throw $this->createNotFoundException('The product does not exist');
        }
    
        // continue with the normal flow if no exception is thrown
        return $this->render(...);
    }


  [1]: http://api.symfony.com/3.2/Symfony/Component/HttpKernel/Exception/NotFoundHttpException.html

## Using data from the Request object
If you need to access the `Request` object (for instance to read the query parameters, to read an HTTP header or to process an uploaded file), you can receive the request as a method argument by adding a type-hinted argument:

    use Symfony\Component\HttpFoundation\Request;

    public function indexAction(Request $request)
    {
        $queryParam = $request->query->get('param');

        // ...
    }

Symfony will recognize the type hint and add the request argument to the controller call.

