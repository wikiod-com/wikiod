---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Routing is the process of mapping a URL to a [controller][1]. Symfony has a powerfull Routing component which allows you to define routes.

The Routing component supports a number of configuration formats: annotations, YAML, XML and raw PHP.


  [1]: https://www.wikiod.com/symfony/controllers

## Parameters
| Parameter | Details |
| --- | --- |
| name | The name of the route. Example: `book_show` |
| path | The path (may contain wildcards). Example: `/book/{isbn}` |
| defaults | Default values of parameters |

## Simple routes
Using YAML:

    # app/config/routing.yml
    blog_list:
        path:     /blog
        defaults: { _controller: AppBundle:Blog:list }

Using Annotations:

    // src/AppBundle/Controller/BlogController.php
    namespace AppBundle\Controller;
    
    use Symfony\Bundle\FrameworkBundle\Controller\Controller;
    use Sensio\Bundle\FrameworkExtraBundle\Configuration\Route;
    
    class BlogController extends Controller
    {
        /**
         * @Route("/blog", name="blog_list")
         */
        public function listAction()
        {
            // ...
        }
    }

A request for the `/blog` URL will be handled by the `listAction()` method of the `BlogController` inside `AppBundle`.

## Routes with placeholders
Using YAML:

    # app/config/routing.yml
    blog_show:
        path:     /blog/{slug}
        defaults: { _controller: AppBundle:Blog:show }

Using Annotations:

    // src/AppBundle/Controller/BlogController.php
    namespace AppBundle\Controller;
    
    use Symfony\Bundle\FrameworkBundle\Controller\Controller;
    use Sensio\Bundle\FrameworkExtraBundle\Configuration\Route;
    
    class BlogController extends Controller
    {
        /**
         * @Route("/blog/{slug}", name="blog_show")
         */
        public function showAction($slug)
        {
            // ...
        }
    }

Any request with a URL matching `/blog/*` will be handled by the `showAction()` method of the `BlogController` within `AppBundle`. The controller action will receive the value of the placeholder as a method argument.

For example, a request for `/blog/my-post` will trigger a call to `showAction()` with an argument `$slug` containing the value `my-post`. Using that argument, the controller action can change the response depending on the value of the placeholder, for instance by retrieving the blog post with the slug `my-post` from the database.

## Default values for placeholders
If you want to have a placeholder that may be omitted, you can give it a default value:

Using YAML:

    # app/config/routing.yml
    blog_list:
        path:      /blog/{page}
        defaults:  { _controller: AppBundle:Blog:list, page: 1 }
        requirements:
            page: '\d+'

Using Annotations:

    // src/AppBundle/Controller/BlogController.php
    namespace AppBundle\Controller;
    
    use Symfony\Bundle\FrameworkBundle\Controller\Controller;
    use Sensio\Bundle\FrameworkExtraBundle\Configuration\Route;
    
    class BlogController extends Controller
    {
        /**
         * @Route("/blog/{page}", name="blog_list", requirements={"page": "\d+"})
         */
        public function listAction($page = 1)
        {
            // ...
        }
    }

In this example, both the `/blog` and `/blog/1` URLs will match the `blog_list` route and will be handled by the `listAction()` method. In the case of `/blog`, `listAction()` will still receive the `$page` argument, with the default value `1`.

