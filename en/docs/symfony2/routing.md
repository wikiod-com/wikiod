---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

## Return a 404 response
404 responses are returned when a resource is not found on the server, in Symfony this status can be created by throwing a `NotFoundHttpException` exception. To avoid an extra `use` statement inside a controller use the `createNotFoundException()` provided by the `Controller` class

    <?php
    
    namespace Bundle\Controller;
    
    use Symfony\Bundle\FrameworkBundle\Controller\Controller;
    use Sensio\Bundle\FrameworkExtraBundle\Configuration\Route;
    
    class TestController extends Controller
    {
        /**
         * @Route("/{id}", name="test")
         * Recommended to avoid template() as it has a lot of background processing.
         * Query database for 'test' record with 'id' using param converters.
         */
        public function testAction(Test $test)
        {
            if (!$test) {
                throw $this->createNotFoundException('Test record not found.');
            }
            return $this->render('::Test/test.html.twig', array('test' => $test));
        }
    
    }



## Multiple Routes
In Symfony it's possible to define multiple routes for one action. This can be very helpful if you have functions that do the same but have different parameters. 

    class TestController extends Controller
    {
        /**
         * @Route("/test1/{id}", name="test")
         * @Route("/test2/{id}", name="test2")
         * Here you can define multiple routes with multiple names
         */
        public function testAction(Test $test)
        {
            if (!$test) {
                throw $this->createNotFoundException('Test record not found.');
            }

            return $this->render('::Test/test.html.twig', array('test' => $test));
        }
    }

## POST request redirect
When you are in a **controllerAction**  And have a **POST request coming in**, but want to **redirect it, to a different route**, while still **maintaining the POST method and the request object**, you can use the following:



    return $this->redirectToRoute('route', array(
        'request' => $request,
    ), 307);

Code [307][1] here preserves the request method.


  [1]: https://tools.ietf.org/html/rfc2616#section-10.3.8

## Subdomain-based routing
Subdomain-based routing can be handled in Symfony using `host` parameter. For example, `_locale` parameter can be used as subdomain value.

Assuming

    locale: en
    domain: somedomain.com

parameters are defined in `parameters.yml` config file, route would be:

    /**
     * @Route(
     *      "/",
     *      name="homepage",
     *      host="{_locale}.{domain}",
     *      defaults={"_locale" = "%locale%", "domain" = "%domain%"},
     *      requirements={"_locale" = "%locale%|de|fr", "domain" = "%domain%"}
     * )
     * @Route(
     *      "/",
     *      name="homepage_default",
     *      defaults={"_locale" = "%locale%"}
     * )
     */

From this point router can handle URI's such as `http://de.somedomain.com`. Second `@Route` annotation can be used as a fallback for default locale and void subdomain, `http://somedomain.com`.

## Symfony routes using Routing.yml
        profile_user_profile:
            path:    /profile/{id}
            defaults: { _controller: ProfileBundle:Profile:profile }
            requirements:
                id: \d+
            methods: [get, delete]

If you decide to use Routing.yml instead of Annotations You can get better view of **all routes** and easier to search and find one.

> It is up to you to chose between **Routing.yml** and **Annotations**. You can
> use both for different routes but this is not best solution.

Annotation `@Route()` equivalent is:

    class ProfileController extends Controller
    {
        /**
         * @Route("/profile/{id}", name="profile_user_profile", requirements={"id": "\d+"})
         * @Method("GET", "DELETE")
         */
        public function profileAction($id)
        {
            if (!$id) {
                throw $this->createNotFoundException('User not found.');
            }
    
            return $this->render('::Profile/profile.html.twig', array('id' => $id));
        }
    }

