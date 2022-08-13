---
title: "Working with Web Services"
slug: "working-with-web-services"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Rest API
I have previously written [documentation][1] on this site in order to describe how to make web services on Symfony

I will write again a tutorial for the symfony >= 3 version.

We think that we have a installed web-server on a configured version of [Symfony Framework][2]. You must have [composer][3] (php packages manager) installed too.

To made it simple, if you have composer installed, type this in a terminal / command prompt :

    composer create-project symfony/framework-standard-edition example "3.1.*"

This will create a new directory called "example" in the current directory, with a standard installation of symfony framework.

You must install this 2 Bundles : JMSSerializer Bundle (extends framework component serializer) and FOSRest Bundle (extends framework component routing and controllers...)

You can do this like this (in the example directory) :

    composer require jms/serializer-bundle "~0.13"
    composer require friendsofsymfony/rest-bundle

**Don't forget to activate them in AppKernel !**

Here you can't use : 

    composer create-project gimler/symfony-rest-edition --stability=dev example

Because it's based on Symfony 2.8 version.

First, create your own ("Example") Bundle (in Symfony directory) :

    php bin/console generate:bundle
    php bin/console doctrine:create:database

Imagine that we want to make CRUD (Create / Read / Update / Delete) of this StackOverFlower Entity :

    # src/ExampleBundle/Resources/config/doctrine/StackOverFlower.orm.yml
    ExampleBundle\Entity\StackOverFlower:
        type: entity
        table: stackoverflower
        id:
            id:
                type: integer
                generator: { strategy: AUTO }
        fields:
            name:
                type: string
                length: 100

Configure your Bundle :

    #app/config/config.yml
    fos_rest:
        format_listener:
            rules:
                - { path: '^/stackoverflower', priorities: ['xml', 'json'], fallback_format: xml, prefer_extension: true }
                - { path: '^/', priorities: [ 'text/html', '*/*'], fallback_format: html, prefer_extension: true }


Generate this entity :

    php bin/console doctrine:generate:entity StackOverFlower
    php bin/console doctrine:schema:update --force

Make a Controller :

    #src/ExampleBundle/Controller/StackOverFlowerController.php
    
    namespace ExampleBundle\Controller;
    
    use FOS\RestBundle\Controller\FOSRestController;
    use Symfony\Component\HttpFoundation\Request;
    
    use FOS\RestBundle\Controller\Annotations\Get;
    use FOS\RestBundle\Controller\Annotations\Post;
    use FOS\RestBundle\Controller\Annotations\Delete;
    
    use ExampleBundle\Entity\StackOverFlower;
    
    class StackOverFlowerController extends FOSRestController
    {
        /**
         * findStackOverFlowerByRequest
         * 
         * @param Request $request
         * @return StackOverFlower
         * @throws NotFoundException
         */
        private function findStackOverFlowerByRequest(Request $request) {
            
            $id = $request->get('id');
            $user = $this->getDoctrine()->getManager()->getRepository("ExampleBundle:StackOverFlower")->findOneBy(array('id' => $id));
            
            return $user;
        }
        
        /**
         * validateAndPersistEntity
         * 
         * @param StackOverFlower $user
         * @param Boolean $delete
         * @return View the view
         */
        private function validateAndPersistEntity(StackOverFlower $user, $delete = false) {
            
            $template = "ExampleBundle:StackOverFlower:example.html.twig";
            
            $validator = $this->get('validator');
            $errors_list = $validator->validate($user); 
            
            if (0 === count($errors_list)) {
                
                $em = $this->getDoctrine()->getManager();
                
                if ($delete === true) {
                    $em->remove($user);
                } else {
                    $em->persist($user);
                }
                
                $em->flush();
                
                $view = $this->view($user)
                             ->setTemplateVar('user')
                             ->setTemplate($template);
            } else {
                
                $errors = "";
                foreach ($errors_list as $error) {
                    $errors .= (string) $error->getMessage();  
                }
                
                $view = $this->view($errors)
                             ->setTemplateVar('errors')
                             ->setTemplate($template);
                
            } 
            
            return $view;
        }
        
        /**
         * newStackOverFlowerAction
         * 
         * @Get("/stackoverflower/new/{name}")
         * 
         * @param Request $request
         * @return String
         */
        public function newStackOverFlowerAction(Request $request)
        {   
            $user = new StackOverFlower();
            $user->setName($request->get('name'));
            
            $view = $this->validateAndPersistEntity($user);
                
            return $this->handleView($view);
        }
          
        /**
         * editStackOverFlowerAction
         * 
         * @Get("/stackoverflower/edit/{id}/{name}")
         * 
         * @param Request $request
         * @return type
         */
        public function editStackOverFlowerAction(Request $request) {
            
            $user = $this->findStackOverFlowerByRequest($request);
            
            if (! $user) {
                $view = $this->view("No StackOverFlower found for this id:". $request->get('id'), 404);
                return $this->handleView($view);
            }
            
            $user->setName($request->get('name'));
            
            $view = $this->validateAndPersistEntity($user);
                    
            return $this->handleView($view);
        }
        
        /**
         * deleteStackOverFlowerAction
         * 
         * @Get("/stackoverflower/delete/{id}")
         * 
         * @param Request $request
         * @return type
         */
        public function deleteStackOverFlowerAction(Request $request) {
            
            $user = $this->findStackOverFlowerByRequest($request);
            
            if (! $user) {
                $view = $this->view("No StackOverFlower found for this id:". $request->get('id'), 404);
                return $this->handleView();
            }
            
            $view = $this->validateAndPersistEntity($user, true);
                    
            return $this->handleView($view);
        }
        
        /**
         * getStackOverFlowerAction
         * 
         * @Get("/stackoverflowers")
         * 
         * @param Request $request
         * @return type
         */
        public function getStackOverFlowerAction(Request $request) {
            
            $template = "ExampleBundle:StackOverFlower:example.html.twig";
            
            $users = $this->getDoctrine()->getManager()->getRepository("ExampleBundle:StackOverFlower")->findAll();
            
            if (0 === count($users)) {
                $view = $this->view("No StackOverFlower found.", 404);
                return $this->handleView();
            }
            
            $view = $this->view($users)
                         ->setTemplateVar('users')
                         ->setTemplate($template);
            
            return $this->handleView($view);
        }
    }

**Don't tell me that is a fat controller, it's for the example !!!**

Create your template :

    #src/ExampleBundle/Resources/views/StackOverFlower.html.twig
    {% if errors is defined %}
      {{ errors }}  
    {% else %}
      {% if users is defined %}
        {{ users | serialize }}
      {% else %}
        {{ user | serialize }}
      {% endif %}
    {% endif %}

You have just made your first RESTFul API !!!

You can test it on : http://your-server-name/your-symfony-path/app_dev.php/stackoverflower/new/test.

As you can see in the database, a new user has been created with the name : "test".

You can view a full working example of this code on my [GitHub Account][4], one branch with more real routes...

**This is a very basic example, don't let that in production environnement, you must protect your api with apikey !!!**

*A future example, may be ?*

  [1]: https://www.wikiod.com/symfony2/creating-web-services-with-symfony-28#Work with RESTFul API
  [2]: http://symfony.com/doc/current/setup.html
  [3]: https://getcomposer.org/download/
  [4]: https://github.com/weenesta/symfony-3.1-example-rest

