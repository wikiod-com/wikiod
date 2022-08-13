---
title: "Symfony Services"
slug: "symfony-services"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## How to declare, write and use a simple service in Symfony2
Services declaration :

    # src/Acme/YourBundle/Resources/config/services.yml

    services:
        my_service:
            class: Acme\YourBundle\Service\MyService
            arguments: ["@doctrine", "%some_parameter%", "@another_service"]
        another_service:
            class: Acme\YourBundle\Service\AnotherService
            arguments: []

Service code :

    <?php
    namespace Acme\YourBundle\Service\Service;

    class MyService 
    {
        /**
         * Constructor
         * You can had whatever you want to use in your service by dependency injection
         * @param $doctrine Doctrine
         * @param $some_parameter Some parameter defined in app/config/parameters.yml
         * @param $another_service Another service
         */
        public function __construct($doctrine, $some_parameter, $another_service) 
        {
            $this->doctrine = $doctrine;
            $this->some_parameter = $some_parameter;
            $this->another_service = $another_service;
        }
    
        public function doMagic() 
        {
            // Your code here
        }
    }

Use it in a controller :

    <?php
    
    namespace Acme\YourBundle\Controller;
    
    use Symfony\Bundle\FrameworkBundle\Controller\Controller;
    use Acme\YourBundle\Service\Service\MyService;
    
    class MyController extends Controller
    {
      /**
         * One action
         */
        public function oneAction(Request $request)
        {
            $myService = $this->get('my_service');
            $myService->doMagic();
            // ...
        }
    }

