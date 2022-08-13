---
title: "Getting started with zend-framework2"
slug: "getting-started-with-zend-framework2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## A simple Hello World
In your command line, get in the directory you want to create the project in, then type: `composer create-project zendframework/skeleton-application helloWorldTest`. During installation, you will be asked if you want a minimal install: Let's say yes for the moment, we are just testing.

For simplicity sake, we will use the built-in PHP CLI server. From the command line, get yourself in the root directory of your project (`helloWorldTest`), then run : `php -S 0.0.0.0:8080 -t public/ public/index.php`. Now, open your web browser and go to http://localhost/, you should see the welcome page of the ZF2 Skeleton Application.

If you do so, we will now setup a new page. In `module/Application/config/module.config.php` you can see that a dynamic route is already setup for the application subfolder:

    return [
        'router' => [
            'routes' => [
                'home' => [
                    ...
                ],
                'application' => [
                    'type'    => Segment::class,
                    'options' => [
                        'route'    => '/application[/:action]',
                        'defaults' => [
                            'controller'    => Controller\IndexController::class,
                            'action'        => 'index',
                        ],
                    ],
                ],
            ],
        ],


Set a new action "`helloWorldAction()`" in `module/Applicaiton/src/Controller/IndexController.php`:

    class IndexController extends AbstractActionController
    {
        public function indexAction()
        {
            ...
        }
        
        public function helloWorldAction()
        {
            return new ViewModel();
        }
    }

Finally, create the view file `module/Application/view/application/index/hello-world.phtml` with the following content:

    <?php
    echo "Hello World !";

Now, go to http://localhost/application/hello-world, and say hi to ZF2 !

## Installation or Setup
Detailed instructions on getting Zend Framework 2 set up or installed. There are various ways of installing the framework. Below are some of them: 

## Using Composer - Recommended way
Assuming `composer` is [installed][1] on the target box.

To install a skeleton MVC application, run in your terminal to create a new zend framework 2 project in specified location:

    php composer.phar create-project -sdev \
        --repository-url="https://packages.zendframework.com" \
        zendframework/skeleton-application path/to/install

to manually install a **minimal** ZF2 (Zend MVC + its handful of dependencies), run in your command line:

    composer require zendframework/zend-mvc

or for a **full-fledge**d ZF2 (+64 modules):

    composer require zendframework/zendframework`

Please note that the first option runs an installer that will provide you with a fully-functionnal application along with the usual application directories structure. Other options will let you build the whole application from scratch as it simply provides ZF2 modules to build upon.

## Using Git Submodules 
Run the command below to clone zf2 and it's dependencies recursively from Github:

    git clone git://github.com/zendframework/ZendSkeletonApplication.git --recursive

# HTTP Server Setup
A typical web application requires a running a HTTP service listening a dedicated port (usually :80) to pass incoming requests to application, process and serve the output (response) back.

> Note: You can also write console-aware applications in Zend Framework 2 without a need to a HTTP server. 

## OPTION 1 - PHP CLI Server
The simplest way to get started if you are using PHP 5.4 or above is to start the internal PHP cli-server in the root directory.

Go to project directory and run:

    php -S 0.0.0.0:8080 -t public/ public/index.php`.

This will start the builtin cli-server on port `8080`, and bind it to all network interfaces.

## OPTION 2 - A custom HTTP Server
   Configure a virtualhost on Apache *or* Microsoft IIS Server *or* Nginx and pass incoming HTTP requests to the application.


  [1]: https://getcomposer.org/download/

## How to create a factory
When a class needs to be provided with hard dependencies best practice is to use a constructor injection pattern where those dependencies are injected using a factory.

Let's assume that `MyClass` is hard dependent on a value `$dependency` that needs to be resolved from the application config.  

    <?php
    namespace Application\Folder;

    use Zend\ServiceManager\FactoryInterface;
    use Zend\ServiceManager\ServiceLocatorInterface;

    class MyClass
    {
        protected $dependency;

        public function __construct($dependency)
        { 
            $this->dependency = $dependency;
        }
    }

To inject this dependency a factory class is created. This factory will resolve the dependency from the config and inject the config value on construction of the class and return the result:
    
    <?php
    namespace Application\Factory;

    use Zend\ServiceManager\FactoryInterface;
    use Zend\ServiceManager\ServiceLocatorInterface;

    class MyClassFactory implements FactoryInterface
    {
        public function createService(ServiceLocatorInterface $serviceLocator)
        { 
            $config = $servicelocator->get('Config');
            $dependency = $config['dependency'];
            $myClass = new MyClass($dependency);
            return $myClass;
        }
    }

Now that the factory class has been created it has to be registered inside the service manager config in the module config file `module.config.php` under the key factories.  It is good practice to use the same names for both the class and the factory so it is easy to find them in the project folder tree:

    <?php

    namespace Application;

    return array(
        //...
        'service_manager' => [
            'factories' => [
                'Application\Folder\MyClass' => 'Application\Factory\MyClassFactory'
            ]
        ],
        //...
    );

Alternatively the class name constants can be used to register them:

    <?php

    namespace Application;

    use Application\Folder\MyClass;
    use Application\Factory\MyClassFactory;

    return array(
        //...
        'service_manager' => [
            'factories' => [
                MyClass::class => MyClassFactory::class'
            ]
        ],
        //...
    );

Now the class can be collected at the service manager using the key that we used when registering the factory for that class:

     $serviceManager->get('Application\Folder\MyClass');

or

     $serviceManager->get(MyClass::class);

The service manager will find, collect and run the factory and then it returns your class instance with the dependency injected.

