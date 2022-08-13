---
title: "Introduction to Zend Expressive"
slug: "introduction-to-zend-expressive"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## A simple Hello World
Using composer, execute the following command in the directory in which the application will be installed: `composer create-project zendframework/zend-expressive-skeleton expressive-skeleton`.

During installation process, you will be asked to make various decisions.
1) For the default installation question, say no (`n`); 
2) For the router, let's use Aura Router (#`1`);
3) For the container, let's use Zend ServiceManager (#`3`);
4) For the template, let's use Zend View (#`3`);
5) Finally, for the error handler, let's use Whoops (#`1`).

Once installed, get yourself in the root directory, `expressive-skeleton`, launch the built-in PHP CLI server: `php -S 0.0.0.0:8080 -t public public/index.php`. Go to http://localhost:8080/ with your browser, your application should now be up and running.


Let's configure a new path to a new middleware. First, open the router config file in `config/autoload/routes.global.php` and add the lines as follow:

    <?php
    
    return [
        'dependencies' => [
            ...
        ],
    
        'routes' => [
            [
                'dependencies' => [
                    'invokables' => [
                        ...
                    ],
                    'factories' => [
                        ...
                        // Add the following line
                        App\Action\HelloWorldAction::class => App\Action\HelloWorldFactory::class,
                    ],
                ],
            ],
            // Following lines should be added
            [
                'name' => 'hello-world',
                'path' => '/hello-world',
                'middleware' => App\Action\HelloWorldAction::class,
                'allowed_methods' => ['GET'],
            ],
        ],
    ];

Put the following content in `src/App/Action/HelloWorldFactory.php`:

    <?php
    
    namespace App\Action;
    
    use Interop\Container\ContainerInterface;
    use Zend\Expressive\Template\TemplateRendererInterface;
    
    class HelloWorldFactory
    {
        public function __invoke(ContainerInterface $container)
        {
            $template = ($container->has(TemplateRendererInterface::class))
                ? $container->get(TemplateRendererInterface::class)
                : null;
    
            return new HelloWorldAction($template);
        }
    }



Then, this content in `src/App/Action/HelloWorldAction.php`:

    <?php
    
    namespace App\Action;
    
    use Psr\Http\Message\ResponseInterface;
    use Psr\Http\Message\ServerRequestInterface;
    use Zend\Diactoros\Response\HtmlResponse;
    use Zend\Diactoros\Response\JsonResponse;
    use Zend\Expressive\Template;
    use Zend\Expressive\Plates\PlatesRenderer;
    use Zend\Expressive\Twig\TwigRenderer;
    use Zend\Expressive\ZendView\ZendViewRenderer;
    
    class HelloWorldAction
    {
        private $template;
    
        public function __construct(Template\TemplateRendererInterface $template = null)
        {
            $this->template = $template;
        }
    
        public function __invoke(ServerRequestInterface $request, ResponseInterface $response, callable $next = null)
        {
            $data = [];
    
            return new HtmlResponse($this->template->render('app::hello-world'));
        }
    }

Then, finally, simply put the following in `templates/app/hello-world.phtml`:

    <?php echo 'Hello World'; ?>

We are done ! Navigate to http://localhost:8080/hello-world, and say "hi" to Zend Expressive !

