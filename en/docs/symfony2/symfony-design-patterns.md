---
title: "Symfony Design Patterns"
slug: "symfony-design-patterns"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Dependency Injection pattern
Imagine you have a class manager to manages sending mails (be called MailManager).
 
In this, you have to log mails that are sent. A good solution is to transform the MailManager class into a `service` and then inject class for creating logs (`Monolog` for example) into the MailManager creating a service.

To do this :

 1- Declare future MailManager class as service (in services.yml)

    services:
        mail.manager.class:
            class:     Vendor/YourBundle/Manager/MailManager
           
2- Inject [logger][1] existant service using `argument` method

    services:
        mail.manager.class:
            class:    Project/Bundle/Manager/MailManager
            arguments: ["@logger"]    # inject logger service into constructor

3- Create MailManager class

    <?php

    namespace Project\Bundle\Manager;
    
    use Symfony\Component\HttpKernel\Log\LoggerInterface;
    
    class MailManager
    {
      protected $logger;
    
      //initialized logger object
      public function __construct(LoggerInterface $logger)
      {
         $this->logger = $logger;
      }

       public function sendMail($parameters)
       {
          //some codes to send mail
          
          //example using logger
          $this->logger->info('Mail sending');
       }
    }

4- Call MailManager in a Controller for example

    <?php

    class TestController extends Controller
    {
        public function indexAction()
        {
            //some codes...

            //call mail manager service
            $mailManager = $this->get('mail.manager.class');
            //call 'sendMail' function from this service
            $mailManager->sendMail($parameters);
            
        }
    }



  [1]: http://symfony.com/doc/current/logging.html

