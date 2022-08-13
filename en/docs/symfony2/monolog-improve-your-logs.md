---
title: "Monolog  improve your logs"
slug: "monolog--improve-your-logs"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Add user's details and posted parameters sent to logs
Logs are very important. Recreate an error context can be sometimes very painful due to the lack of information about how and when the error occurred. 

This example shows:

 - How to add user's data in the error logs 
 - How to add post parameters
   sent when an error occurred 
 - How to use [WebProcessor][1] in order to
   add all data regarding the request like :
   
    - url
    - ip
    - http method
    - server
    - referrer

**Service Configuration**

    services:          
        # Permits to convert logs in HTML format for email notification
        monolog.formatter.html:
            class: Monolog\Formatter\HtmlFormatter
    
        # Add request data (url, ip, http method, server, referrer)
        monolog.processor.web_processor:
            class: Monolog\Processor\WebProcessor
            tags:
                - { name: monolog.processor, method: __invoke } 
    
        # Custom class to include user's data and posted parameters in the logs
        monolog.processor.user:
            class: Company\ToolBoxBundle\Services\Monolog\ExtraProcessor
            arguments:  ["@security.token_storage"]
            tags:
                - { name: monolog.processor }
                - { name: kernel.event_listener, event: kernel.request, method: onKernelRequest }    

**Service code**

    namespace Company\ToolBoxBundle\Services\Monolog;
    
    use Symfony\Component\HttpKernel\Event\GetResponseEvent;
    use Symfony\Component\Security\Core\Authentication\Token\Storage\TokenStorageInterface;
    
    class ExtraProcessor
    {
        /**
        * @var string
        */
        private $postParams = null;    
    
        /**
        * @var TokenStorageInterface
        */
        private $tokenStorage = null;

        /**
        * @var \Company\UserBundle\Entity\User
        */
        private $user = null;
    
        public function __construct(TokenStorageInterface $tokenStorage)
        {
            $this->tokenStorage = $tokenStorage;
        }
            
        // Called when an error occurred and a log (record) is creating
        public function __invoke(array $record)
        {
            if (null !== $this->user) {
                // $this->user is your user's entity. Extract all pertinent data you would need. In this case, getUserDetails method create a summary including alias, name, role, ...
                $record['extra']['user'] = $this->user->getUserDetails();
            }
    
            if (null !== $this->postParams) {
                // Includes all posted parameter when the error occurred
                $record['extra']['postParams'] = $this->postParams;
            }
    
            return $record;
        }
    
        public function onKernelRequest(GetResponseEvent $event)
        {
            // Retain post parameters sent (serialized) in order to log them if needed
            $postParams = $event->getRequest()->request->all();
            if(false === empty($postParams)){
                $this->postParams = serialize($postParams);
            }  
    
            // Do not continue if user is not logged
            if (null === $token = $this->tokenStorage->getToken()) {
                return;
            }
    
            if (!is_object($user = $token->getUser())) {
                // e.g. anonymous authentication
                return;
            }
    
            // Retain the user entity in order to use it
            $this->user = $user;
        }
    }


  [1]: https://github.com/Seldaek/monolog/blob/master/src/Monolog/Processor/WebProcessor.php

