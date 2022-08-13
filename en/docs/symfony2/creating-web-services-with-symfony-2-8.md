---
title: "Creating Web-Services with Symfony 2.8"
slug: "creating-web-services-with-symfony-28"
draft: false
images: []
weight: 9877
type: docs
toc: true
---

## Work with RESTFul API
REpresentational State Transfer (REST) is an architectural style used for web development, introduced and defined in 2000 by Roy Fielding.

See it on wiki : [REST wiki][1]

It's based on HTTP protocol ([HTTP on Wiki][2]), HTTP requests (GET, POST, PATCH, DELETE...) / responses codes (404, 400, 200, 201, 500...) and bodies structure.

This is a great way to expose your datas to an another system on Internet.

Imagine you want to make a RESTFul api to manage your StackOverFlower (User) on your local database. 

*Let's make the example !*

## Symfony 2.8 framework ##

 1. Web server :

You must install and configure a web server on your local machine, see [Wamp][3] or [Lamp][4] or [Mamp][5] : You must have a recent version of PHP (**!!! Symfony requirements !!!**)

 2. Php cli and Composer :

You must configure PHP cli (varying on our system), type this "PHP cli [OS-NAME] how-to" in our friend Google!
You must install composer, see [Composer install][6]

 3. Symfony :

You must install Symfony 2.8 (with composer, it's the better way), open a terminal (or cmd on windows) and go to your web server path.

Symfony 2 works with the one of the better structure types: Bundles. All are Bundles on Symfony! We can test it above.

    cd /your-web-server-path/
    composer create-project symfony/framework-standard-edition example "2.8.*"

Go to the tree structure an see : Symfony 2.8 is installed on "example" directory.

 4. FOSRest (for FriendsOfSymfony) on JMSSerializer Bundle :

You must install these two Bundles :

JMSSerializer ([Install][7]) : 

    composer require jms/serializer-bundle "~0.13"

FosRestBundle ([Install][8]) :

    composer require friendsofsymfony/rest-bundle

**Don't forget to activate them in AppKernel.php !**

 5. Basic configuration :

Make your own "Example" bundle and create the database.

    cd /path/to/your/symfony/
    php app/console generate:bundle
    php app/console doctrine:generate:database

Go to the bottom of your Symfony 2.8 application configuration file, and paste it :

    #app/config/config.yml
    fos_rest:
        format_listener:
            rules:
                - { path: '^/stackoverflower', priorities: ['xml', 'json'], fallback_format: xml, prefer_extension: true }
                - { path: '^/', priorities: [ 'text/html', '*/*'], fallback_format: html, prefer_extension: true }

Make your doctrine directory ("example/src/ExampleBundle/Entity") and resource file ("StackOverFlower.orm.yml") :

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

Generate Entity and Update Schema :

    php app/console doctrine:generate:entity StackOverFlower
    php app/console doctrine:schema:update --force

Make a default controller :

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
            
            if (count($errors_list) == 0) {
                
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
            
            if (count($users) === 0) {
                $view = $this->view("No StackOverFlower found.", 404);
                return $this->handleView();
            }
            
            $view = $this->view($users)
                         ->setTemplateVar('users')
                         ->setTemplate($template);
            
            return $this->handleView($view);
        }
    }

Make your default Twig view :

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

You have just made your first RESTFul API!

You can test it on : http://your-server-name/your-symfony-path/app_dev.php/stackoverflower/new/test.

As you can see in the databse, a new user has been created with the name "test".

You can get the list of stackoverflower on : http://your-server-name/your-symfony-path/app_dev.php/stackoverflowers

You have a full example on my github account of this example : [Git Hub example][9], at the "master" branch this example, and on the "real-routes" branche an example with more appropriate URL (like POST and DELETE). 

See you later for an example with SOAP!

Best Regards,

Mathieu

  [1]: https://en.wikipedia.org/wiki/Representational_state_transfer
  [2]: https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol
  [3]: http://www.wampserver.com/en/
  [4]: https://doc.ubuntu-fr.org/lamp
  [5]: https://www.mamp.info/en/
  [6]: https://getcomposer.org/download/
  [7]: http://jmsyst.com/bundles/JMSSerializerBundle/master/installation
  [8]: http://symfony.com/doc/1.5/bundles/FOSRestBundle/1-setting_up_the_bundle.html
  [9]: https://github.com/weenesta/symfony-2.8-example-rest

## Work with SOAP API
SOAP (Simple Access Object Protocol) is XML based, like XML-RPC, *is ancestor*, with file called **WSDL**, what describe the method to be exposed.

This protocol is often based with **SOAP-Enveloppe**, a **SOAP-Body**, and alternatively **SOAP-Header**, the data is envelopped in a structure and be interpreted as the same way from different langages.

[![Description of a Soap message][1]][1]

For more information, see : [SOAP on wiki][2]

As described above, the most important to describe your web service is the **WSDL** file, see : [WSDL explanation on wiki][3]

The basic of the work will be to define what is exposed on your SOAP API, your class and your business process will be automatically handled by the basic PHP [SOAPServer][4] class. You still need the code!

Let's see how the file is constructed :

 1. Service : Set the API URI and what will be associated.
 2. Binding : It define the operations associated with the service
 3. Operations : Some methods you want to expose to the Web
 4. PortTypes : Define queries and responses
 5. Requests and Responses : what you expect input and output
 6. Messages : what formt you expect (parameters) on each IO, they can be simple (string, integer, float...) or complex type (structured format)

With this basic information, you can achieve all API you want.

Imagine you want to make a SOAP api to manage your StackOverFlower (User) on your local database.

*Let's make the example !*

Install Web server, Php cli, Composer, Symfony 2.8, create a new Bundle "ExampleBundle" and build the schema like described above.

Before we start to build our business logic, we had to know what to expose of our controller. This job is done by using the WSDL. This is an example of a good syntax of an WSDL :

    <definitions name="StackOverFlowerService"
       targetNamespace="http://example/soap/stackoverflower.wsdl"
       xmlns="http://schemas.xmlsoap.org/wsdl/"
       xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
       xmlns:tns="http://example/soap/stackoverflower.wsdl"
       xmlns:xsd="http://www.w3.org/2001/XMLSchema">
     
       <message name="NewRequest">
          <part name="name" type="xsd:string"/>
       </message>
        
       <message name="NewResponse">
          <part name="status" type="xsd:string"/>
       </message>
    
       <message name="getListRequest"></message>
        
       <message name="getListResponse">
          <part name="list" type="xsd:string"/>
       </message>
    
       <message name="editRequest">
          <part name="id" type="xsd:string"/>
          <part name="name" type="xsd:string"/>
       </message>
        
       <message name="editResponse">
          <part name="status" type="xsd:string"/>
       </message>
       
       <message name="deleteRequest">
          <part name="id" type="xsd:string"/>
       </message>
        
       <message name="deleteResponse">
          <part name="status" type="xsd:string"/>
       </message>
       
       <portType name="StackOverFlower_PortType">
          <operation name="newStack">
             <input message="tns:NewRequest"/>
             <output message="tns:NewResponse"/>
          </operation>
          <operation name="getList">
             <input message="tns:getListRequest"/>
             <output message="tns:getListResponse"/>
          </operation>
          <operation name="edit">
             <input message="tns:editRequest"/>
             <output message="tns:editResponse"/>
          </operation>
          <operation name="delete">
             <input message="tns:deleteRequest"/>
             <output message="tns:deleteResponse"/>
          </operation>
       </portType>
    
       <binding name="StackOverFlower_Binding" type="tns:StackOverFlower_PortType">
          <soap:binding style="rpc"
             transport="http://schemas.xmlsoap.org/soap/http"/>
          <operation name="newStack">
             <soap:operation soapAction="newStack"/>
             <input>
                <soap:body
                   encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"
                   namespace="urn:example:new"
                   use="encoded"/>
             </input>
            
             <output>
                <soap:body
                   encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"
                   namespace="urn:example:new"
                   use="encoded"/>
             </output>
          </operation>
          
          <operation name="getList">
             <soap:operation soapAction="getList"/>
             <input>
                <soap:body
                   encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"
                   namespace="urn:example:get-list"
                   use="encoded"/>
             </input>
            
             <output>
                <soap:body
                   encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"
                   namespace="urn:example:get-list"
                   use="encoded"/>
             </output>
          </operation>
          
          <operation name="edit">
             <soap:operation soapAction="edit"/>
             <input>
                <soap:body
                   encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"
                   namespace="urn:example:edit"
                   use="encoded"/>
             </input>
            
             <output>
                <soap:body
                   encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"
                   namespace="urn:example:edit"
                   use="encoded"/>
             </output>
          </operation>
          
          <operation name="delete">
             <soap:operation soapAction="delete"/>
             <input>
                <soap:body
                   encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"
                   namespace="urn:example:delete"
                   use="encoded"/>
             </input>
            
             <output>
                <soap:body
                   encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"
                   namespace="urn:example:delete"
                   use="encoded"/>
             </output>
          </operation>
       </binding>
    
       <service name="StackOverFlower_Service">
          <documentation>Description File of StackOverFlowerService</documentation>
          <port binding="tns:StackOverFlower_Binding" name="StackOverFlower_Port">
             <soap:address
                location="http://example/stackoverflower/" />
          </port>
       </service>
    </definitions>

We must take this on your web symfony directory (in soap subdirectory, and name this "stackoverflower.wsdl").

Really inspired from [WSDl Example][5]. You can validate that with an [Online WSDl Validator][6]

After this, we can make our basic service and controller, inspirated from [SOAP Symfony 2.8 Doc][7]. 

Service, that is handled by PHP SOAPServer :

    #src\ExampleBundle\Services\StackOverFlowerService.php
    namespace ExampleBundle\Services;
    
    use Doctrine\ORM\EntityManager;
    use Symfony\Component\Serializer\Serializer;
    use Symfony\Component\Serializer\Encoder\XmlEncoder;
    use Symfony\Component\Serializer\Encoder\JsonEncoder;
    use Symfony\Component\Serializer\Normalizer\ObjectNormalizer;
    
    use ExampleBundle\Entity\StackOverFlower;
    
    class StackOverFlowerService
    {
      private $em;
      private $stackoverflower;
    
      public function __construct(EntityManager $em)
      {
        $this->em = $em;
      }
    
      public function newStack($name)
      {
        $stackoverflower = new StackOverFlower();
        $stackoverflower->setName($name);
        
        $this->em->persist($stackoverflower);
        $this->em->flush();
        
        return "ok";
      }
      
      public function getList()
      {
        $stackoverflowers = $this->em->getRepository("ExampleBundle:StackOverFlower")->findAll();
        
        $encoders = array(new XmlEncoder(), new JsonEncoder());
        $normalizers = array(new ObjectNormalizer());
    
        $serializer = new Serializer($normalizers, $encoders);
        
        return $serializer->serialize($stackoverflowers, 'json');
      }
      
      public function edit($id, $name)
      {
        $stackoverflower = $this->em->getRepository("ExampleBundle:StackOverFlower")->findOneById($id);
        
        $stackoverflower->setName($name);
        
        $this->em->persist($stackoverflower);
        $this->em->flush();
        
        return "ok";
      }
        
      public function delete($id)
      {
        $stackoverflower = $this->em->getRepository("ExampleBundle:StackOverFlower")->findOneById($id);
        
        $this->em->remove($stackoverflower);
        $this->em->flush();
        
        return "ok";
      }
    }

Configure this service :

    #src\ExampleBundle\Resources\config\services.yml
    services:
      stackoverflower_service:
        class: ExampleBundle\Services\StackOverFlowerService
        arguments: [@doctrine.orm.entity_manager]

As you can see, we inject the Doctrine Entity Manger as a dependency because we have to use this to CRUD StackOverFlower Object. 

Controller, that expose the service object :

    #src\ExampleBundle\Controller\StackOverFlowerController.php
    namespace ExampleBundle\Controller;
    
    use Symfony\Bundle\FrameworkBundle\Controller\Controller;
    use Symfony\Component\HttpFoundation\Request;
    use Symfony\Component\HttpFoundation\Response;
    
    class StackOverFlowerController extends Controller
    {
      public function indexAction()
      {
        ini_set("soap.wsdl_cache_enabled", "0");
    
        $options = array(
          'uri' => 'http://example/app_dev.php/soap',
          'cache_wsdl' => WSDL_CACHE_NONE, 
          'exceptions' => true
        );
      
        $server = new \SoapServer(dirname(__FILE__).'/../../../**web/soap/stackoverflower.wsdl**', $options);
        $server->setObject($this->get('stackoverflower_service'));
    
        $response = new Response();
        $response->headers->set('Content-Type', 'text/xml; charset=utf-8');
    
        ob_start();
        $server->handle();
        $response->setContent(ob_get_clean());
    
        return $response;
      }
    }

To learn more about services, see :[Service container on Symfony doc][8]

The route :

    example_soap:
      path:     /soap
      defaults: { _controller: ExampleBundle:StackOverFlower:index }

The basic Twig Template :

    #src\ExampleBundle\Resources\views\Soap\default.html.twig
    {% if status is defined %}
    {{ status }}
    {% else %}
    {{ list }}
    {% endif %}

**We have made your first SOAP API with Symfony 2.8 !**

Before you expose it, we have to test !!

In your StackOverFlowerController, add this :

      public function testNewAction(Request $request)
      {
        $service = $this->get('stackoverflower_service');
        $result = $service->newStack($request->query->get('name'));
        
        return $this->render('ExampleBundle:Soap:default.html.twig', array('status' => $result));
      }
      
      public function testEditAction(Request $request)
      {
        $service = $this->get('stackoverflower_service');
        $result = $service->edit($request->query->get('id'), $request->query->get('name'));
        
        return $this->render('ExampleBundle:Soap:default.html.twig', array('status' => $result));
      }
      
      public function testGetListAction(Request $request)
      {
        $service = $this->get('stackoverflower_service');
        $result = $service->getList();
        
        return $this->render('ExampleBundle:Soap:default.html.twig', array('list' => $result));
      }
      
      public function testDeleteAction(Request $request)
      {
        $service = $this->get('stackoverflower_service');
        $result = $service->delete($request->query->get('id'));
        
        return $this->render('ExampleBundle:Soap:default.html.twig', array('list' => $result));
      }

    // To test this from an another server, you can type this :
    // $client = new \SoapClient("http://example/app_dev.php/soap?wsdl", array("trace" => 1, "exception" => 1)); 
    // $result = $client->newStack($request->query->get('name'));
    // print_r($result); 

The routes :

    test_new:
      path:     /stackoverflower/new
      defaults: { _controller: ExampleBundle:StackOverFlower:testNew }
      
    test_edit:
      path:     /stackoverflower/edit
      defaults: { _controller: ExampleBundle:StackOverFlower:testEdit }
      
    test_get_list:
      path:     /stackoverflower/get-list
      defaults: { _controller: ExampleBundle:StackOverFlower:testGetList }
      
    test_delete:
      path:     /stackoverflower/delete
      defaults: { _controller: ExampleBundle:StackOverFlower:testDelete }

You can type this in your browser :

 1. [getList][9]
 2. [new][10]
 3. [edit][11]
 4. [delete][12]

This is a very basic example of a non secured API with SOAP, I can do an example of a secured example behind a api key authentication later.

That all folks...

Mathieu

  [1]: http://i.stack.imgur.com/aVAyU.png
  [2]: https://en.wikipedia.org/wiki/SOAP
  [3]: https://en.wikipedia.org/wiki/Web_Services_Description_Language
  [4]: http://php.net/manual/en/soapserver.soapserver.php
  [5]: http://www.tutorialspoint.com/wsdl/wsdl_example.htm
  [6]: https://www.wsdl-analyzer.com
  [7]: http://symfony.com/doc/2.8/controller/soap_web_service.html
  [8]: http://symfony.com/doc/2.8/service_container.html
  [9]: http://example/app_dev.php/stackoverflower/get-list
  [10]: http://example/app_dev.php/stackoverflower/new?name=test
  [11]: http://example/app_dev.php/stackoverflower/edit?id=1&name=test1
  [12]: http://example/app_dev.php/stackoverflower/delete?id=1

