---
title: "Integration testing on existing routes with Apache-Camel and Spring (And DBUnit)"
slug: "integration-testing-on-existing-routes-with-apache-camel-and-spring-and-dbunit"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

The point of this wiki is to show you how to execute integration tests using Apache Camel.

More precisely, doing this you will be able to launch an existing route from beginning to end (With or without your real database) or intercept the exchange between each part of the route and test if your headers or body are correct or not.

The project I have been doing this on uses classic Spring with xml configuration and DBUnit to mock a test database. Hope this will give you a few leads.




  

## Parameters
| Parameter/Function | Details |
| ------ | ------ |
| Exchange   | The exchange is used inside the camel processor to pass objects between parts of your route   |
| CamelContext | The camel context is used in the test to manually start and stop the context.|
| ProducerTemplate  | Allows you to send messages in your route, setting the complete exchange manually or sending dummy headers/body  |
| AdviceWith | Helps you redefine an existing route with the current context  |
| WeaveById | Used inside the advice with configuration, tells pieces of your route how to behave (Can also use *weaveByToString*) |
| MockEndpoint | The mockendpoint is a point you define for your test. In your weaveById, you can tell your route to its usual processing and go into a mockEnpoint rather than following the usual route. This way you can check the message count, the exchange status ... |

Some definitions given here are not perfectly accurate, but they will help you understand the code above. Here are a few links for more detailed information :

 - About the use of *AdviceWith* and
   *weaveById* (Or other ways to trigger routes), have a look at the official apache-camel documentation : [see this link][1]
 - About the use of *ProducerTemplate*, see the official documentation again : [see this link][2]
   
  - To really understand what camel is all about : [Entreprise
   Integration Patterns detailed documentation][3]

This particular way of testing is pretty hard to find, even on stack overflow. This is pretty specific but don't hesitate to ask for more details, maybe I'll be able to help.

  [1]: http://camel.apache.org/advicewith.html
  [2]: http://camel.apache.org/producertemplate.html
  [3]: http://camel.apache.org/enterprise-integration-patterns.html

## Camel route example
The following route has a simple goal :

 - First, it checks if and **ImportDocumentProcess** object is present in the database and adds it as an *exchange header*
 - Then, it adds an **ImportDocumentTraitement** (Which is linked to the previous ImportDocumentProcess) in the database

Here is the code of this route : 

    @Component
    public class TestExampleRoute extends SpringRouteBuilder {
   
        public static final String ENDPOINT_EXAMPLE = "direct:testExampleEndpoint";
    
        @Override
        public void configure() throws Exception {
            from(ENDPOINT_EXAMPLE).routeId("testExample")
                .bean(TestExampleProcessor.class, "getImportDocumentProcess").id("getImportDocumentProcess")
                .bean(TestExampleProcessor.class, "createImportDocumentTraitement").id("createImportDocumentTraitement")
                .to("com.pack.camel.routeshowAll=true&multiline=true");
        }
    
    }

The *id* on the routes are not mandatory, you can use the bean strings afterwards too. However I think using *ids* can be considered a good practice, in case your route strings change in the future.

## Camel Processor example
The processor just contains just contains the methods needed by the route. It is just a classic Java Bean containing several methods. You can also *implement Processor* and override the *process* method.

See the code below :

    @Component("testExampleProcessor")
    public class TestExampleProcessor {
    
        private static final Logger LOGGER = LogManager.getLogger(TestExampleProcessor.class);
    
        @Autowired
        public ImportDocumentTraitementServiceImpl importDocumentTraitementService;
    
        @Autowired
        public ImportDocumentProcessDAOImpl importDocumentProcessDAO;
    
        @Autowired
        public ImportDocumentTraitementDAOImpl importDocumentTraitementDAO;
    
        // ---- Constants to name camel headers and bodies
        public static final String HEADER_ENTREPRISE = "entreprise";

        public static final String HEADER_UTILISATEUR = "utilisateur";
    
        public static final String HEADER_IMPORTDOCPROCESS = "importDocumentProcess";
    
        public void getImportDocumentProcess(@Header(HEADER_ENTREPRISE) Entreprise entreprise, Exchange exchange) {
            LOGGER.info("Entering TestExampleProcessor method : getImportDocumentProcess");
    
            Utilisateur utilisateur = SessionUtils.getUtilisateur();
            ImportDocumentProcess importDocumentProcess = importDocumentProcessDAO.getImportDocumentProcessByEntreprise(
                    entreprise);
    
            exchange.getIn().setHeader(HEADER_UTILISATEUR, utilisateur);
            exchange.getIn().setHeader(HEADER_IMPORTDOCPROCESS, importDocumentProcess);
        }
    
        public void createImportDocumentTraitement(@Header(HEADER_ENTREPRISE) Entreprise entreprise,
                @Header(HEADER_UTILISATEUR) Utilisateur utilisateur,
                @Header(HEADER_IMPORTDOCPROCESS) ImportDocumentProcess importDocumentProcess, Exchange exchange) {
            LOGGER.info("Entering TestExampleProcessor method : createImportDocumentTraitement");
    
            long nbImportTraitementBefore = this.importDocumentTraitementDAO.countNumberOfImportDocumentTraitement();
            ImportDocumentTraitement importDocumentTraitement = this.importDocumentTraitementService.createImportDocumentTraitement(
                    entreprise, utilisateur, importDocumentProcess, "md5_fichier_example_test", "fichier_example_test.xml");
            long nbImportTraitementAfter = this.importDocumentTraitementDAO.countNumberOfImportDocumentTraitement();
    
            exchange.getIn().setHeader("nbImportTraitementBefore", Long.valueOf(nbImportTraitementBefore));
            exchange.getIn().setHeader("nbImportTraitementAfter", Long.valueOf(nbImportTraitementAfter));
            exchange.getIn().setHeader("importDocumentTraitement", importDocumentTraitement);
        }
    // Rest of the code contains getters and setters for imported dependencies
    }

Not much to say here, except that we use the exchange to transfer objects from one part to another. This is the way it is usually done on my project, since we have really complex processes to handle.

## Camel Integration test class example
> Don't forget to add the camel test support and spring camel test
> support to your project dependencies. See the following for maven users :

    <dependency>
      <groupId>org.apache.camel</groupId>
      <artifactId>camel-test</artifactId>
      <version>${camel.version}</version>
      <scope>test</scope>
    </dependency>
    <dependency>
        <groupId>org.apache.camel</groupId>
        <artifactId>camel-test-spring</artifactId>
        <version>${camel.version}</version>
         <scope>test</scope>
    </dependency>


----------


This class is going to trigger and run tests on the example route. These tests also use **DBUnit** to simulate a database altough you can configure your context to use a real or other kind of mocked database. 

First, we use an abstract class in order to share common annotations between each Camel Integration Test class we'll later use :

    @RunWith(CamelSpringRunner.class)
    @BootstrapWith(CamelTestContextBootstrapper.class)
    @ContextConfiguration(locations = { "classpath:/test-beans.xml" })
    @DbUnitConfiguration(dataSetLoader = ReplacementDataSetLoader.class)
    @TestExecutionListeners({ DependencyInjectionTestExecutionListener.class, DirtiesContextTestExecutionListener.class,
            DbUnitTestExecutionListener.class })
    @DirtiesContext(classMode = ClassMode.AFTER_EACH_TEST_METHOD)
    public abstract class AbstractCamelTI {
    
    }

**Careful not to forget any annotation** or your DAOs won't be injected correctly. That being said, you can safely remove the DBUnit annotations if you don't want to use the database depicted in your context configuration.

**IMPORTANT EDIT** : I have added the ```@DirtiesContext(classMode = ClassMode.AFTER_EACH_TEST_METHOD)``` recently. That way, the *camel context* is reloaded for each test. You can really test each part of your route individually.
However, if you really want that, you need to use *remove()* on the parts of the chosen route you don't want to go through. Some would argue that this is not a real Integration test, and they would be right. But If, like me, you have large processors you need to refactor, you can start there.

The code below depicts the test class beginning (See down bellow for the actual tests) :

    @DatabaseSetup(value = { "/db_data/dao/common.xml", "/db_data/dao/importDocumentDAOCommonTest.xml" })
    public class TestExampleProcessorTest extends AbstractCamelTI {
    
        @Autowired
        protected CamelContext camelContext;
    
        @EndpointInject(uri = "mock:catchTestEndpoint")
        protected MockEndpoint mockEndpoint;
    
        @Produce(uri = TestExampleRoute.ENDPOINT_EXAMPLE)
        protected ProducerTemplate template;
    
        @Autowired
        ImportDocumentTraitementDAO importDocumentTraitementDAO;
    
        // -- Variables for tests
        ImportDocumentProcess importDocumentProcess;
    
        @Override
        @Before
        public void setUp() throws Exception {
            super.setUp();
    
            importDocumentProcess = new ImportDocumentProcess();
            //specific implementation of your choice
        }
    }


The following test is supposed to trigger the first part of the route and lead it to a `mockEndpoint` so we can test if the  *ImportDocumentProcess* has been correctly selected and put into the headers :

    @Test
    public void processCorrectlyObtained_getImportDocumentProcess() throws Exception {
        camelContext.getRouteDefinitions().get(0).adviceWith(camelContext, new AdviceWithRouteBuilder() {

            @Override
            public void configure() throws Exception {
                weaveById("getImportDocumentProcess").after().to(mockEndpoint);
            }
        });

        // -- Launching the route
        camelContext.start();
        template.sendBodyAndHeader(null, "entreprise", company);

        mockEndpoint.expectedMessageCount(1);
        mockEndpoint.expectedHeaderReceived(TestExampleProcessor.HEADER_UTILISATEUR, null);
        mockEndpoint.expectedHeaderReceived(TestExampleProcessor.HEADER_IMPORTDOCPROCESS, importDocumentProcess);
        mockEndpoint.assertIsSatisfied();

        camelContext.stop();
    }



The last test triggers the whole route :

    @Test
    public void traitementCorrectlyCreated_createImportDocumentTraitement() throws Exception {
        camelContext.getRouteDefinitions().get(0).adviceWith(camelContext, new AdviceWithRouteBuilder() {
    
            @Override
            public void configure() throws Exception {
                weaveById("createImportDocumentTraitement").after().to(mockEndpoint);
            }
        });
    
        // -- Launching the route
        camelContext.start();
    
        Exchange exchange = new DefaultExchange(camelContext);
        exchange.getIn().setHeader(TestExampleProcessor.HEADER_ENTREPRISE, company);
        exchange.getIn().setHeader(TestExampleProcessor.HEADER_UTILISATEUR, null); // No user in this case
        exchange.getIn().setHeader(TestExampleProcessor.HEADER_IMPORTDOCPROCESS, importDocumentProcess);
    
        long numberOfTraitementBefore = this.importDocumentTraitementDAO.countNumberOfImportDocumentTraitement();
    
        template.send(exchange);
    
        mockEndpoint.expectedMessageCount(1);
        mockEndpoint.assertIsSatisfied();
    
        camelContext.stop();
    
        long numberOfTraitementAfter = this.importDocumentTraitementDAO.countNumberOfImportDocumentTraitement();
        assertEquals(numberOfTraitementBefore + 1L, numberOfTraitementAfter);
    }


It is also possible to redirect the current route to another process. But I prefer redirecting to a `mockEndpoint`. It is a bit more interesting because you can really do intermediate tests on your exchange body and headers.


----------


**IMPORTANT NOTE** : In this example I am using the following piece of code to get my routes and use ```adviceWith``` on them : 

    camelContext.getRouteDefinitions().get(0).adviceWith(camelContext, new AdviceWithRouteBuilder() { [...] });

**HOWEVER** It is possible to get the route by an ID previously defined as a string, like this : 

     camelContext.getRouteDefinition("routeId").adviceWith(camelContext, new AdviceWithRouteBuilder() {  [...]  });

I highly recommend this method, it can save a lot of time figuring out where why your tests are failing

