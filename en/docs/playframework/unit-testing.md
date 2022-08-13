---
title: "Unit Testing"
slug: "unit-testing"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Unit testing - Java, Play 2.4,2.5
Helpers and fakeApplication
===========================
Class *Helpers* is used a lot for unit tests. It imitates a Play application, fakes HTTP requests and responses, session, cookies - all whatever may be needed for tests. A controller under the test should be executed in a context of a Play application. The *Helpers* method *fakeApplication* provides an application for running tests. In order to use *Helpers* and *fakeApplication* a test class should derive from *WithApplication*. 

The following *Helpers* API-s should be used:

    Helpers.running(Application application, final Runnable block);
    Helpers.fakeApplication();
Test with *Helpers* looks like this:

    public class TestController extends WithApplication {
     @Test  
     public void testSomething() {  
         Helpers.running(Helpers.fakeApplication(), () -> {  
             // put test stuff  
             // put asserts  
         });
     }  
    }  
Adding import statements for Helpers methods makes code more compact:

     import static play.test.Helpers.fakeApplication;
     import static play.test.Helpers.running;
     ...
     @Test  
     public void testSomething() {  
          running(fakeApplication(), () -> {  
               // put test stuff  
               // put asserts  
           });
     }
}  

Testing controllers
===================
Let's call a controller method, which is bound to the particular URL in the *routes* as a **routed** method. An invocation of a **routed** method is called a controller **action** and has a Java type *Call*. 
Play builds so-called reverse route to each **action**.  Call to a reverse route creates an appropriate *Call* object. This reverse routing mechanism is used for testing controllers.

To invoke a controller **action** from test the following Helpers API should be used:

    Result result = Helpers.route(Helpers.fakeRequest(Call action));

Controller tests example
------------------------
1. The *routes*:


    GET /conference/:confId    controllers.ConferenceController.getConfId(confId: String)  
    POST /conference/:confId/participant controllers.ConferenceController.addParticipant(confId:String) 
2. Generated reverse routes:

       controllers.routes.ConferenceController.getConfId(conferenceId) 
       controllers.routes.ConferenceController.addParticipant(conferenceId)
3. The method *getConfId* is bound to **GET** and does not receive a body in a request. It may be invoked for test with:

       Result result = Helpers.route(Helpers.fakeRequest(controllers.routes.ConferenceController.getConfId(conferenceId)));
4. The method *addParticipant* is bound to **POST**. It expects to receive a body in a request. Its invocation in test should be done like this:

       ParticipantDetails inputData = DataSimulator.createParticipantDetails();
       Call action = controllers.routes.ConferenceController.addParticipant(conferenceId);
       Result result = route(Helpers.fakeRequest(action).bodyJson(Json.toJson(inputData));

Mocking with PowerMock
======================
To enable mocking a test class should be annotated as following:

    @RunWith(PowerMockRunner.class)
    @PowerMockIgnore({"javax.management.*", "javax.crypto.*"})
    public class TestController extends WithApplication {
    ....

Mocking of a controller action
------------------------------
A controller call is mocked with *RequestBuilder*:

    RequestBuilder fakeRequest = Helpers.fakeRequest(action); 

For the above addParticipant an action is mocked with:

    RequestBuilder mockActionRequest = Helpers.fakeRequest(controllers.routes.ConferenceController.addParticipant(conferenceId)); 
To invoke the controller method:

    Result result = Helpers.route(mockActionRequest);

The whole test:

    @Test
    public void testLoginOK() {
     running(fakeApplication(), () -> {
          ///*whatever mocking*/Mockito.when(...).thenReturn(...);
          RequestBuilder mockActionRequest = Helpers.fakeRequest(
               controllers.routes.LoginController.loginAdmin());
          Result result = route(mockActionRequest);
          assertEquals(OK, result.status());
     });
    }

Mocking of an action with JSON body
-----------------------------------
Let's suppose, that an input is an object of type *T*.
The action request mocking may be done in several ways.

Option 1:

    public static <T> RequestBuilder fakeRequestWithJson(T input, String method, String url) {  
      JsonNode jsonNode = Json.toJson(input);  
      RequestBuilder fakeRequest = Helpers.fakeRequest(method, url).bodyJson(jsonNode);  
      System.out.println("Created fakeRequest="+fakeRequest +", body="+fakeRequest.body().asJson());  
      return fakeRequest;  
    }  
Option 2:

    public static <T> RequestBuilder fakeActionRequestWithJson(Call action, T input) {  
      JsonNode jsonNode = Json.toJson(input);  
      RequestBuilder fakeRequest = Helpers.fakeRequest(action).bodyJson(jsonNode);  
      System.out.println("Created fakeRequest="+fakeRequest +", body="+fakeRequest.body().asJson());  
      return fakeRequest;  
    } 

Mocking of an action with Base authentication header
----------------------------------------------------
The action request mocking:

    public static final String BASIC_AUTH_VALUE = "dummy@com.com:12345";
    public static RequestBuilder fakeActionRequestWithBaseAuthHeader(Call action) {
      String encoded = Base64.getEncoder().encodeToString(BASIC_AUTH_VALUE.getBytes());
      RequestBuilder fakeRequest = Helpers.fakeRequest(action).header(Http.HeaderNames.AUTHORIZATION,
                                                 "Basic " + encoded);
      System.out.println("Created fakeRequest="+fakeRequest.toString() );
      return fakeRequest;
    }

Mocking of an action with session
---------------------------------
The action request mocking:

    public static final String FAKE_SESSION_ID = "12345";
    public static RequestBuilder fakeActionRequestWithSession(Call action) {
      RequestBuilder fakeRequest = RequestBuilder fakeRequest = Helpers.fakeRequest(action).session("sessionId", FAKE_SESSION_ID);
      System.out.println("Created fakeRequest="+fakeRequest.toString() );
      return fakeRequest;
    }
The Play *Session* class is just an extension of the *HashMap<String, String>*. It may be mocked with simple code:

    public static Http.Session fakeSession() {  
      return new Http.Session(new HashMap<String, String>());  
    }


