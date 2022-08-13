---
title: "Getting started with mockito"
slug: "getting-started-with-mockito"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Add behaviour to mocked object
    Mockito.when(mock.returnSomething()).thenReturn("my val");

    mock.returnSomething(); // returns "my val"
    mock.returnSomething(); // returns "my val" again
    mock.returnSomething(); // returns "my val" again and again and again...

If you want different value on second call you can add wanted return argument to thenReturn method:

    Mockito.when(mock.returnSomething()).thenReturn("my val", "other val");

    mock.returnSomething(); // returns "my val"
    mock.returnSomething(); // returns "other val"
    mock.returnSomething(); // returns "other val" again

If you will call method without adding behaviour to mock it will return null:

    barMock.mock.returnSomethingElse(); // returns null

In case that mocked method has parameters, you should declarate values too:

    Mockito.when(mock.returnSomething("param 1")).thenReturn("my val 1");
    Mockito.when(mock.returnSomething("param 2")).thenReturn("my val 2");

    mock.returnSomething("param 1"); // returns "my val 1"
    mock.returnSomething("param 2"); // returns "my val 2"
    mock.returnSomething("param 3"); // returns null

If you don't care about param value you can use Matchers.any():

    Mockito.when(mock.returnSomething(Matchers.any())).thenReturn("p1");

    mock.returnSomething("param 1"); // returns "p1"
    mock.returnSomething("param other"); // returns "p1"

To throw exception use thenThrow method:

    Mockito.when(mock.returnSomething()).thenThrow(new Exception());

    mock.returnSomething(); // throws Exception

## Create objects mocked by Mockito
There are two ways to create object mocked by Mockito:

* via annotation
* via mock function

**Via annotation:**

With a JUnit test runner:

    @RunWith(MockitoJUnitRunner.class)
    public class FooTest {
        @Mock
        private Bar barMock;

        // ...
    }

You can also use Mockito's JUnit `@Rule`, which provides the same functionality as the `MockitoJUnitRunner`, but doesn't need a `@RunWith` test runner:

    public class FooTest {
        @Rule
        public MockitoRule mockito = MockitoJUnit.rule();        

        @Mock
        private Bar barMock;

        // ...
    }

If you can't use `@RunWith` or the `@Rule` annotation you can also init mocks "per hand":

    public class FooTest {
        @Mock
        private Bar barMock;

        @Before
        public void setUp() {
            MockitoAnnotations.initMocks(this);
        }

        // ...
    }

**Via mock function:**

    public class FooTest {
        private Bar barMock = Mockito.mock(Bar.class);

        // ...
    }

Because of type erasure, you cannot mock a generic class as above. You must mock the base class and explicitly cast to the right generic type:

    public class FooTest {
        private Bar<String> genericBarMock = (Bar<String>) Mockito.mock(Bar.class);

        // ...
    }

## Verify method calls on mocked object
To check if a method was called on a mocked object you can use the `Mockito.verify` method:

    Mockito.verify(someMock).bla();

In this example, we assert that the method `bla` was called on the `someMock` mock object.

You can also check if a method was called with certain parameters:

    Mockito.verify(someMock).bla("param 1");

If you would like to check that a method was *not* called, you can pass an additional `VerificationMode` parameter to `verify`:

    Mockito.verify(someMock, Mockito.times(0)).bla();

This also works if you would like to check that this method was called more than once (in this case we check that the method `bla` was called 23 times):

    Mockito.verify(someMock, Mockito.times(23)).bla();

These are more examples for the `VerificationMode` parameter, providing more control over the number of times a method should be called:

    Mockito.verify(someMock, Mockito.never()).bla(); // same as Mockito.times(0)

    Mockito.verify(someMock, Mockito.atLeast(3)).bla(); // min 3 calls

    Mockito.verify(someMock, Mockito.atLeastOnce()).bla(); // same as Mockito.atLeast(1)

    Mockito.verify(someMock, Mockito.atMost(3)).bla(); // max 3 calls

## Simple unit test using Mockito
The class we are going to test is:

    public class Service {
    
        private Collaborator collaborator;
    
        public Service(Collaborator collaborator) {
            this.collaborator = collaborator;
        }
        
        public String performService(String input) {
            return collaborator.transformString(input);
        }
    }

Its collaborator is:

    public class Collaborator {
    
        public String transformString(String input) {
            return doStuff();
        }
    
        private String doStuff() {
            // This method may be full of bugs
            . . .
            return someString;
        }
    
    }

In our test, we want to break the dependency from `Collaborator` and its bugs, so we are going to mock `Collaborator`:

    import static org.junit.Assert.*;
    import static org.mockito.Mockito.*;
    
    import org.junit.Test;
    
    public class ServiceTest {
        @Test
        public void testPerformService() throws Exception {
            // Configure mock
            Collaborator collaboratorMock = mock(Collaborator.class);
            doReturn("output").when(collaboratorMock).transformString("input");
    
            // Perform the test
            Service service = new Service(collaboratorMock);
            String actual = service.performService("input");
            
            // Junit asserts
            String expected = "output";
            assertEquals(expected, actual);
        }  
    }



## Using Mockito annotations
The class we are going to test is:

    public class Service{
    
        private Collaborator collaborator;
    
        public Service(Collaborator collaborator){
            this.collaborator = collaborator;
        }
        
        
        public String performService(String input){
            return collaborator.transformString(input);
        }
    }

Its collaborator is:

    public class Collaborator {
    
        public String transformString(String input){
            return doStuff();
        }
    
        private String doStuff()
        {
            // This method may be full of bugs
            . . .
            return someString;
        }
    
    }

In our test, we want to break the dependency from `Collaborator` and its bugs, so we are going to mock `Collaborator`. Using `@Mock` annotation is a convenient way to create different instances of mocks for each test:

    import static org.junit.Assert.*;
    import static org.mockito.Mockito.*;

    import org.junit.Test;
    import org.junit.runner.RunWith;
    import org.mockito.Mock;
    import org.mockito.InjectMocks;
    import org.mockito.runners.MockitoJUnitRunner;
    
    @RunWith(MockitoJUnitRunner.class)
    public class ServiceTest {

        @Mock
        private Collaborator collaboratorMock;

        @InjectMocks
        private Service service;
        
        @Test
        public void testPerformService() throws Exception {
            // Configure mock
            doReturn("output").when(collaboratorMock).transformString("input");            
    
            // Perform the test
            String actual = service.performService("input");
            
            // Junit asserts
            String expected = "output";
            assertEquals(expected, actual);
        }
        
        
        @Test(expected=Exception.class)
        public void testPerformServiceShouldFail() throws Exception {
            // Configure mock
            doThrow(new Exception()).when(collaboratorMock).transformString("input");
    
            // Perform the test
            service.performService("input");
        }
    }
Mockito will try to resolve dependency injection in the following order: 
1. **Constructor-based injection** - mocks are injected into the constructor with most arguments (if some arguments can not be found, then nulls are passed). If an object was successfully created via constructor, then no other strategies will be applied. 
2. **Setter-based injection** - mocks are injected by type. If there are several properties of the same type, then property names and mock names will be matched. 
3. **Direct field injection** - same as for setter-based injection.

Note that no failure is reported in case if any of the aforementioned strategies failed. 

Please consult the latest [`@InjectMocks`][1] for more detailed information on this mechanism in the latest version of Mockito. 

[1]: http://site.mockito.org/mockito/docs/current/org/mockito/InjectMocks.html

## Installation and setup
## Installation ##

The preferred way to install Mockito is to declare a dependency on `mockito-core` with a build system of choice. As of July 22nd, 2016, the latest non-beta version is 1.10.19, but [2.x is already encouraged to be migrated to][1].

**Maven**

    <dependency>
        <groupId>org.mockito</groupId>
        <artifactId>mockito-core</artifactId>
        <version>1.10.19</version>
        <scope>test</scope>
    </dependency>

**Gradle**

    repositories { jcenter() }
    dependencies { testCompile "org.mockito:mockito-core:1.+" }

There is also `mockito-all` which contains Hamcrest and Objenesis besides Mockito itself.  It is delivered through Maven mainly for ant users, but the distribution has been discontinued in Mockito 2.x.

---

Import
------

The most of the Mockito facilities are static methods of `org.mockito.Mockito`.
Thus, Mockito can be statically imported into a class in this way:

    import static org.mockito.Mockito.*;

Documentation entry point is located in the [javadoc][2] of this class.


  [1]: http://site.mockito.org/mockito/docs/current/org/mockito/Mockito.html#0
  [2]: http://site.mockito.org/mockito/docs/current/org/mockito/Mockito.html

## Mock some methods on an object


## Simple minimal Mockito Test
This example shows a minimal Mockito test using a mocked `ArrayList`:

    import static org.mockito.Mockito.*;
    import static org.junit.Assert.*;

    import java.util.ArrayList;
    
    import org.junit.Test;
    import org.junit.runner.RunWith;
    import org.mockito.Mock;
    import org.mockito.runners.MockitoJUnitRunner;
    
    @RunWith(MockitoJUnitRunner.class)
    public class MockitoTest
    {
        @Mock
        ArrayList<String> listMock;
    
        @Test
        public void testAppend() {
            // configure the mock to return "foobar" whenever "get()" 
            // is called on "listMock" with an int value as parameter
            doReturn("foobar").when(listMock).get(anyInt());            
            String result = listMock.get(0);
            
            assertEquals("foobar", result);
        }
    }

## Verifying arguments with ArgumentCaptor
To validate arguments to methods called on a mock, use the `ArgumentCaptor` class. This will allow you to extract the arguments into your test method and perform assertions on them.

This example tests a method which updates the name of a user with a given ID. The method loads the user, updates the `name` attribute with the given value and saves it afterwards. The test wants to verify that the argument passed to the `save` method is a `User` object with the correct ID and name.

    // This is mocked in the test
    interface UserDao {
        void save(User user);
    }

    @RunWith(MockitoJUnitRunner.class)
    public class UserServiceTest {
        @Mock
        UserDao userDao;

        @Test
        public void testSetNameForUser() {
            UserService serviceUnderTest = new UserService(userDao);
            
            serviceUnderTest.setNameForUser(1L, "John");
    
            ArgumentCaptor<User> userArgumentCaptor = ArgumentCaptor.forClass(User.class);
            
            verify(userDao).save(userArgumentCaptor.capture());
            User savedUser = userArgumentCaptor.getValue();
            assertTrue(savedUser.getId() == 1);
            assertTrue(savedUser.getName().equals("John"));
        }
    }

## Verifying Arguments with ArgumentMatcher
Mockito provides a `Matcher<T>` interface along with an abstract `ArgumentMatcher<T>` class to verify arguments. It uses a different approach to the same use-case than the `ArgumentCaptor`. Additionally the ArgumentMatcher can be used in mocking too. Both use-cases make use of the `Mockito.argThat()` method that provides a reasonably readable test code.

    verify(someMock).someMethod(Mockito.argThat(new ArgumentMatcher<String>() {
           
        @Override
        public boolean matches(Object o) {
            return o instanceof String && !((String)o).isEmpty();
        }

    });        


----------


From the JavaDocs of ArgumentMatcher: 

**Warning:** Be reasonable with using complicated argument matching, especially custom argument matchers, as it can make the test less readable. Sometimes it's better to implement equals() for arguments that are passed to mocks (Mockito naturally uses equals() for argument matching). This can make the test cleaner. 

## Stubbing void methods


## Check arguments passed to mock


