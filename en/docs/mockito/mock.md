---
title: "Mock"
slug: "mock"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Mocking a class using annotations


## "Spy" for partial mocking


## Set private fields in mocked objects
In your class that is under test, you may have some private fields that are not accessible even through constructor. In such cases you can use reflection to set such properties. This is a snippet from such JUnit test.


    @InjectMocks
    private GreetingsService greetingsService = new GreetingsService(); // mocking this class

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        String someName = "Some Name";
        ReflectionTestUtils.setField(greetingsService, // inject into this object
            "name", // assign to this field
            someName); // object to be injected
    }


I'm using Sptring's `ReflectionTestUtils.setField(Object targetObject, String name, Object value)` [method][1] here to simplify, but you can use plain old Java Reflection to do the same.


  [1]: http://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/test/util/ReflectionTestUtils.html#setField-java.lang.Class-java.lang.String-java.lang.Object-

## Simple Mock
Mockito offers a one-size-fits-all mehtod to create mocks of (non-final) classes and interfaces.

    Dependency mock = Mockito.mock(Dependency.class);

This creates a mock instance of `Dependency` regardless of whether `Dependency` is a interface or class.

It is then possible to stub method calls to that mock using the Mockito.when(x).thenReturn(y) notation.

    Mockito.when(mock.possiblyBuggyMethod()).thenReturn("someString");

So that calls to `Dependency.possiblyBuggyMethod()` simply return `"someString"`.

There is another notation that is discouraged in most use cases as it is not typesafe.

    Mockito.doReturn("someString").when(mock).possiblyBuggyMethod()


## Mock with defaults
While a simple mock returns null (or defaults for primitives) to every call, it is possible to change that behaviour.

    Dependency mock = Mockito.mock(Dependency.class, new Answer() {

            @Override
            public Object answer(InvocationOnMock invocationOnMock) throws Throwable {
                return "someString";
            }
        });

or using lambdas:

    Dependency mock = Mockito.mock(Dependency.class, (Answer) invocationOnMock -> "someString");

This examples return "someString" to every invocation but it is possible to define any logic in the answer-method.


