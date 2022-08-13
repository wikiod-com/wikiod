---
title: "Verify method calls"
slug: "verify-method-calls"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Verify call arguments using ArgumentCaptor
`ArgumentCaptor` will to receive the actual invocation arguments that has been passed to method.

    ArgumentCaptor<Foo> captor = ArgumentCaptor.forClass(Foo.class);
    verify(mockObj).doSomethind(captor.capture());
    Foo invocationArg = captor.getValue();
    //do any assertions on invocationArg

For cases of multiple invocations of mocked method to receive all invocation arguments

    List<Foo> invocationArgs = captor.getAllValues();

The same approach is used for capturing varargs.

Also there is possibility to create `ArgumentCaptor` using `@Captor` annotation:

    @Captor
    private ArgumentCaptor<Foo> captor;




## Simple method call verification
One can verify whether a method was called on a mock by using `Mockito.verify()`.

    Original mock = Mockito.mock(Original.class);
    String param1 = "Expected param value";
    int param2 = 100; // Expected param value

    //Do something with mock

    //Verify if mock was used properly
    Mockito.verify(mock).method();
    Mockito.verify(mock).methodWithParameters(param1, param2);



## Verify order of calls
In some cases it may not suffice to know whether more that one methods were called. The calling order of methods is also important. In such case you may use `InOrder` class of `Mockito` to verify the order of methods.

    SomeClass mock1 = Mockito.mock(SomeClass.class);
    otherClass mock2 = Mockito.mock(OtherClass.class);

    // Do something with mocks

    InOrder order = Mockito.inOrder(mock1, mock2)
    order.verify(mock2).firstMethod();
    order.verify(mock1).otherMethod(withParam);
    order.verify(mock2).secondMethod(withParam1, withParam2);

The `InOrder.verify()` works same as `Mockito.verify()` all other aspects.

