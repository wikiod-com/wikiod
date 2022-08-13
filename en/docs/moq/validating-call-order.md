---
title: "Validating call order"
slug: "validating-call-order"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Validating call order implicitly
Where a method to be tested uses information from one call to pass on to subsequent calls, one approach that can be used to ensure the methods are called in the expected order is to setup the expectations to reflect this flow of data.

Given the method to test:

    public void MethodToTest()
    {
        var str = _utility.GetInitialValue();

        str = _utility.PrefixString(str);
        str = _utility.ReverseString(str);

        _target.DoStuff(str);
    }

Expectations can be set to pass data from `GetInitialValue` through `PrefixString` and `ReverseString` to `DoStuff`, where the information is verified.  If any of the methods are called out of order, the end data will be wrong and the test will fail.

    // Create mocks
    var utilityMock = new Mock<IUtility>();
    var targetMock = new Mock<ITarget>();

    // Setup expectations, note that the returns value from one call matches the expected
    // parameter for the next call in the sequence of calls we're interested in.
    utilityMock.Setup(x => x.GetInitialValue()).Returns("string");
    utilityMock.Setup(x => x.PrefixString("string")).Returns("Prefix:string");
    utilityMock.Setup(x => x.ReverseString("Prefix:string")).Returns("gnirts:xiferP");

    string expectedFinalInput = "gnirts:xiferP";

    // Invoke the method to test
    var sut = new SystemUnderTest(utilityMock.Object, targetMock.Object);
    sut.MethodToTest();

    // Validate that the final call was passed the expected value.
    targetMock.Verify(x => x.DoStuff(expectedFinalInput));


## Validating call order with callbacks
When you can't / don't want to use Strict Mocks, you can't use `MockSequence` to validate call order.  An alternate approach is to use callbacks to validate that the `Setup` expectations are being invoked in the expected order.  Given the following method to test:

    public void MethodToTest()
    {
        _utility.Operation1("1111");
        _utility.Operation3("3333");
        _utility.Operation2("2222");
    }

It can be tested as follows:

    // Create the mock (doesn't have to be in strict mode)
    var utilityMock = new Mock<IUtility>();

    // Create a variable to track the current call number
    int callOrder = 1;

    // Setup each call in the sequence to be tested.  Note that the callback validates that
    // that callOrder has the expected value, then increments it in preparation for the next
    // call.
    utilityMock.Setup(x => x.Operation1(It.IsAny<string>()))
               .Callback(() => Assert.AreEqual(1, callOrder++, "Method called out of order") );
    utilityMock.Setup(x => x.Operation2(It.IsAny<string>()))
               .Callback(() => Assert.AreEqual(2, callOrder++, "Method called out of order") );
    utilityMock.Setup(x => x.Operation3(It.IsAny<string>()))
               .Callback(() => Assert.AreEqual(3, callOrder++, "Method called out of order") );


    // Invoke the method to be tested
    var sut = new SystemUnderTest(utilityMock.Object);
    sut.MethodToTest();


    // Validate any parameters that are important, note these Verifications can occur in any
    // order.
    utilityMock.Verify(x => x.Operation2("2222"));
    utilityMock.Verify(x => x.Operation1("1111"));
    utilityMock.Verify(x => x.Operation3("3333"));


## Validating call order with MockSequence
Moq provides support for validating call order using `MockSequence`, however it only works when using Strict mocks.  So, Given the following method to test:


    public void MethodToTest()
    {
        _utility.Operation1("1111");
        _utility.Operation2("2222");
        _utility.Operation3("3333");
    }

It can be tested as follows:

    // Create the mock, not MockBehavior.Strict tells the mock how to behave
    var utilityMock = new Mock<IUtility>(MockBehavior.Strict);

    // Create the MockSequence to validate the call order
    var sequence = new MockSequence();

    // Create the expectations, notice that the Setup is called via InSequence
    utilityMock.InSequence(sequence).Setup(x => x.Operation1(It.IsAny<string>()));
    utilityMock.InSequence(sequence).Setup(x => x.Operation2(It.IsAny<string>()));
    utilityMock.InSequence(sequence).Setup(x => x.Operation3(It.IsAny<string>()));

    // Run the method to be tested
    var sut = new SystemUnderTest(utilityMock.Object);
    sut.MethodToTest();

    // Verify any parameters that are cared about to the operation being orchestrated.
    // Note that the Verify calls can be in any order
    utilityMock.Verify(x => x.Operation2("2222"));
    utilityMock.Verify(x => x.Operation1("1111"));
    utilityMock.Verify(x => x.Operation3("3333"));

The above example uses `It.IsAny<string>` when setting up the expectations.  These could have used relevant strings ("1111", "2222", "3333") if more exact matches were required.

The error reported when calls are made out of sequence can be a bit misleading.

> invocation failed with mock behavior Strict.
All invocations on the mock must have a corresponding setup.

This is because, each `Setup` expectation is treated as if it doesn't exist until the previous expectation in the sequence has been satisfied.

