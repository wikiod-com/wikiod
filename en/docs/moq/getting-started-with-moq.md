---
title: "Getting started with moq"
slug: "getting-started-with-moq"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
1. Select the project you want to add the reference to Moq.
2. Open Nuget for this project.
3. Select "Browse" than type "moq" at the search box.
4. Select "Moq" and than click on Install.

[![enter image description here][1]][1]


Following these steps will install the Moq package and will add a reference to it in the selected project references. After completing these steps Moq can be used in the unit test project by simply declaring it in the test classes files:

    Using Moq;

  [1]: http://i.stack.imgur.com/0zQUA.jpg

## Moqs are test doubles
Mocks are meant as test doubles, that allow interactions with the mocks to be validated, they are not meant to replace the system you are testing.  Examples will often demonstrate features of Moq as follows:

    // Create the mock
    var mock = new Mock<IMockTarget>();

    // Configure the mock to do something
    mock.SetupGet(x => x.PropertyToMock).Returns("FixedValue");

    // Demonstrate that the configuration works
    Assert.AreEqual("FixedValue", mock.Object.PropertyToMock);

    // Verify that the mock was invoked
    mock.VerifyGet(x => x.PropertyToMock);

Whilst this example shows the steps involved in using the mock, it is important to remember that it doesn't actually test anything, other than that the mock has been setup and used correctly.  An actual test that makes use of a mock will supply the mock to the system that is to be tested.  To test the following method:

    public class ClassToTest
    {
        public string GetPrefixedValue(IMockTarget provider)
        {
            return "Prefixed:" + provider.PropertyToMock;
        }
    }

It is possible to create a mock of the dependent interface:

    public interface IMockTarget
    {
        string PropertyToMock { get; }
    }

To create a test that actually validates the behaviour of the `GetPrefixedValue` method:

    // Create and configure the mock to return a known value for the property
    var mock = new Mock<IMockTarget>();
    mock.SetupGet(x => x.PropertyToMock).Returns("FixedValue");

    // Create an instance of the class to test
    var sut = new ClassToTest();

    // Invoke the method to test, supplying the mocked instance
    var actualValue = sut.GetPrefixedValue(mock.Object);

    // Validate that the calculated value is correct
    Assert.AreEqual("Prefixed:FixedValue", actualValue);

    // Depending on what your method does, the mock can then be interrogated to
    // validate that the property getter was called.  In this instance, it's
    // unnecessary since we're validating it via the calculated value.
    mock.VerifyGet(x => x.PropertyToMock);


