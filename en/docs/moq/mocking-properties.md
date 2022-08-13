---
title: "Mocking properties"
slug: "mocking-properties"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Auto stubbing properties
Sometimes you want to mock a class or an interface and have its properties behave as if they were simple getters and setters.  As this is a common requirement, Moq provides a short cut method to setup all properties of a mock to store and retrieve values:

    // SetupAllProperties tells mock to implement setter/getter funcationality
    var userMock = new Mock<IUser>().SetupAllProperties();

    // Invoke the code to test
    SetPropertiesOfUser(userMock.Object);

    // Validate properties have been set
    Assert.AreEqual(5, userMock.Object.Id);
    Assert.AreEqual("SomeName", userMock.Object.Name);

For completeness, the code being tested is below

    void SetPropertiesOfUser(IUser user)
    {
        user.Id = 5;
        user.Name = "SomeName";
    }



## Properties with private setters
Sometimes you want to create a mock of a class that has a private setter:

    public class MockTarget
    {
        public virtual string PropertyToMock { get; private set; }
    }

Or an interface that only defines a getter:

    public interface MockTarget
    {
        string PropertyToMock { get; }
    }

In both cases, you can ignore the setter and simply Setup the property getter to return a desired value:

    var mock = new Mock<MockTarget>();
    mock.SetupGet(x => x.PropertyToMock).Returns("ExpectedValue");

    Assert.AreEqual("ExpectedValue", mock.Object.PropertyToMock);


