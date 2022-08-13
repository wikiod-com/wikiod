---
title: "Test Doubles"
slug: "test-doubles"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

When testing, it is sometimes useful to use a test double to manipulate or verify the behaviour of the system under test.  The doubles are passed or [injected][1] into the class or method under test instead of instances of production code.


  [1]: https://www.wikiod.com/unit-testing/dependency-injection

## Using a stub to supply canned responses
A stub is a light weight test double that provides canned responses when methods are called.  Where a class under test relies on an interface or base class an alternative 'stub' class can be implemented for testing which conforms to the interface.

So, assuming the following interface,

    public interface IRecordProvider {
        IEnumerable<Record> GetRecords();
    }


If the following method was to be tested

    public bool ProcessRecord(IRecordProvider provider)

A stub class that implements the interface can be written to return known data to the method being tested.

    public class RecordProviderStub : IRecordProvider
    {
        public IEnumerable<Record> GetRecords()
        {
            return new List<Record> {
                new Record { Id = 1, Flag=false, Value="First" },
                new Record { Id = 2, Flag=true, Value="Second" },
                new Record { Id = 3, Flag=false, Value="Third" }
            };
        }
    }

This stub implementation can then be provided to the system under test, to influence it's behaviour.

    var stub = new RecordProviderStub();
    var processed = sut.ProcessRecord(stub);


## Using a mocking framework as a stub
The terms Mock and Stub can often become confused.  Part of the reason for this is that many mocking frameworks also provide support for creating Stubs without the verification step associated with Mocking.

Rather than writing a new class to implement a stub as in the "Using a stub to supply canned responses" example, mocking frameworks can be used instead.

Using Moq:

    var stub = new Mock<IRecordProvider>();
    stub.Setup(provider => provider.GetRecords()).Returns(new List<Record> {
        new Record { Id = 1, Flag=false, Value="First" },
        new Record { Id = 2, Flag=true, Value="Second" },
        new Record { Id = 3, Flag=false, Value="Third" }
    });

This achieves the same behaviour as the hand coded stub, and can be supplied to the system under test in a similar way:

    var processed = sut.ProcessRecord(stub.Object);


## Using a mocking framework to validate behaviour
Mocks are used when it is necessary to verify the interactions between the system under test and test doubles.  Care needs to be taken to avoid creating overly brittle tests, but mocking can be particularly useful when the method to test is simply orchestrating other calls.

This test verifies that when the method under test is called (`ProcessRecord`), that the service method (`UseValue`)  is called for the `Record` where `Flag==true`.  To do this, it sets up a stub with canned data:

    var stub = new Mock<IRecordProvider>();
    stub.Setup(provider => provider.GetRecords()).Returns(new List<Record> {
        new Record { Id = 1, Flag=false, Value="First" },
        new Record { Id = 2, Flag=true, Value="Second" },
        new Record { Id = 3, Flag=false, Value="Third" }
    });

Then it sets up a mock which implements the `IService` interface:

    var mockService = new Mock<IService>();
    mockService.Setup(service => service.UseValue(It.IsAny<string>())).Returns(true);

These are then supplied to the system under test and the method to be tested is called.

    var sut = new SystemUnderTest(mockService.Object);

    var processed = sut.ProcessRecord(stub.Object);

The mock can then be interrogated to verify that the expected call has been made to it.  In this case, a call to `UseValue`, with one parameter "Second", which is the value from the record where `Flag==true`.

    mockService.Verify(service => service.UseValue("Second"));



