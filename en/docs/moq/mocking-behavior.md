---
title: "Mocking Behavior"
slug: "mocking-behavior"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
 - mock.Setup(expression).Returns(value) //Whenever called the method in the expression will return value

## Parameters
| Parameter | Details|
| --------- | ------ |
| expression|  Lambda expression that specifies the method invocation.  |

## No-Argument method mocking
    interface Mockable {
        bool DoSomething();
    }

    var mock = new Mock<Mockable>();
    mock.Setup(x => x.DoSomething()).Returns(true);

    var result = mock.Object.DoSomething(); //true

## Mocking protected members
To mock a protected member you must first include the following at the top of your test fixture:

    using Moq.Protected;

You then call `Protected()` on your mock, after which you can use the generic `Setup<>` with the return type of your method.

    var mock = new Mock<MyClass>();
    mock.Protected()
         .Setup<int>("MyProtectedGetIntMethod")
         .Returns(1);

If the method returns void then use the non-generic `Setup()`.

## Mocking void methods to confirm what they return
    var logRepository = new Mock<ILogRepository>();
    logRepository.Setup(x => x.Write(It.IsAny<Exception>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()))
      .Verifiable();

In this case, we are using the Verifiable to ensure that it runs.

We could also use a callback here:


    logRepository.Setup(x => x.Write(It.IsAny<Exception>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()))
         .Callback<Exception, string, string, string, string, string>((ex, caller, user, machine, source, message) => { Console.WriteLine(message); });

this would log the output from the method to standard output on the console (many testing frameworks let you capture that output into their runner)

