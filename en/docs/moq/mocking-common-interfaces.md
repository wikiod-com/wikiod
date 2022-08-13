---
title: "Mocking common interfaces"
slug: "mocking-common-interfaces"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Mocking IEnumerable
Mocking an interface that inherits from `IEnumerable` to return canned data is quite straightforward.  Assuming the following classes:

    public class DataClass
    {
        public int Id { get; set; }
    }

    public interface IEnumerableClass : IEnumerable<DataClass>
    {
    }

The following approach can be taken.  First, create a list containing the information that needs to be returned by the mock:

    IList<DataClass> list = new List<DataClass>();
    for (int i = 0; i < 10; i++)
    {
        list.Add(new DataClass { Id = 20 + i });
    }

Then create a mock of the `IEnumerable` class and setup its `GetEnumerator` method to return the list's enumerator instead:

    var mock = new Mock<IEnumerableClass>();
    mock.Setup(x => x.GetEnumerator()).Returns(list.GetEnumerator());


This can be validated as follows:

    int expected = 20;
    foreach (var i in mock.Object)
    {
        Assert.AreEqual(expected++, i.Id);
    }



