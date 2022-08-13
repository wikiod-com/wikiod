---
title: "Mocking consecutive calls to a void return method"
slug: "mocking-consecutive-calls-to-a-void-return-method"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

The [Mockito docs](https://static.javadoc.io/org.mockito/mockito-core/2.7.2/org/mockito/Mockito.html#stubbing_consecutive_calls) have an excellent example of how to provide a sequence of answers for multiple calls to a mock. However, they don't cover how to do that for a method that returns void, other than noting that stubbing void methods require using the [do family of methods](https://static.javadoc.io/org.mockito/mockito-core/2.7.2/org/mockito/Mockito.html#do_family_methods_stubs).

Remember, for non-void methods, the `when(mock.method()).thenThrow().thenReturn()` version (see [docs](https://static.javadoc.io/org.mockito/mockito-core/2.7.2/org/mockito/Mockito.html#stubbing_consecutive_calls)) is preferred because it is argument type-safe and more readable.

## Faking a transient error
Imagine you're testing code that makes a call to this interface, and you want to make sure your retry code is working.
```java
public interface DataStore {
    void save(Data data) throws IOException;
}
```

You could do something like this:
```java
public void saveChanges_Retries_WhenDataStoreCallFails() {
    DataStore dataStore = new DataStore();
    Data data = new Data();
    doThrow(IOException.class).doNothing().when(dataStore).save(data);

    dataStore.save(data);

    verify(dataStore, times(2)).save(data);
    verifyDataWasSaved();
}
```

