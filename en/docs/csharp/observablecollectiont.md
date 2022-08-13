---
title: "ObservableCollection<T>"
slug: "observablecollectiont"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Initialize ObservableCollection<T>
`ObservableCollection` is a collection of type `T` like `List<T>` which means that it holds objects of type `T`. 

From documentation we read that :

> `ObservableCollection`represents a dynamic data collection that
> provides notifications when items get added, removed, or when the
> entire list is refreshed.

The key difference from other collections is that `ObservableCollection` implements the interfaces `INotifyCollectionChanged` and `INotifyPropertyChanged` and immediately raise notification event when a new object is added or removed and when collection is cleared.

This is especially useful for conneting the UI and backend of an application without having to write extra code because when an object is added to or removed from an observable collection, the UI is automatically updated. 

The first step in order to use it is to include 

    using System.Collections.ObjectModel

You can either create an empty instance of a collection for example of type `string`

    ObservableCollection<string> collection = new ObservableCollection<string>();

or an instance that is filled with data

     ObservableCollection<string> collection = new ObservableCollection<string>()
     {
      "First_String", "Second_String"
     };

Remember as in all IList collection, index starts from 0 ([IList<T>.Item Property][1]).


  [1]: https://msdn.microsoft.com/en-us/library/ewthkb10(v=vs.110).aspx "IList&lt;T&gt;.Item Property"

