---
title: "BindingList<T>"
slug: "bindinglistt"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Add item to list
    BindingList<string> listOfUIItems = new BindingList<string>();
    listOfUIItems.Add("Alice");
    listOfUIItems.Add("Bob");
    
    
    

## Avoiding N*2 iteration
This is placed in a Windows Forms event handler

    var nameList = new BindingList<string>();
    ComboBox1.DataSource = nameList;
    for(long i = 0; i < 10000; i++ ) {
        nameList.AddRange(new [] {"Alice", "Bob", "Carol" });
    } 

This takes a long time to execute, to fix, do the below:

    var nameList = new BindingList<string>();
    ComboBox1.DataSource = nameList;
    nameList.RaiseListChangedEvents = false;
    for(long i = 0; i < 10000; i++ ) {
        nameList.AddRange(new [] {"Alice", "Bob", "Carol" });
    } 
    nameList.RaiseListChangedEvents = true;
    nameList.ResetBindings();


