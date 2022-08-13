---
title: "BindingList<T>"
slug: "bindinglistt"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Listeye öğe ekle
    BindingList<string> listOfUIItems = new BindingList<string>();
    listOfUIItems.Add("Alice");
    listOfUIItems.Add("Bob");
    
    
    

## N*2 yinelemesinden kaçınma
Bu, bir Windows Forms olay işleyicisine yerleştirilir

    var nameList = new BindingList<string>();
    ComboBox1.DataSource = nameList;
    for(long i = 0; i < 10000; i++ ) {
        nameList.AddRange(new [] {"Alice", "Bob", "Carol" });
    } 

Bunun yürütülmesi, düzeltilmesi uzun zaman alır, aşağıdakileri yapın:

    var nameList = new BindingList<string>();
    ComboBox1.DataSource = nameList;
    nameList.RaiseListChangedEvents = false;
    for(long i = 0; i < 10000; i++ ) {
        nameList.AddRange(new [] {"Alice", "Bob", "Carol" });
    } 
    nameList.RaiseListChangedEvents = true;
    nameList.ResetBindings();


