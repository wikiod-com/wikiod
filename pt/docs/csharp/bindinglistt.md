---
title: "BindingList<T>"
slug: "bindinglistt"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Adicionar item à lista
    BindingList<string> listOfUIItems = new BindingList<string>();
    listOfUIItems.Add("Alice");
    listOfUIItems.Add("Bob");
    
    
    

## Evitando a iteração N*2
Isso é colocado em um manipulador de eventos do Windows Forms

    var nameList = new BindingList<string>();
    ComboBox1.DataSource = nameList;
    for(long i = 0; i < 10000; i++ ) {
        nameList.AddRange(new [] {"Alice", "Bob", "Carol" });
    } 

Isso leva muito tempo para executar, para corrigir, faça o seguinte:

    var nameList = new BindingList<string>();
    ComboBox1.DataSource = nameList;
    nameList.RaiseListChangedEvents = false;
    for(long i = 0; i < 10000; i++ ) {
        nameList.AddRange(new [] {"Alice", "Bob", "Carol" });
    } 
    nameList.RaiseListChangedEvents = true;
    nameList.ResetBindings();


