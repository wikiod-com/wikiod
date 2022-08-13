---
title: "BindingList<T>"
slug: "bindinglistt"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Agregar elemento a la lista
    BindingList<string> listOfUIItems = new BindingList<string>();
    listOfUIItems.Add("Alice");
    listOfUIItems.Add("Bob");
    
    
    

## Evitar la iteración N*2
Esto se coloca en un controlador de eventos de Windows Forms

    var nameList = new BindingList<string>();
    ComboBox1.DataSource = nameList;
    for(long i = 0; i < 10000; i++ ) {
        nameList.AddRange(new [] {"Alice", "Bob", "Carol" });
    } 

Esto toma mucho tiempo para ejecutarse, para solucionarlo, haga lo siguiente:

    var nameList = new BindingList<string>();
    ComboBox1.DataSource = nameList;
    nameList.RaiseListChangedEvents = false;
    for(long i = 0; i < 10000; i++ ) {
        nameList.AddRange(new [] {"Alice", "Bob", "Carol" });
    } 
    nameList.RaiseListChangedEvents = true;
    nameList.ResetBindings();


