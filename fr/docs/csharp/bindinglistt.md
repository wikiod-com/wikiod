---
title: "BindingList<T>"
slug: "bindinglistt"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Ajouter un élément à la liste
    BindingList<string> listOfUIItems = new BindingList<string>();
    listOfUIItems.Add("Alice");
    listOfUIItems.Add("Bob");
    
    
    

## Eviter l'itération N*2
Ceci est placé dans un gestionnaire d'événements Windows Forms

    var nameList = new BindingList<string>();
    ComboBox1.DataSource = nameList;
    for(long i = 0; i < 10000; i++ ) {
        nameList.AddRange(new [] {"Alice", "Bob", "Carol" });
    } 

Cela prend beaucoup de temps à exécuter, pour corriger, procédez comme suit :

    var nameList = new BindingList<string>();
    ComboBox1.DataSource = nameList;
    nameList.RaiseListChangedEvents = false;
    for(long i = 0; i < 10000; i++ ) {
        nameList.AddRange(new [] {"Alice", "Bob", "Carol" });
    } 
    nameList.RaiseListChangedEvents = true;
    nameList.ResetBindings();


