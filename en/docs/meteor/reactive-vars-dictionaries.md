---
title: "Reactive (Vars & Dictionaries)"
slug: "reactive-vars--dictionaries"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Reactive Query
Example code :

In main.html

    <template name="test">
         <input type="checkbox" id="checkbox1" name="name" value="data">Check Me
          {{showData}}
    </template>

In Main.js

     var check_status='';
     //Reactive Var Initialization
     Template.main.onCreated(function (){
           check_status=new ReactiveVar({});
           
     });

     Template.main.helpers({
           showData : function(){
               return Collection.find(check_status.get());
           }
     });

     Template.main.events({
          "change #checkbox1" : function(){
                  check_status.set({field: 'data'});
           }
     });


Explanation:     

When we initialize the reactive var `check_status` we set the value equal to  `{}`. In the helper, at the time of rendering, the same data is passed to the query `Collection.find(check_status.get())` which is as good as *show all* data.

As soon as you click on the checkbox, the event described in the `Template.main.events` is triggered which sets the value of `check_status` to `{field: data}`. Since, this is a *reactive var*, the `showData` template is re run and this time the query is `Collection.find({field: data})`, so only fields, where `field` matched `'data'` is returned.

You will need to add the `reactive var` package(`meteor add reactive-var`) before using this commands.    

