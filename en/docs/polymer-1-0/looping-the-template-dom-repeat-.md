---
title: "Looping, the template dom-repeat."
slug: "looping-the-template-dom-repeat"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## A basic list
This is a basic polymer element that show a list of names.

    <link rel="import" href="../bower_components/polymer/polymer.html">
    
    <dom-module id="basic-list">
      <template>
        <style>
        </style>
    
        <div>Name's list</div>
        <template is="dom-repeat" items="{{list}}">
            <div>{{item.lastName}}, {{item.firstName}}</div>
        </template >
    
      </template>
    
      <script>
        Polymer({
          is: 'basic-list',
    
          properties:{
            list:{
              type: Array,
              value: function(){
                let list = [
                  {firstName: "Alice", lastName: "Boarque"},
                  {firstNName: "Carlos, lastName: "Dutra"}
                ]        
    
                return list
              },
            },
          },
        });
      </script>
    </dom-module>


## Specifying an element in a list
By default, accessing an individual item in a template dom-repeat loop is by calling `{{item}}`. Passing in an `as=` attribute to template will allow you to switch out the default `{{item}}` syntax for something that is more customized to the module you are working with. In this case, we want to grab the first and last name of a person, so we pass in `as="person"` to template. We can now access the names using `{{person}}`.
    
    <dom-module id="basic-list">
      <template>
        <template is="dom-repeat" items={{list}} as="person">
          <div>{{person.lastName}}, {{person.firstName}}</div>
        </template>
      </template>
    </dom-module>

