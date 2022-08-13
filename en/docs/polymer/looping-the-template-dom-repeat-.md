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

