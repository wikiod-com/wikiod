---
title: "Example of Polymer toggleAttribute"
slug: "example-of-polymer-toggleattribute"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
 1. toggleAttribute(name, bool, node)

## Parameters
| Name | Details |
| ------ | ------ |
| name   | String: name of the HTML attribute which needs to be toggled   |
| bool   | boolean: Boolean to force the attribute on or off. When unspecified, the state of the attribute will be reversed.   |
| node   | HTMLElement:  name of the node which contains the HTML attribute. Defaults to this   |

A good example of this will be form, where submit button should only be active if all the mandatory fields have input.

## Basic example
    <script src='bower_components/webcomponentsjs/webcomponents-lite.min.js'></script>
    <link rel='import' href='bower_components/polymer/polymer.html'> 
    <link rel='import' href='bower_components/paper-button/paper-button.html'> 
    <link rel='import' href='bower_components/paper-input/paper-input.html'> 
    <dom-module id='toggle-attribute'>
      <template>
        <style>
        </style>
        <paper-input id='input'></paper-input>
        <paper-button on-tap='_toggle'>Tap me</paper-button>
      </template>
    </dom-module>
    <script>
      Polymer({
        is:'toggle-attribute',
        properties:{
          isTrue:{
            type:Boolean,
            value:false
          }
        },
        _toggle:function(){
          this.isTrue = !this.isTrue;
          this.toggleAttribute('disabled',this.isTrue,this.$.input);
        }
      })
    </script>

Here's a running [plunker](https://plnkr.co/edit/sWRhITPmV30b3oaIwjZ0)

