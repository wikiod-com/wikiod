---
title: "Controllers with ES6"
slug: "controllers-with-es6"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Controller
it is very easy to write an angularJS controller with ES6 if your are familiarized with the 
**Object Oriented Programming** : 

   
     class exampleContoller{
          
            constructor(service1,service2,...serviceN){
               let ctrl=this;
                   ctrl.service1=service1;
                   ctrl.service2=service2;
                    .
                    .
                    .
                   ctrl.service1=service1;
                   ctrl.controllerName = 'Example Controller';
                   ctrl.method1(controllerName)
    
            }
    
            method1(param){
               let ctrl=this;
                    ctrl.service1.serviceFunction();
                   .
                   .
                   ctrl.scopeName=param;
            }
            .
            .
            .
            methodN(param){
               let ctrl=this;
                    ctrl.service1.serviceFunction();
                   .
                   .
            }
    
        
        } 
        exampleContoller.$inject = ['service1','service2',...,'serviceN'];
        export default exampleContoller;

