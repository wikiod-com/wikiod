---
title: "Multithreading Model"
slug: "multithreading-model"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

The new chrome v8 engine is partially ES7 compliant.
So if we add `"use strict";` to top of our file (typescript do that when transpiles typescript) we have to make sure that any functions that are on the global scope are actually assigned to the global scope. so we should use  `self.functionName` or `global.functionName`.

## use Workers in angular2 service
`/app/services/greeting.service.ts` :

    import { Injectable } from '@angular/core';
    import {greetingTypes,request,response} 
                from  './greeting.interface'

    @Injectable()
    export class Greeting{

        private worker;
        constructor(){
           this.worker = new Worker('../workers    /greeting.worker');
        }
    
        sayHello(message:string, answerCallback:Function){
            let requestData:request =   
                {'type':greetingTypes.HELLO ,'message':message} ;
    
            this.worker.postMessage(requestData); 
            this.worker.onmessage = (msg)=>{
                let response:response = msg.data; 
    
                if(response.type == greetingTypes.HELLO){
                    answerCallback(response.answer)
                }
            }
        }

        sayBye(message:string, answerCallback:Function){
            let requestData:request =     {'type':greetingTypes.BYE ,'message':message}; 

            this.worker.postMessage(requestData); 
            this.worker.onmessage = (msg)=>{
                let response:response = msg.data; 

                if(response.type == greetingTypes.BYE)
                    answerCallback(response.answer)
            }
        }
    } 


`app/services/greeting.interface.ts` :

    export enum greetingTypes{
        BYE,
        HELLO
    }

    export interface request{
       type:greetingTypes,
       message:string 
    }

    export interface response{
        type:greetingTypes,
        answer:string
    }

`app/workers/greeting.worker.ts` : 

    require("globals");
    import {greetingTypes,request,response} from 
                '../services/greeting.interface';

    self.onmessage = (msg)=> {
       let request:request = msg.data;
       let responseData:response;
       if(request.type ==  greetingTypes.HELLO)
            console.log('worker got the message: ' + 
                            request.message);
            responseData = {'type':greetingTypes.HELLO, 
                                'answer': 'HELLO!'};
            global.postMessage(responseData);

       if(request.type == greetingTypes.BYE )
            console.log('worker got the message: ' +request.message);
            responseData = {'type':greetingTypes.BYE ,  
                               'answer':'goodBye!'};
                global.postMessage(responseData);

        };

`app/app.component.ts` :

    import {Component} from "@angular/core";
    import {Greeting} from './services/greeting.service';
    @Component({
        selector: "my-app",
        templateUrl: "app.component.html",
        providers:[Greeting]
    })
    export class AppComponent {

        constructor(private greeting:Greeting){}

    
    public tapHello() {

        this.greeting.sayHello('hi',
            (answer)=>{console.log('answer from worker : '+ answer)});
    }


    public tapBye() {
        this.greeting.sayBye('bye',
            (answer) => {console.log('answer from worker : ' + answer)});
    }
}

`app/app.component.html` :

    <StackLayout>
        <Button text="sayBye" (tap)="tapBye()"></Button>
        <Button text="sayHello" (tap) = "tapHello()"></Button>
    </StackLayout>


