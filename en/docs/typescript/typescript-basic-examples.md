---
title: "Typescript basic examples"
slug: "typescript-basic-examples"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

This is a basic example which extends a generic car class and defines a car description method. 

Find more TypeScript examples here - [TypeScript Examples GitRepo][1]


  [1]: https://github.com/rvashishth/TypeScriptExamples

## 1 basic class inheritance example using extends and super keyword
A generic Car class has some car property and a description method

    class Car{
        name:string;
        engineCapacity:string;
    
        constructor(name:string,engineCapacity:string){
            this.name = name;
            this.engineCapacity = engineCapacity;
        }
    
        describeCar(){
            console.log(`${this.name} car comes with ${this.engineCapacity} displacement`);
        }
    }
    
    new Car("maruti ciaz","1500cc").describeCar();
        
HondaCar extends the existing generic car class and adds new property.

    class HondaCar extends Car{
        seatingCapacity:number;
    
        constructor(name:string,engineCapacity:string,seatingCapacity:number){
            super(name,engineCapacity);
            this.seatingCapacity=seatingCapacity;
        }
    
        describeHondaCar(){
            super.describeCar();
            console.log(`this cars comes with seating capacity of ${this.seatingCapacity}`);
        }
    }
    new HondaCar("honda jazz","1200cc",4).describeHondaCar();






## 2 static class variable example - count how many time method is being invoked
here countInstance is a static class variable

    class StaticTest{
        static countInstance : number= 0;
        constructor(){
            StaticTest.countInstance++;
        }
    }
    
    new StaticTest();
    new StaticTest();
    console.log(StaticTest.countInstance);

