---
title: "Classes"
slug: "classes"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Declaring a class
    // class declaration with a list of parameters for a primary constructor
    type Car (model: string, plates: string, miles: int) =    
    
        // secondary constructor (it must call primary constructor)
        new (model, plates) = 
            let miles = 0
            new Car(model, plates, miles)
        
        // fields
        member this.model = model
        member this.plates = plates
        member this.miles = miles
        

# Creating an instance

    let myCar = Car ("Honda Civic", "blue", 23)
    // or more explicitly
    let (myCar : Car) = new Car ("Honda Civic", "blue", 23)

