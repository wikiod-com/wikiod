---
title: "Declare class method and instance method"
slug: "declare-class-method-and-instance-method"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

Instance method are methods that are specific to particular classes. Instance methods are declared and defined followed by - (minus) symbol.

Class methods can be called by class name itself .Class methods are declared and defined by using + (plus)sign .

## Syntax
 1.  -(void)testInstanceMethod; //Class methods declare with "+" sign 
 2. (void)classMethod;//instance methods declare with "-" sign 
  
   

 



  



   



    

    



## How to declare class method and instance method.
 **instance methods use an instance of a class.** 

    @interface MyTestClass : NSObject
    
    - (void)testInstanceMethod;
        
    @end

**They could then be used like so:**

    MyTestClass *object = [[MyTestClass alloc] init];
    [object testInstanceMethod];


 Class method can be used with just the class name.

    @interface MyClass : NSObject
    
    + (void)aClassMethod;
    
    @end
**They could then be used like so:** 

    [MyClass aClassMethod];


**class methods are the convenience methods on many Foundation classes like [NSString's +stringWithFormat:] or NSArray's +arrayWithArray**





