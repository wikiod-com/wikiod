---
title: "JavaFX bindings"
slug: "javafx-bindings"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Simple property binding
   JavaFX has a binding API, which provides ways of binding one property to the other. This means that whenever one property's value is changed, the value of the bound property is updated automatically. An example of simple binding:


    SimpleIntegerProperty first =new SimpleIntegerProperty(5); //create a property with value=5
    SimpleIntegerProperty second=new SimpleIntegerProperty();
    
    public void test()
    {
        System.out.println(second.get()); // '0'
        second.bind(first);               //bind second property to first
        System.out.println(second.get()); // '5'
        first.set(16);                    //set first property's value
        System.out.println(second.get()); // '16' - the value was automatically updated
    }

You can also bind a primitive property with applying an addition, subtraction, division, etc:

    public void test2()
    {
            second.bind(first.add(100));
            System.out.println(second.get()); //'105'
            second.bind(first.subtract(50));
            System.out.println(second.get()); //'-45'
    }

Any Object can be put into SimpleObjectProperty:

    SimpleObjectProperty<Color> color=new SimpleObjectProperty<>(Color.web("45f3d1"));

It is possible to create bidirectional bindings. In this case, properties depend on each other.

    public void test3()
    {
            second.bindBidirectional(first);
            System.out.println(second.get()+" "+first.get());
            second.set(1000);
            System.out.println(second.get()+" "+first.get()); //both are '1000'
    }



