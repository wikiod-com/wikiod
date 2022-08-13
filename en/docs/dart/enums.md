---
title: "Enums"
slug: "enums"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Basic usage
    enum Fruit {
      apple, banana
    }
    
    main() {
      var a = Fruit.apple;
      switch (a) {
        case Fruit.apple:
          print('it is an apple');
          break;
      }
    
      // get all the values of the enums
      for (List<Fruit> value in Fruit.values) {
        print(value);
      }
    
      // get the second value
      print(Fruit.values[1]);
    }

