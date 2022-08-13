---
title: "EnumSet class"
slug: "enumset-class"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Java EnumSet class is the specialized Set implementation for use with enum types. It inherits AbstractSet class and implements the Set interface.

## Enum Set Example
    import java.util.*;  
    enum days {  
      SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY  
    }  
    public class EnumSetExample {  
      public static void main(String[] args) {  
        Set<days> set = EnumSet.of(days.TUESDAY, days.WEDNESDAY);  
        // Traversing elements  
        Iterator<days> iter = set.iterator();  
        while (iter.hasNext())  
          System.out.println(iter.next());  
      }  
    }  

