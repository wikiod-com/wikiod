---
title: "Java 8 – Convert Map to List"
slug: "java-8-–-convert-map-to-list"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Convert Map to List in java 8 using stream api

## Java 8 – Convert Map to List
    public class ConvertMapToList {
    
        public static void main(String[] args) {
    
            Map<Integer, String> map = new HashMap<>();
            map.put(10, "apple");
            map.put(20, "orange");
            map.put(30, "banana");
            map.put(40, "watermelon");
            map.put(50, "dragonfruit");
    
            System.out.println("\n1. Export Map Key as List of Integrer method 1");
    
            List<Integer> methodOneIntegers= map.entrySet().stream()
                    .map(x -> x.getKey())
                    .collect(Collectors.toList());
                 
           methodOneIntegers.forEach(System.out::println);

           System.out.println("\n1. Export Map Key as List of Integrer method 2");
    
            List<Integer> methodTwoIntegers= map.keySet().stream()                    
                    .collect(Collectors.toList());
    
             methodOneIntegers.forEach(System.out::println);
    
            System.out.println("\n2. Export Map Value as List of String method 1");
    
            List<String> methodOneStrings= map.entrySet().stream()
                    .map(x -> x.getValue())
                    .collect(Collectors.toList());
    
            methodOneStrings.forEach(System.out::println);


        System.out.println("\n2. Export Map Value as List of String method 2");
    
            List<String> methodTwoStrings= map.values().stream()
                    .map(x -> x.getValue())
                    .collect(Collectors.toList());
    
            methodTwoStrings.forEach(System.out::println);
    
    
        }
    
    }



Output

    Export Map Key as List of Integrer method 1
    50
    20
    40
    10
    30
    
    Export Map Key as List of Integrer method 2
    50
    20
    40
    10
    30
    
    Export Map Value as List of String method 1
    dragonfruit
    orange
    watermelon
    apple
    banana
    
    Export Map Value as List of String method 2
    dragonfruit
    orange
    watermelon
    apple
    banana




