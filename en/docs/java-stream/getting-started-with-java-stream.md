---
title: "Getting started with java-stream"
slug: "getting-started-with-java-stream"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting java-stream set up or installed.

## Working with Java Stream Api (Java 8) in Android using Android Studio

**Gradle Setup :**

*build.gradle(Module: app)*

   

     compileOptions {
            sourceCompatibility JavaVersion.VERSION_1_8
            targetCompatibility JavaVersion.VERSION_1_8
        }
    
         jackOptions {
                enabled true
            }

**What is the Stream API ?**

Stream is a new abstract layer introduced in Java 8. A stream is a sequence of elements (objects, primitive types) from the stream source. Therefore, stream is not a data structure and it doesn’t store the elements it works with. It can be of a finite or infinite size and allow effortless code parallelism
------------------------------------------------------------------------

**Advantage's:**

 - It helps in using data in a declarative way. We can make use of
   Database functions like Max, Min etc., without running a full
   iteration.
 - It makes good use of multi-core architectures without worrying about
   multi-threading code.
 - We can create a pipeline of data operations with Java Stream that can
   run in a sequence or in parallel.
 - It provides support for group by, order by etc. operations.
 - It supports writing for code in Functional programming style.
 - It provides parallel processing of data.

**How streams work :** 

**Normal Approach (Without using Stream Api) :** 

     List<Integer> numbers = new ArrayList<>();
        numbers.addAll(Arrays.asList(1, 20, 3, 10, 20, 30, 4, 50, 80, 1, 2));//adding dummy data

        int i = 0;
        List<String> number_str = new ArrayList<>();
        for (Integer num : numbers) {
            if (i >= 5)//after 5 loop will stop
                break;

            if (num >= 10) // check number greater than or equal to 10
            {
                number_str.add(String.format("Number %d", num));//Typecast Integer to String then add to String List
                i++;//increment i count
            }
        }

        number_str.sort(Comparator.naturalOrder());//sort the list

In Above code we created Integer List and add some data's then Iterate using for loop. On each loop execution we check loop count(`i >= 5`) and `num >=10`. finally sort the string list.
------------------------------------------------------------------------

**Now, let’s rewrite the code using Java’s 8 Stream API:**

  

      List<Integer> numbers = new ArrayList<>();
    numbers.addAll(Arrays.asList(1, 20, 3, 10, 20, 30, 4, 50, 80, 1, 2));
    
    List<String> number_str = numbers.stream()
            .filter(num -> num >= 10)//check num greater than 10
            .limit(5)//stop loop at 5
            .sorted()//sort the list
            .map(num -> String.format("Number %d", num))//typecast into String List
            .collect(Collectors.toList());
    
        





