---
title: "Annotation Processor"
slug: "annotation-processor"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Annotation processor is a tool build in javac for scanning and processing annotations at compile time.

Annotations are a class of metadata that can be associated with classes, methods, fields, and even other annotations.There are two ways to access these annotations at runtime via reflection and at compile time via annotation processors.



 

## @NonNull Annotation
    public class Foo {
        private String name;
        public Foo(@NonNull String name){...};
        ...
    }

Here @NonNull is annotation which is processed compile time by the android studio to warn you that the particular function needs non null parameter. 

## Types of Annotations
There are three types of annotations.

 1. **Marker Annotation** - annotation that has no method

        @interface CustomAnnotation {}

   
 2. **Single-Value Annotation** - annotation that has one method

        @interface CustomAnnotation {  
            int value();  
        }

  
 3. **Multi-Value Annotation** - annotation that has more than one method

        @interface CustomAnnotation{  
            int value1();  
            String value2();  
            String value3();  
        }

  
  

 


## Creating and Using  Custom Annotations
For creating custom annotations we need to decide
    
 - Target - on which these annotations will work on like field level,
   method level, type level etc.
 - Retention - to what level annotation will be available.

For this, we have built in custom annotations. Check out these mostly used ones:

**@Target**

[![Target and what it means][1]][1]

**@Retention**

[![Retention and what it means][2]][2]

**Creating Custom Annotation**
    
    @Retention(RetentionPolicy.SOURCE) // will not be available in compiled class   
    @Target(ElementType.METHOD) // can be applied to methods only
    @interface CustomAnnotation{  
          int value();    
    }

**Using Custom Annotation**

    class Foo{  
      @CustomAnnotation(value = 1)  // will be used by an annotation processor
      public void foo(){..}  
    }
the value provided inside `@CustomAnnotation` will be consumed by an Annotationprocessor may be to generate code at compile time etc.


  

 

 


  [1]: https://i.stack.imgur.com/JTB0w.png
  [2]: https://i.stack.imgur.com/P4Ota.png

