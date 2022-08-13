---
title: "Java SE 7 Features"
slug: "java-se-7-features"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

In this topic you'll find a summary of the new features added to the Java programming language in Java SE 7. There are many other new features in other fields such as JDBC and Java Virtual Machine (JVM) that are not going to be covered in this topic.

[Enhancements in Java SE 7][1]


  [1]: http://docs.oracle.com/javase/8/docs/technotes/guides/language/enhancements.html#javase7 "Official Java SE7 Enhancements List"

## New Java SE 7 programming language features
 - [Binary Literals][1]: The integral types (byte, short, int, and long) can also be expressed using the binary number system. To specify a binary literal, add the prefix 0b or 0B to the number.
 - [Strings in switch Statements][2]: You can use a String object in the expression of a switch statement
 - [The try-with-resources Statement][3]: The try-with-resources statement is a try statement that declares one or more resources. A resource is as an object that must be closed after the program is finished with it. The try-with-resources statement ensures that each resource is closed at the end of the statement. Any object that implements java.lang.AutoCloseable, which includes all objects which implement java.io.Closeable, can be used as a resource.
 - [Catching Multiple Exception Types and Rethrowing Exceptions with Improved Type Checking][4]: a single catch block can handle more than one type of exception. This feature can reduce code duplication and lessen the temptation to catch an overly broad exception.
 - [Underscores in Numeric Literals][5]: Any number of underscore characters (_) can appear anywhere between digits in a numerical literal. This feature enables you, for example, to separate groups of digits in numeric literals, which can improve the readability of your code.
 - [Type Inference for Generic Instance Creation][6]: You can replace the type arguments required to invoke the constructor of a generic class with an empty set of type parameters (<>) as long as the compiler can infer the type arguments from the context. This pair of angle brackets is informally called the diamond.
 - [Improved Compiler Warnings and Errors When Using Non-Reifiable Formal Parameters with Varargs Methods][7]


  [1]: http://docs.oracle.com/javase/8/docs/technotes/guides/language/binary-literals.html
  [2]: http://docs.oracle.com/javase/8/docs/technotes/guides/language/strings-switch.html
  [3]: http://docs.oracle.com/javase/8/docs/technotes/guides/language/try-with-resources.html
  [4]: http://docs.oracle.com/javase/8/docs/technotes/guides/language/catch-multiple.html
  [5]: http://docs.oracle.com/javase/8/docs/technotes/guides/language/underscores-literals.html
  [6]: http://docs.oracle.com/javase/8/docs/technotes/guides/language/type-inference-generic-instance-creation.html
  [7]: http://docs.oracle.com/javase/8/docs/technotes/guides/language/non-reifiable-varargs.html

## Binary Literals
    // An 8-bit 'byte' value:
    byte aByte = (byte)0b00100001;
    
    // A 16-bit 'short' value:
    short aShort = (short)0b1010000101000101;
    
    // Some 32-bit 'int' values:
    int anInt1 = 0b10100001010001011010000101000101;
    int anInt2 = 0b101;
    int anInt3 = 0B101; // The B can be upper or lower case.
    
    // A 64-bit 'long' value. Note the "L" suffix:
    long aLong = 0b1010000101000101101000010100010110100001010001011010000101000101L;

## The try-with-resources statement
The example reads the first line from a file. It uses an instance of `BufferedReader` to read data from the file. `BufferedReader` is a resource that must be closed after the program is finished with it:

    static String readFirstLineFromFile(String path) throws IOException {
      try (BufferedReader br = new BufferedReader(new FileReader(path))) {
        return br.readLine();
      }
    }

In this example, the resource declared in the try-with-resources statement is a `BufferedReader`. The declaration statement appears within parentheses immediately after the try keyword. The class `BufferedReader`, in Java SE 7 and later, implements the interface `java.lang.AutoCloseable`. Because the `BufferedReader` instance is declared in a try-with-resource statement, it will be closed regardless of whether the try statement completes normally or abruptly (as a result of the method `BufferedReader.readLine` throwing an `IOException`).

## Underscores in Numeric Literals
The following example shows other ways you can use the underscore in numeric literals:

    long creditCardNumber = 1234_5678_9012_3456L;
    long socialSecurityNumber = 999_99_9999L;
    float pi = 3.14_15F;
    long hexBytes = 0xFF_EC_DE_5E;
    long hexWords = 0xCAFE_BABE;
    long maxLong = 0x7fff_ffff_ffff_ffffL;
    byte nybbles = 0b0010_0101;
    long bytes = 0b11010010_01101001_10010100_10010010;

You can place underscores only between digits; you cannot place underscores in the following places:
- At the beginning or end of a number
- Adjacent to a decimal point in a floating point literal
- Prior to an F or L suffix
- In positions where a string of digits is expected


## Type Inference for Generic Instance Creation
You can use

    Map<String, List<String>> myMap = new HashMap<>();
instead of 

    Map<String, List<String>> myMap = new HashMap<String, List<String>>();
However, you can't use

    List<String> list = new ArrayList<>();
    list.add("A");
    
      // The following statement should fail since addAll expects
      // Collection<? extends String>
    
    list.addAll(new ArrayList<>());
because it can't compile. Note that the diamond often works in method calls; however, it is suggested that you use the diamond primarily for variable declarations.

## Strings in switch Statements
    public String getTypeOfDayWithSwitchStatement(String dayOfWeekArg) {
         String typeOfDay;
         switch (dayOfWeekArg) {
             case "Monday":
                 typeOfDay = "Start of work week";
                 break;
             case "Tuesday":
             case "Wednesday":
             case "Thursday":
                 typeOfDay = "Midweek";
                 break;
             case "Friday":
                 typeOfDay = "End of work week";
                 break;
             case "Saturday":
             case "Sunday":
                 typeOfDay = "Weekend";
                 break;
             default:
                 throw new IllegalArgumentException("Invalid day of the week: " + dayOfWeekArg);
         }
         return typeOfDay;
    }

