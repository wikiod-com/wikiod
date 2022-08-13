---
title: "Converting to and from Strings"
slug: "converting-to-and-from-strings"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

## Converting String to other datatypes.
You can convert a **numeric** string to various Java numeric types as follows:

**String to int:**

    String number = "12";
    int num = Integer.parseInt(number);

**String to float:**

    String number = "12.0";
    float num = Float.parseFloat(number);

**String to double:**

    String double = "1.47";
    double num = Double.parseDouble(double);
  
**String to boolean:**  
    
    String falseString = "False";
    boolean falseBool = Boolean.parseBoolean(falseString);   // falseBool = false 
        
    String trueString = "True";
    boolean trueBool = Boolean.parseBoolean(trueString);     // trueBool = true

**String to long:**

    String number = "47";
    long num = Long.parseLong(number);

**String to BigInteger:**


    String bigNumber = "21";
    BigInteger reallyBig = new BigInteger(bigNumber);

**String to BigDecimal:**

    String bigFraction = "17.21455";
    BigDecimal reallyBig = new BigDecimal(bigFraction);




**Conversion Exceptions:**

The numeric conversions above will all throw an (unchecked) `NumberFormatException` if you attempt to parse a string that is not a suitably formatted number, or is out of range for the target type.  The [Exceptions][1] topic discusses how to deal with such exceptions.

If you wanted to test that you can parse a string, you could implement a `tryParse...` method like this:

    boolean tryParseInt (String value) {  
        try {  
            String somechar = Integer.parseInt(value);
            return true;  
         } catch (NumberFormatException e) { 
            return false;  
         }  
    }

However, calling this `tryParse...` method immediately before parsing is (arguably) poor practice.  It would be better to just call the `parse...` method and deal with the exception.

  [1]: https://www.wikiod.com/java/exceptions-and-exception-handling

## Conversion to / from bytes

To encode a string into a byte array, you can simply use the `String#getBytes()` method, with one of the standard character sets available on any Java runtime:

    byte[] bytes = "test".getBytes(StandardCharsets.UTF_8);

and to decode:

    String testString = new String(bytes, StandardCharsets.UTF_8);


you can further simplify the call by using a static import:

    import static java.nio.charset.StandardCharsets.UTF_8;
    ...
    byte[] bytes = "test".getBytes(UTF_8);

---

For less common character sets you can indicate the character set with a string:

    byte[] bytes = "test".getBytes("UTF-8");

and the reverse:

    String testString = new String (bytes, "UTF-8");

this does however mean that you have to handle the checked `UnsupportedCharsetException`.

---

The following call will use the default character set.
The default character set is platform specific and generally differs between Windows, Mac and Linux platforms.

    byte[] bytes = "test".getBytes();

and the reverse:

    String testString = new String(bytes);
    
---

Note that invalid characters and bytes may be replaced or skipped by these methods.
For more control - for instance for validating input - you're encouraged to use the `CharsetEncoder` and `CharsetDecoder` classes.

## Base64 Encoding / Decoding
Occasionally you will find the need to encode binary data as a [*base64*][1]-encoded string. 

For this we can use the [`DatatypeConverter`][2] class from the [`javax.xml.bind`][3] package:

    import javax.xml.bind.DatatypeConverter;
    import java.util.Arrays;

    // arbitrary binary data specified as a byte array
    byte[] binaryData = "some arbitrary data".getBytes("UTF-8");

    // convert the binary data to the base64-encoded string
    String encodedData = DatatypeConverter.printBase64Binary(binaryData);
    // encodedData is now "c29tZSBhcmJpdHJhcnkgZGF0YQ=="

    // convert the base64-encoded string back to a byte array
    byte[] decodedData = DatatypeConverter.parseBase64Binary(encodedData);

    // assert that the original data and the decoded data are equal
    assert Arrays.equals(binaryData, decodedData);

-----------------------------------------------------------------------------------

**Apache commons-codec**

Alternatively, we can use `Base64` from [Apache commons-codec][4].

    import org.apache.commons.codec.binary.Base64;

    // your blob of binary as a byte array
    byte[] blob = "someBinaryData".getBytes();

    // use the Base64 class to encode
    String binaryAsAString = Base64.encodeBase64String(blob);

    // use the Base64 class to decode
    byte[] blob2 = Base64.decodeBase64(binaryAsAString);

    // assert that the two blobs are equal
    System.out.println("Equal : " + Boolean.toString(Arrays.equals(blob, blob2)));


If you inspect this program wile running, you will see that `someBinaryData` encodes to `c29tZUJpbmFyeURhdGE=`, a very managable _UTF-8_ String object.

-----------------------------------------------------------------------------------

<!-- if version [gte Java SE 8] -->

Details for the same can be found at [Base64][5]


    // encode with padding
    String encoded = Base64.getEncoder().encodeToString(someByteArray);
    
    // encode without padding
    String encoded = Base64.getEncoder().withoutPadding().encodeToString(someByteArray);
    
    // decode a String
    byte [] barr = Base64.getDecoder().decode(encoded); 

[Reference][6]
<!-- end version if -->


  [1]: https://it.wikipedia.org/wiki/Base64
  [2]: https://docs.oracle.com/javase/7/docs/api/javax/xml/bind/DatatypeConverter.html
  [3]: https://docs.oracle.com/javase/7/docs/api/javax/xml/bind/package-summary.html
  [4]: http://commons.apache.org/proper/commons-codec/
  [5]: http://docs.oracle.com/javase/8/docs/api/java/util/Base64.html
  [6]: http://stackoverflow.com/questions/19743851/base64-java-encode-and-decode-a-string

## Converting other datatypes to String

* You can get the value of other primitive data types as a String using one the String class's `valueOf` methods.

    For example:

      int i = 42;
      String string = String.valueOf(i);
      //string now equals "42”.

    This method is also overloaded for other datatypes, such as `float`, `double`, `boolean`, and even `Object`.

* You can also get any other Object (any instance of any class) as a String by calling `.toString` on it. For this to give useful output, the class must override `toString()`. Most of the standard Java library classes do, such as `Date` and others.

    For example:

      Foo foo = new Foo(); //Any class.
      String stringifiedFoo = foo.toString().
        
    Here `stringifiedFoo` contains a representation of `foo` as a String.

You can also convert any number type to String with short notation like below.

    int i = 10;
    String str = i + "";
 
 Or just simple way is

    String str = 10 + "";




## Getting a `String` from an `InputStream`
A `String` can be read from an `InputStream` using the byte array constructor.

    import java.io.*;
    
    public String readString(InputStream input) throws IOException {
        byte[] bytes = new byte[50]; // supply the length of the string in bytes here
        input.read(bytes);
        return new String(bytes);
    }

This uses the system default charset, although an alternate charset may be specified:

    return new String(bytes, Charset.forName("UTF-8"));

## Parsing Strings to a Numerical Value
**String to a primitive numeric type or a numeric wrapper type:**

Each numeric wrapper class provides a `parseXxx` method that converts a `String` to the corresponding primitive type.   The following code converts a `String` to an `int` using the `Integer.parseInt` method:

    String string = "59";
    int primitive = Integer.parseInteger(string);
    
To convert to a `String` to an instance of a numeric wrapper class you can either use an overload of the wrapper classes `valueOf` method:

    String string = "59";
    Integer wrapper = Integer.valueOf(string);
    
or rely on auto boxing (Java 5 and later):

    String string = "59";
    Integer wrapper = Integer.parseInteger(string);  // 'int' result is autoboxed

The above pattern works for `byte`, `short`, `int`, `long`, `float` and `double` and the corresponding wrapper classes (`Byte`, `Short`, `Integer`, `Long`, `Float` and `Double`).

**String to Integer using radix:**

    String integerAsString = "0101"; // binary representation
    int parseInt = Integer.parseInt(integerAsString,2);
    Integer valueOfInteger = Integer.valueOf(integerAsString,2);
    System.out.println(valueOfInteger); // prints 5 
    System.out.println(parseInt); // prints 5 

**Exceptions**

The unchecked [NumberFormatException][1] exception will be thrown if a numeric `valueOf(String)` or `parseXxx(...)` method is called for a string that is not an acceptable numeric representation, or that represents a value that is out of range.

  [1]: https://docs.oracle.com/javase/7/docs/api/java/lang/NumberFormatException.html

