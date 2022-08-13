---
title: "Strings and Characters"
slug: "strings-and-characters"
draft: false
images: []
weight: 9082
type: docs
toc: true
---

## Syntax
 - String.characters // Returns an Array of the characters in the String
 - String.characters.count // Returns the number of characters
 - String.utf8 // A String.UTF8View, returns the UTF-8 character points in the String
 - String.utf16 // A String.UTF16View, returns the UTF-16 character points in the String
 - String.unicodeScalars // A String.UnicodeScalarView, returns the UTF-32 character points in the String
 - String.isEmpty // Returns true if the String does not contain any text
 - String.hasPrefix(String) // Returns true if the String is prefixed with the argument
 - String.hasSuffix(String) // Returns true if the String is suffixed with the argument
- String.startIndex // Returns the Index that corresponds to the first character in the string
- String.endIndex // Returns the Index that corresponds to the spot after the last character in the string
- String.components(separatedBy: String) // Returns an array containing the substrings separated by the given separator string
- String.append(Character) // Adds the character (given as argument) to the String

A `String` in Swift is a collection of characters, and by extension a collection of Unicode scalars. Because Swift Strings are based on Unicode, they may be any Unicode scalar value, including languages other than English and emojis.

Because two scalars could combine to form a single character, the number of scalars in a String is not necessarily always the same as the number of characters.

For more information about Strings, see [The Swift Programming Language](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html) and the [String Structure Reference](https://developer.apple.com/library/tvos/documentation/Swift/Reference/Swift_String_Structure/index.html).

For implementation details, see ["Swift String Design"](https://github.com/apple/swift/blob/master/docs/StringDesign.rst)

## String & Character Literals
[String][1] literals in Swift are delimited with double quotes (`"`):

    let greeting = "Hello!"  // greeting's type is String

[Characters][2] can be initialized from string literals, as long as the literal contains only one grapheme cluster:

    let chr: Character = "H" // valid
    let chr2: Character = "😊" // valid
    let chr3: Character = "abc" // invalid - multiple grapheme clusters


# String Interpolation

[**String interpolation**](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html#//apple_ref/doc/uid/TP40014097-CH7-ID292) allows injecting an expression directly into a string literal. This can be done with all types of values, including strings, integers, floating point numbers and more.

The syntax is a backslash followed by parentheses wrapping the value: `\(value)`. Any valid expression may appear in the parentheses, including function calls.

    let number = 5
    let interpolatedNumber = "\(number)"  // string is "5"
    let fortyTwo = "\(6 * 7)"             // string is "42"

    let example = "This post has \(number) view\(number == 1 ? "" : "s")"
    // It will output "This post has 5 views" for the above example.
    // If the variable number had the value 1, it would output "This post has 1 view" instead.

For custom types, the [default behavior](https://github.com/apple/swift/blob/master/stdlib/public/core/StringInterpolation.swift.gyb) of string interpolation is that `"\(myobj)"` is equivalent to `String(myobj)`, the same representation used by `print(myobj)`. You can customize this behavior by implementing the [`CustomStringConvertible` protocol](https://developer.apple.com/library/ios/documentation/Swift/Reference/Swift_CustomStringConvertible_Protocol/index.html) for your type.

<!-- if version [gte 3.0] -->
For Swift 3, in accordance with [SE-0089](https://github.com/apple/swift-evolution/blob/master/proposals/0089-rename-string-reflection-init.md), `String.init<T>(_:)` has been renamed to `String.init<T>(describing:)`.

The string interpolation `"\(myobj)"` will prefer the new `String.init<T: LosslessStringConvertible>(_:)` initializer, but will fall back to `init<T>(describing:)` if the value is not `LosslessStringConvertible`.
<!-- end version if -->


# Special Characters

Certain characters require a special **escape sequence** to use them in string literals:

Character | Meaning
--- | ---
`\0` | the null character
`\\` | a plain backslash, `\`
`\t` | a tab character
`\v` | a vertical tab
`\r` | a [carriage return](https://en.wikipedia.org/wiki/Newline)
`\n` | a [line feed](https://en.wikipedia.org/wiki/Newline) ("newline")
`\"` | a double quote, `"`
`\'` | a single quote, `'`
`\u{n}` | the Unicode code point *n* (in hexadecimal)

**Example:**

    let message = "Then he said, \"I \u{1F496} you!\""

    print(message) // Then he said, "I 💖 you!"


  [1]: https://developer.apple.com/reference/swift/string
  [2]: https://developer.apple.com/reference/swift/character

## Concatenate strings
Concatenate strings with the `+` operator to produce a new string:

```
let name = "John"
let surname = "Appleseed"
let fullName = name + " " + surname  // fullName is "John Appleseed"
```

Append to a [**mutable**](http://stackoverflow.com/questions/24002092/what-is-the-difference-between-let-and-var-in-swift) string using the [`+=`][1] [compound assignment operator][2], or using a method:

```
let str2 = "there"
var instruction = "look over"
instruction += " " + str2  // instruction is now "look over there"

var instruction = "look over"
instruction.append(" " + str2)  // instruction is now "look over there"
```

Append a single character to a mutable String:

```
var greeting: String = "Hello"
let exclamationMark: Character = "!"
greeting.append(exclamationMark) 
// produces a modified String (greeting) = "Hello!"
```

Append multiple characters to a mutable String

```
var alphabet:String = "my ABCs: "
alphabet.append(contentsOf: (0x61...0x7A).map(UnicodeScalar.init)
                                         .map(Character.init) )
// produces a modified string (alphabet) = "my ABCs: abcdefghijklmnopqrstuvwxyz"
```

<!-- if version [gte 3.0] -->

`appendContentsOf(_:)` has been renamed to [`append(_:)`][3].

<!-- end version if -->


Join a [sequence][4] of strings to form a new string using [`joinWithSeparator(_:)`][5]:

    let words = ["apple", "orange", "banana"]
    let str = words.joinWithSeparator(" & ")
    
    print(str)   // "apple & orange & banana"

<!-- if version [gte 3.0] -->

`joinWithSeparator(_:)` has been renamed to [`joined(separator:)`][6].

The `separator` is the empty string by default, so `["a", "b", "c"].joined() == "abc"`.

<!-- end version if -->


  [1]: https://developer.apple.com/reference/swift/1540630
  [2]: https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/BasicOperators.html#//apple_ref/doc/uid/TP40014097-CH6-ID69
  [3]: http://swiftdoc.org/v3.0/type/String/#func-append_-string
  [4]: http://swiftdoc.org/v2.2/protocol/SequenceType/
  [5]: https://developer.apple.com/library/tvos/documentation/Swift/Reference/Swift_SequenceType_Protocol/index.html#//apple_ref/swift/intfm/SequenceType/s:FesRxs12SequenceTypeWx9Generator7Element_zSSrS_17joinWithSeparatorFSSSS
  [6]: http://swiftdoc.org/v3.0/protocol/Sequence/#func-iterator-element-string-joined_

## String Encoding and Decomposition
A Swift [String](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html) is made of [Unicode](http://www.unicode.org/standard/standard.html) code points. It can be decomposed and encoded in several different ways.

    let str = "ที่👌①!"

# Decomposing Strings

A string's `characters` are Unicode [extended grapheme clusters](https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/Strings/Articles/stringsClusters.html):

    Array(str.characters)  // ["ที่", "👌", "①", "!"]

The `unicodeScalars` are the Unicode [code points](https://en.wikipedia.org/wiki/Code_point) that make up a string (notice that `ที่` is one grapheme cluster, but 3 code points — 3607, 3637, 3656 — so the length of the resulting array is not the same as with `characters`):

    str.unicodeScalars.map{ $0.value }  // [3607, 3637, 3656, 128076, 9312, 33]

You can encode and decompose strings as [UTF-8](https://en.wikipedia.org/wiki/UTF-8) (a sequence of `UInt8`s) or [UTF-16](https://en.wikipedia.org/wiki/UTF-16) (a sequence of `UInt16`s):

    Array(str.utf8)   // [224, 184, 151, 224, 184, 181, 224, 185, 136, 240, 159, 145, 140, 226, 145, 160, 33]
    Array(str.utf16)  // [3607, 3637, 3656, 55357, 56396, 9312, 33]

# String Length and Iteration

A string's `characters`, `unicodeScalars`, `utf8`, and `utf16` are all [Collection][1]s, so you can get their `count` and iterate over them:

    // NOTE: These operations are NOT necessarily fast/cheap! 

    str.characters.count     // 4
    str.unicodeScalars.count // 6
    str.utf8.count           // 17
    str.utf16.count          // 7
<!---->
    for c in str.characters { // ...
    for u in str.unicodeScalars { // ...
    for byte in str.utf8 { // ...
    for byte in str.utf16 { // ...


  [1]: https://developer.apple.com/library/ios/documentation/Swift/Reference/Swift_CollectionType_Protocol/index.html

## Examine and compare strings
Check whether a string is empty:

    if str.isEmpty {
        // do something if the string is empty
    }

    // If the string is empty, replace it with a fallback:
    let result = str.isEmpty ? "fallback string" : str

Check whether two strings are equal (in the sense of [Unicode canonical equivalence](http://www.unicode.org/glossary/#canonical_equivalent)):

    "abc" == "def"          // false
    "abc" == "ABC"          // false
    "abc" == "abc"          // true

    // "LATIN SMALL LETTER A WITH ACUTE" == "LATIN SMALL LETTER A" + "COMBINING ACUTE ACCENT"
    "\u{e1}" == "a\u{301}"  // true
    
Check whether a string starts/ends with another string:

    "fortitude".hasPrefix("fort")      // true
    "Swift Language".hasSuffix("age")  // true

## Reversing Strings
<!-- if version [eq 2.2] -->
    let aString = "This is a test string."

    // first, reverse the String's characters 
    let reversedCharacters = aString.characters.reverse()

    // then convert back to a String with the String() initializer
    let reversedString = String(reversedCharacters)

    print(reversedString) // ".gnirts tset a si sihT"
<!-- end version if -->

<!-- if version [eq 3.0] -->
    let reversedCharacters = aString.characters.reversed()
    let reversedString = String(reversedCharacters)
<!-- end version if -->

## Check if String contains Characters from a Defined Set


**Letters**

<!-- if version [eq 3.0] -->
    let letters = CharacterSet.letters

    let phrase = "Test case"
    let range = phrase.rangeOfCharacter(from: letters)

    // range will be nil if no letters is found
    if let test = range {
       print("letters found")
    }
    else {
       print("letters not found")
    }

<!-- end version if -->

<!-- if version [eq 2.2] -->
    let letters = NSCharacterSet.letterCharacterSet()

    let phrase = "Test case"
    let range = phrase.rangeOfCharacterFromSet(letters)

    // range will be nil if no letters is found
    if let test = range {
       print("letters found")
    }
    else {
      print("letters not found")
    }

<!-- end version if -->
The new `CharacterSet` struct that is also bridged to the Objective-C `NSCharacterSet` class define several predefined sets as:

 - `decimalDigits`
 - `capitalizedLetters`
 - `alphanumerics`
 - `controlCharacters`
 - `illegalCharacters`
 - and more you can find in the [NSCharacterSet](https://developer.apple.com/reference/foundation/nscharacterset) reference.

You also can define your own set of characters:

<!-- if version [eq 3.0] -->
    let phrase = "Test case"
    let charset = CharacterSet(charactersIn: "t")

    if let _ = phrase.rangeOfCharacter(from: charset, options: .caseInsensitive) {
       print("yes") 
    }
    else {
       print("no")
    }
<!-- end version if -->

<!-- if version [eq 2.2] -->
    let charset = NSCharacterSet(charactersInString: "t")

    if let _ = phrase.rangeOfCharacterFromSet(charset, options: .CaseInsensitiveSearch, range: nil) {
       print("yes")
    }
    else {
        print("no")
    }

<!-- end version if -->

You can also include range: 
<!-- if version [eq 3.0] -->
    let phrase = "Test case"
    let charset = CharacterSet(charactersIn: "t")

    if let _ = phrase.rangeOfCharacter(from: charset, options: .caseInsensitive, range: phrase.startIndex..<phrase.endIndex)) {
       print("yes") 
    }
    else {
       print("no")
    }
<!-- end version if -->

## String Iteration
<!-- if version [lt 3.0] -->
    let string = "My fantastic string"
    var index = string.startIndex

    while index != string.endIndex {
        print(string[index])
        index = index.successor()
    }

Note: `endIndex` is after the end of the string (i.e. `string[string.endIndex]` is an error, but `string[string.startIndex]` is fine). Also, in an empty string (`""`), `string.startIndex == string.endIndex` is `true`. Be sure to check for empty strings, since you cannot call `startIndex.successor()` on an empty string.
<!-- end version if -->

<!-- if version [eq 3.0] -->
 In Swift 3, String indexes no longer have `successor()`, `predecessor()`, `advancedBy(_:)`, `advancedBy(_:limit:)`, or `distanceTo(_:)`.
 
Instead, those operations are moved to the collection, which is now responsible for incrementing and decrementing its indices.
 
Available methods are `.index(after:)`, `.index(before:)` and `.index(_:, offsetBy:)`.

    let string = "My fantastic string"
    var currentIndex = string.startIndex
    
    while currentIndex != string.endIndex {
        print(string[currentIndex])
        currentIndex = string.index(after: currentIndex)
    }

*Note: we're using `currentIndex` as a variable name to avoid confusion with the `.index` method.*

<!-- end version if -->

And, for example, if you want to go the other way:

<!-- if version [lt 3.0] -->
    var index:String.Index? = string.endIndex.predecessor()

    while index != nil {
        print(string[index!])
        if index != string.startIndex {
            index = index.predecessor()
        }
        else {
            index = nil
        }
    }

(Or you could just reverse the string first, but if you don't need to go all the way through the string you probably would prefer a method like this)

<!-- end version if -->

<!-- if version [eq 3.0] -->

    var currentIndex: String.Index? = string.index(before: string.endIndex)
    
    while currentIndex != nil {
        print(string[currentIndex!])
        if currentIndex != string.startIndex {
            currentIndex = string.index(before: currentIndex!)
        }
        else {
            currentIndex = nil
        }
    }

<!-- end version if -->

Note, `Index` is an object type, and not an `Int`. You cannot access a character of string as follows:
    
    let string = "My string"
    string[2] // can't do this
    string.characters[2] // and also can't do this

But you can get a specific index as follows:

<!-- if version [lt 3.0] -->

    index = string.startIndex.advanceBy(2)

<!-- end version if -->

<!-- if version [eq 3.0] -->

    currentIndex = string.index(string.startIndex, offsetBy: 2)

<!-- end version if -->

And can go backwards like this:

<!-- if version [lt 3.0] -->

    index = string.endIndex.advancedBy(-2)

<!-- end version if -->

<!-- if version [eq 3.0] -->

    currentIndex = string.index(string.endIndex, offsetBy: -2)

<!-- end version if -->

If you might exceed the string's bounds, or you want to specify a limit you can use:

<!-- if version [lt 3.0] -->

    index = string.startIndex.advanceBy(20, limit: string.endIndex)

<!-- end version if -->

<!-- if version [eq 3.0] -->

    currentIndex = string.index(string.startIndex, offsetBy: 20, limitedBy: string.endIndex)

<!-- end version if -->

Alternatively one can just iterate through the characters in a string, but this might be less useful depending on the context:

    for c in string.characters {
        print(c)
    }



## Unicode
# Setting values

**Using Unicode directly**

    var str: String = "I want to visit 北京, Москва, मुंबई, القاهرة, and 서울시. 😊"
    var character: Character = "🌍"

**Using hexadecimal values**

    var str: String = "\u{61}\u{5927}\u{1F34E}\u{3C0}" // a大🍎π
    var character: Character = "\u{65}\u{301}" // é = "e" + accent mark

Note that the Swift `Character` can be composed of multiple Unicode code points, but appears to be a single character. This is called an Extended Grapheme Cluster.

# Conversions

**String --> Hex**

    // Accesses views of different Unicode encodings of `str`
    str.utf8
    str.utf16
    str.unicodeScalars // UTF-32
    
**Hex --> String**

    let value0: UInt8 = 0x61
    let value1: UInt16 = 0x5927
    let value2: UInt32 = 0x1F34E
    
    let string0 = String(UnicodeScalar(value0)) // a
    let string1 = String(UnicodeScalar(value1)) // 大
    let string2 = String(UnicodeScalar(value2)) // 🍎

    // convert hex array to String
    let myHexArray = [0x43, 0x61, 0x74, 0x203C, 0x1F431] // an Int array
    var myString = ""
    for hexValue in myHexArray {
        myString.append(UnicodeScalar(hexValue))
    }
    print(myString) // Cat‼🐱

Note that for UTF-8 and UTF-16 the conversion is not always this easy because things like emoji cannot be encoded with a single UTF-16 value. It takes a surrogate pair. 

## Splitting a String into an Array
In Swift you can easily separate a String into an array by slicing it at a certain character:

<!-- if version [eq 3.0] -->

    let startDate = "23:51"

    let startDateAsArray = startDate.components(separatedBy: ":") // ["23", "51"]`

<!-- end version if -->

<!-- if version [eq 2.2] -->

    let startDate = "23:51"
    
    let startArray = startDate.componentsSeparatedByString(":") // ["23", "51"]`

<!-- end version if -->

Or when the separator isn't present:

<!-- if version [eq 3.0] -->

    let myText = "MyText"
    
    let myTextArray = myText.components(separatedBy: " ") // myTextArray is ["MyText"]

<!-- end version if -->

<!-- if version [eq 2.2] -->

    let myText = "MyText"
    
    let myTextArray = myText.componentsSeparatedByString(" ") // myTextArray is ["MyText"]

<!-- end version if -->

## Uppercase and Lowercase Strings
To make all the characters in a String uppercase or lowercase:

<!-- if version [eq 2.2] -->
    let text = "AaBbCc"
    let uppercase = text.uppercaseString // "AABBCC"
    let lowercase = text.lowercaseString // "aabbcc"
<!-- end version if -->

<!-- if version [eq 3.0] -->
    let text = "AaBbCc"
    let uppercase = text.uppercased() // "AABBCC"
    let lowercase = text.lowercased() // "aabbcc"
<!-- end version if -->

## Formatting Strings
## Leading Zeros

    let number: Int = 7
    let str1 = String(format: "%03d", number) // 007
    let str2 = String(format: "%05d", number) // 00007

## Numbers after Decimal

    let number: Float = 3.14159
    let str1 = String(format: "%.2f", number) // 3.14
    let str2 = String(format: "%.4f", number) // 3.1416 (rounded)

## Decimal to Hexadecimal

    let number: Int = 13627
    let str1 = String(format: "%2X", number) // 353B
    let str2 = String(format: "%2x", number) // 353b (notice the lowercase b)

Alternatively one could use specialized initializer that does the same:

    let number: Int = 13627
    let str1 = String(number, radix: 16, uppercase: true) //353B
    let str2 = String(number, radix: 16) // 353b

## Decimal to a number with arbitrary radix

    let number: Int = 13627
    let str1 = String(number, radix: 36) // aij

Radix is `Int` in `[2, 36]`.

## Converting Swift string to a number type
    Int("123") // Returns 123 of Int type
    Int("abcd") // Returns nil
    Int("10") // Returns 10 of Int type
    Int("10", radix: 2) // Returns 2 of Int type
    Double("1.5") // Returns 1.5 of Double type
    Double("abcd") // Returns nil

Note that doing this returns an [`Optional`](https://www.wikiod.com/swift/optionals) value, which should be [unwrapped](https://www.wikiod.com/swift/optionals#Unwrapping an Optional) accordingly before being used.

## Convert String to and from Data / NSData
To convert String to and from Data / NSData we need to encode this string with a specific encoding. The most famous one is `UTF-8` which is an 8-bit representation of Unicode characters, suitable for transmission or storage by ASCII-based systems. Here is a list of all available [`String Encodings`](https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Classes/NSString_Class/#//apple_ref/c/tdef/NSStringEncoding)

**`String` to `Data`/`NSData`**

<!-- if version [eq 3.0] -->

    let data = string.data(using: .utf8)

<!-- end version if -->

<!-- if version [eq 2.2] -->

    let data = string.dataUsingEncoding(NSUTF8StringEncoding)

<!-- end version if -->

**`Data`/`NSData` to `String`**

<!-- if version [eq 3.0] -->

    let string = String(data: data, encoding: .utf8)

<!-- end version if -->

<!-- if version [eq 2.2] -->

    let string = String(data: data, encoding: NSUTF8StringEncoding)

<!-- end version if -->


## Count occurrences of a Character into a String
Given a `String` and a `Character`

    let text = "Hello World"
    let char: Character = "o"

We can count the number of times the `Character` appears into the `String` using

    let sensitiveCount = text.characters.filter { $0 == char }.count // case-sensitive
    let insensitiveCount = text.lowercaseString.characters.filter { $0 == Character(String(char).lowercaseString) } // case-insensitive

## Remove characters from a string not defined in Set
<!-- if version [eq 2.2] -->
    func removeCharactersNotInSetFromText(text: String, set: Set<Character>) -> String {
       return String(text.characters.filter { set.contains( $0) })
    }

    let text = "Swift 3.0 Come Out"
    var chars = Set([Character]("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLKMNOPQRSTUVWXYZ".characters))
    let newText = removeCharactersNotInSetFromText(text, set: chars) // "SwiftComeOut"
<!-- end version if -->

<!-- if version [eq 3.0] -->
    func removeCharactersNotInSetFromText(text: String, set: Set<Character>) -> String {
      return String(text.characters.filter { set.contains( $0) })
    }

    let text = "Swift 3.0 Come Out"
    var chars = Set([Character]("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLKMNOPQRSTUVWXYZ".characters))
    let newText = removeCharactersNotInSetFromText(text: text, set: chars)
<!-- end version if -->

## Remove leading and trailing WhiteSpace and NewLine
<!-- if version [lt 3.0] -->
    let someString = "  Swift Language  \n"
    let trimmedString = someString.stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceAndNewlineCharacterSet())
    // "Swift Language"

Method `stringByTrimmingCharactersInSet` returns a new string made by removing from both ends of the String characters contained in a given character set.

We can also just remove only whitespace or newline.

Removing only whitespace:

    let trimmedWhiteSpace = someString.stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceCharacterSet())
    // "Swift Language  \n"
   
Removing only newline:

    let trimmedNewLine = someString.stringByTrimmingCharactersInSet(NSCharacterSet.newlineCharacterSet())
    // "  Swift Language  "
<!-- end version if -->

<!-- if version [eq 3.0] -->

    let someString = "  Swift Language  \n"

    let trimmedString = someString.trimmingCharacters(in: .whitespacesAndNewlines)
    // "Swift Language"

    let trimmedWhiteSpace = someString.trimmingCharacters(in: .whitespaces)
    // "Swift Language  \n"

    let trimmedNewLine = someString.trimmingCharacters(in: .newlines)
    // "  Swift Language  "

<!-- end version if -->

*Note: all these methods belong to `Foundation`. Use `import Foundation` if Foundation isn't already imported via other libraries like Cocoa or UIKit.*

