---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

In .NET strings `System.String` are sequence of characters `System.Char`, each character is an UTF-16 encoded code-unit. This distinction is important because _spoken language_ definition of _character_ and .NET (and many other languages) definition of character are different.

One _character_, which should be correctly called [grapheme][1], it's displayed as a [glyph][2] and it is defined by one or more Unicode [code-points][3]. Each code-point is then encoded in a sequence of [code-units][4]. Now it should be clear why a single `System.Char` does not always represent a grapheme, let's see in real world how they're different:

* One grapheme, because of [combining characters][5], may result in two or more code-points: <kbd>à</kbd> is composed by two code-points: _U+0061 LATIN SMALL LETTER A_ and _U+0300 COMBINING GRAVE ACCENT_. This is the most common mistake because `"à".Length == 2` while you may expect `1`.
* There are duplicated characters, for example <kbd>à</kbd> may be a single code-point _U+00E0 LATIN SMALL LETTER A WITH GRAVE_ or two code-points as explained above. Obviously they must compare the same: `"\u00e0" == "\u0061\u0300"` (even if `"\u00e0".Length != "\u0061\u0300".Length`). This is possible because of _string normalization_ performed by `String.Normalize()` method.
* An Unicode sequence may contain a composed or decomposed sequence, for example character <kbd>한</kbd> _U+D55C HAN CHARACTER_ may be a single code-point (encoded as a single code-unit in UTF-16) or a decomposed sequence of its syllables <kbd>ᄒ</kbd>, <kbd>ᅡ</kbd> and <kbd>ᆫ</kbd>. They must be compared equal.
* One code-point may be encoded to more than one code-units: character <kbd>𠂊</kbd> _U+2008A HAN CHARACTER_ is encoded as two `System.Char` (`"\ud840\udc8a"`) even if it is just one code-point: UTF-16 encoding is not fixed size! This is a source of countless bugs (also serious security bugs), if for example your application applies a maximum length and blindly truncates string at that then you may create an invalid string.
* Some languages have [digraph][6] and trigraphs, for example in Czech <kbd>ch</kbd> is a standalone letter (after <kbd>h</kbd> and before <kbd>i</kbd> then when ordering a list of strings you will have *fyzika* before *chemie*.

There are much more issues about text handling, see for example [How can I perform a Unicode aware character by character comparison?][7] for a broader introduction and more links to related arguments.

In general when dealing with _international_ text you may use this simple function to enumerate text elements in a string (avoiding to break Unicode surrogates and encoding):

    public static class StringExtensions
    {
        public static IEnumerable<string> EnumerateCharacters(this string s)
        {
            if (s == null)
                return Enumerable.Empty<string>();

            var enumerator = StringInfo.GetTextElementEnumerator(s.Normalize());
            while (enumerator.MoveNext())
                yield return (string)enumerator.Value;
        }
    }


  [1]: https://en.wikipedia.org/wiki/Grapheme
  [2]: https://en.wikipedia.org/wiki/Glyph
  [3]: https://en.wikipedia.org/wiki/Code_point
  [4]: https://en.wikipedia.org/wiki/Character_encoding#Code_unit
  [5]: https://en.wikipedia.org/wiki/Combining_character
  [6]: https://en.wikipedia.org/wiki/Digraph_(orthography)
  [7]: http://stackoverflow.com/q/27229589/1207195

## Count characters
If you need to count _characters_ then, for the reasons explained in _Remarks_ section, you can't simply use Length property because it's the length of the array of `System.Char` which are not characters but code-units (not Unicode code-points nor graphemes). Correct code is then:

    int length = text.EnumerateCharacters().Count();

A small optimization may rewrite `EnumerateCharacters()` extension method specifically for this purpose:

    public static class StringExtensions
    {
        public static int CountCharacters(this string text)
        {
            if (String.IsNullOrEmpty(text))
                return 0;
    
            int count = 0;
            var enumerator = StringInfo.GetTextElementEnumerator(text);
            while (enumerator.MoveNext())
                ++count;
    
            return count;
        }
    }

## Count distinct characters
If you need to count distinct characters then, for the reasons explained in *Remarks* section, you can't simply use `Length` property because it's the length of the array of `System.Char` which are not characters but code-units (not Unicode code-points nor graphemes). If, for example, you simply write `text.Distinct().Count()` you will get incorrect results, correct code:

    int distinctCharactersCount = text.EnumerateCharacters().Count();

One step further is to **count occurrences of each character**, if performance aren't an issue you may simply do it like this (in this example regardless of case):

    var frequencies = text.EnumerateCharacters()
        .GroupBy(x => x, StringComparer.CurrentCultureIgnoreCase)
        .Select(x => new { Character = x.Key, Count = x.Count() };

## Convert string to/from another encoding
.NET strings contain `System.Char` (UTF-16 code-units). If you want to save (or manage) text with another encoding you have to work with an array of `System.Byte`. 

Conversions are performed by classes derived from `System.Text.Encoder` and `System.Text.Decoder` which, together, can convert to/from another encoding (from a byte _X_ encoded array `byte[]` to an UTF-16 encoded `System.String` and vice-versa).

Because the encoder/decoder usually works very close to each other they're grouped together in a class derived from `System.Text.Encoding`, derived classes offer conversions to/from popular encodings (UTF-8, UTF-16 and so on).

Examples:
=

Convert a string to UTF-8
-
    byte[] data = Encoding.UTF8.GetBytes("This is my text");
---
Convert UTF-8 data to a string
-
    var text = Encoding.UTF8.GetString(data);

---
Change encoding of an existing text file
-

This code will read content of an UTF-8 encoded text file and save it back encoded as UTF-16. Note that this code is not optimal if file is big because it will read all its content into memory:

    var content = File.ReadAllText(path, Encoding.UTF8);
    File.WriteAllText(content, Encoding.UTF16);

## Сomparing strings
Despite `String` is a reference type `==` operator compares string values rather than references.

As you may know `string` is just an array of characters. But if you think that strings equality check and comparison is made character by character, you are mistaken. This operation is culture specific (see Remarks below): some character sequences can be treated as equal depending on the [culture][1]. 

Think twice before short circuiting equality check by comparing `Length` [properties][2] of two strings!

Use overloads of `String.Equals` [method][3] which accept additional `StringComparison` [enumeration][4] value, if you need to change default behavior.


  [1]: https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.currentculture(v=vs.110).aspx
  [2]: https://msdn.microsoft.com/library/system.string.length(v=vs.110).aspx
  [3]: https://msdn.microsoft.com/en-us/library/t4411bks(v=vs.110).aspx
  [4]: https://msdn.microsoft.com/en-us/library/system.stringcomparison(v=vs.110).aspx

## Count occurrences of a character
Because of the reasons explained in _Remarks_ section you can't simply do this (unless you want to count occurrences of a specific code-unit):

    int count = text.Count(x => x == ch);

You need a more complex function:

    public static int CountOccurrencesOf(this string text, string character)
    {
        return text.EnumerateCharacters()
            .Count(x => String.Equals(x, character, StringComparer.CurrentCulture));
    }

Note that string comparison (in contrast to character comparison which is culture invariant) must always be performed according to rules to a specific culture.

## Split string into fixed length blocks
We cannot break a string into arbitrary points (because a `System.Char` may not be valid alone because it's a combining character or part of a surrogate) then code must take that into account (note that with _length_ I mean the number of _graphemes_ not the number of _code-units_):

    public static IEnumerable<string> Split(this string value, int desiredLength)
    {
        var characters = StringInfo.GetTextElementEnumerator(value);
        while (characters.MoveNext())
            yield return String.Concat(Take(characters, desiredLength));
    }
    
    private static IEnumerable<string> Take(TextElementEnumerator enumerator, int count)
    {
        for (int i = 0; i < count; ++i)
        {
            yield return (string)enumerator.Current;
    
            if (!enumerator.MoveNext())
                yield break;
        }
    }

## Object.ToString() virtual method
Everything in .NET is an object, hence every type has `ToString()` [method][1] defined in `Object` [class][2] which can be overridden. Default implementation of this method just returns the name of the type:

    public class Foo
    {
    }
    
    var foo = new Foo();
    Console.WriteLine(foo); // outputs Foo

`ToString()` is implicitly called when concatinating value with a string:

    public class Foo
    {
        public override string ToString()
        {
            return "I am Foo";
        }
    }
    
    var foo = new Foo();
    Console.WriteLine("I am bar and "+foo);// outputs I am bar and I am Foo

The result of this method is also extensively used by debugging tools. If, for some reason, you do not want to override this method, but want to customize how debugger shows the value of your type, use [DebuggerDisplay Attribute][4] ([MSDN][3]):

    // [DebuggerDisplay("Person = FN {FirstName}, LN {LastName}")]
    [DebuggerDisplay("Person = FN {"+nameof(Person.FirstName)+"}, LN {"+nameof(Person.LastName)+"}")]
    public class Person
    {
        public string FirstName { get; set; }
        public string LastName { get; set;}
        // ...
    }


  [1]: https://msdn.microsoft.com/en-us/library/system.object.tostring(v=vs.110).aspx
  [2]: https://msdn.microsoft.com/en-us/library/system.object(v=vs.110).aspx
  [3]: https://msdn.microsoft.com/en-us/library/system.diagnostics.debuggerdisplayattribute(v=vs.110).aspx
  [4]: https://www.wikiod.com/docs/c%23/1062/attributes/4689/debuggerdisplay-attribute#t=201702221225586559231

## Immutability of strings
Strings are immutable. You just cannot change existing string. Any operation on the string crates a new instance of the string having new value. It means that if you need to replace a single character in a very long string, memory will be allocated for a new value.

    string veryLongString = ...
    // memory is allocated
    string newString = veryLongString.Remove(0,1); // removes first character of the string.

If you need to perform many operations with string value, use `StringBuilder` [class][1] which is designed for efficient strings manipulation:

    var sb = new StringBuilder(someInitialString);
    foreach(var str in manyManyStrings)
    {
        sb.Append(str);
    } 
    var finalString = sb.ToString();

  [1]: https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

