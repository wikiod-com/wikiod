---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## String types
Delphi has the following string types (in order of popularity):

| Type   | Maximum length | Minimum size | Description | 
| ------ | -------------- | ------------ | ----------- |
| `string`| 2GB | 16 bytes| A managed string. An alias for `AnsiString` through Delphi 2007, and an alias for `UnicodeString` as of Delphi 2009. |
|`UnicodeString`|2GB|16 bytes| A managed string in UTF-16 format.
|`AnsiString`|2GB|16 bytes| A managed string in pre-Unicode ANSI format. As of Delphi 2009, it carries an explicit code-page indicator.
|`UTF8String`|2GB|16 bytes| A managed string in UTF-8 format, implemented as an `AnsiString` with a UTF-8 code page.
|`ShortString`| 255 chars | 2 bytes | A legacy, fixed-length, unmanaged string with very little overhead
|`WideString`| 2GB|4 bytes| Intended for COM interop, a managed string in UTF-16 format. Equivalent to the Windows `BSTR` type.

`UnicodeString` and `AnsiString` are [reference counted][1] and [copy-on-write][2] (COW).  
`ShortString` and `WideString` are not reference counted and do not have COW semantics.  

[1]: https://en.wikipedia.org/wiki/Reference_counting#Delphi
[2]: https://en.wikipedia.org/wiki/Copy-on-write

## Assignment
  Assigning string to different string types and how the runtime environment behaves regarding them. Memory allocation, reference counting, indexed access to chars and compiler errors described briefly where applicable.    

    var
      SS5: string[5]; {a shortstring of 5 chars + 1 length byte, no trailing `0`}
      WS: Widestring; {managed pointer, with a bit of compiler support}
      AS: ansistring; {ansistring with the default codepage of the system}
      US: unicodestring; {default string type}
      U8: UTF8string;//same as AnsiString(65001)
      A1251: ansistring(1251); {ansistring with codepage 1251: Cryllic set}
      RB: RawbyteString; {ansistring with codepage 0: no conversion set}
    begin
      SS5:= 'test'; {S[0] = Length(SS254) = 4, S[1] = 't'...S[5] = undefined}
      SS5:= 'test1'; {S[0] = 5, S[5] = '1', S[6] is out of bounds}
      SS5:= 'test12'; {compile time error}
      WS:= 'test'; {WS now points to a constant unicodestring hard compiled into the data segment}
      US:= 'test'+IntToStr(1); {New unicode string is created with reference count = 1}
      WS:= US; {SysAllocateStr with datacopied to dest, US refcount = 1 !}
      AS:= US; {the UTF16 in US is converted to "extended" ascii taking into account the codepage in AS possibly losing data in the process}  
      U8:= US; {safe copy of US to U8, all data is converted from UTF16 into UTF8}
      RB:= US; {RB = 'test1'#0 i.e. conversion into RawByteString uses system default codepage}
      A1251:= RB; {no conversion takes place, only reference copied. Ref count incremented }

## Reference counting
Counting references on strings is thread-safe. Locks and exception handlers are used to safeguard the process. Consider the following code, with comments indicating where the compiler inserts code at compile time to manage reference counts:

    procedure PassWithNoModifier(S: string);
    // prologue: Increase reference count of S (if non-negative),
    //           and enter a try-finally block
    begin
      // Create a new string to hold the contents of S and 'X'. Assign the new string to S,
      // thereby reducing the reference count of the string S originally pointed to and
      // brining the reference count of the new string to 1.
      // The string that S originally referred to is not modified.
      S := S + 'X';
    end;
    // epilogue: Enter the `finally` section and decrease the reference count of S, which is
    //           now the new string. That count will be zero, so the new string will be freed.
        
    procedure PassWithConst(const S: string);
    var
      TempStr: string;
    // prologue: Clear TempStr and enter a try-finally block. No modification of the reference
    //           count of string referred to by S.
    begin
      // Compile-time error: S is const.
      S := S + 'X';
      // Create a new string to hold the contents of S and 'X'. TempStr gets a reference count
      // of 1, and reference count of S remains unchanged.
      TempStr := S + 'X';
    end;
    // epilogue: Enter the `finally` section and decrease the reference count of TempStr,
    //           freeing TempStr because its reference count will be zero.

As shown above, introducing temporary local string to hold the modifications to a parameter involves the same overhead as making modifications directly to that parameter. Declaring a string `const` only avoids reference counting when the string parameter is truly read-only. However, to avoid leaking implementation details outside a function, it is advisable to always use one of `const`, `var`, or `out` on string parameter.

## Strings


## Chars
<!-- if version [gte 2009] -->
    uses
      Character;
    
    var
      C1, C2: Char;
    begin
      C1 := 'F';
      C2 := ToLower(C1); // Convert the char to lower-case
      C1 := ToUpper(C2); // Convert the char to upper-case
<!-- end version if -->

The `uses` clause should be `System.Character` if version is  XE2 or above.

## Encodings
String types like UnicodeString, AnsiString, WideString and UTF8String are stored in a memory using their respective encoding (see String Types for more details). Assigning one type of string into another may result in a conversion. Type string is designed to be encoding independent - you should never use its internal representation.

The class `Sysutils.TEncoding` provides method `GetBytes` for converting `string` to `TBytes` (array of bytes) and `GetString`  for converting `TBytes` to `string`. The class `Sysutils.TEncoding` also provides many predefined encodings as class properties.

One way how to deal with encodings is to use only `string` type in your application and use `TEncoding` every time you need to use specific encoding - typically in I/O operations, DLL calls, etc...

    procedure EncodingExample;
    var hello,response:string;
        dataout,datain:TBytes;
        expectedLength:integer;
        stringStream:TStringStream;
        stringList:TStringList;
         
    begin
      hello := 'Hello World!Привет мир!';
      dataout := SysUtils.TEncoding.UTF8.GetBytes(hello); //Conversion to UTF8
      datain := SomeIOFunction(dataout); //This function expects input as TBytes in UTF8 and returns output as UTF8 encoded TBytes.
      response := SysUtils.TEncoding.UTF8.GetString(datain); //Convertsion from UTF8

      //In case you need to send text via pointer and length using specific encoding (used mostly for DLL calls)
      dataout := SysUtils.TEncoding.GetEncoding('ISO-8859-2').GetBytes(hello); //Conversion to ISO 8859-2
      DLLCall(addr(dataout[0]),length(dataout));
      //The same is for cases when you get text via pointer and length
      expectedLength := DLLCallToGetDataLength();
      setLength(datain,expectedLength);
      DLLCall(addr(datain[0]),length(datain));
      response := Sysutils.TEncoding.GetEncoding(1250).getString(datain);

       //TStringStream and TStringList can use encoding for I/O operations
       stringList:TStringList.create;
       stringList.text := hello;
       stringList.saveToFile('file.txt',SysUtils.TEncoding.Unicode);
       stringList.destroy;
       stringStream := TStringStream(hello,SysUtils.TEncoding.Unicode);
       stringStream.saveToFile('file2.txt');
       stringStream.Destroy;
    end;

## UPPER and lower case
    uses
      SysUtils;

    var
      S1, S2: string;
    begin
      S1 := 'Foo';
      S2 := LowerCase(S1); // S2 := 'foo';
      S1 := UpperCase(S2); // S1 := 'FOO';

