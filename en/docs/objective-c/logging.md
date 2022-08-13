---
title: "Logging"
slug: "logging"
draft: false
images: []
weight: 9791
type: docs
toc: true
---

## Syntax
- NSLog(@"text to log"); // Basic text log
- NSLog(@"data: %f - %.2f", myFloat, anotherFloat); // Logging text including float numbers.
- NSLog(@"data: %i", myInteger); // Logging text including integer number.
- NSLog(@"data: %@", myStringOrObject);  // Logging text referencing another String or any NSObject derived object.

For logging various types of objects and data-types refer to: [Objective-C, Format Specifiers][1] 


  [1]: https://www.wikiod.com/objective-c/format-specifiers

## Logging
    NSLog(@"Log Message!");
    NSLog(@"NSString value: %@", stringValue);
    NSLog(@"Integer value: %d", intValue);

The first argument of `NSLog` is an `NSString` containing the log message format. The rest of the parameters are used as values to substitute in place of the format specifiers.

The formatting works exactly the same as `printf`, except for the additional format specifier `%@` for an arbitrary Objective-C object. This:

    NSLog(@"%@", object);

is equivalent to:

    NSLog(@"%s", [object description].UTF8String);

## NSLog Output Format
    NSLog(@"NSLog message");

The message that gets printed by calling `NSLog` has the following format when viewed in Console.app:

| Date         | Time           | Program name | Process ID |     | Thread ID  | Message        |
|---|---|---|---|---|---|---
|`2016-07-16`|`08:58:04.681`|`test`       | `[46259`   | `:` | `1244773]` | `  NSLog message` |


## Logging Variable Values
You shouldn't call `NSLog` without a literal format string like this:

    NSLog(variable);    // Dangerous code!

If the variable is not an `NSString`, the program will crash, because `NSLog` expects an `NSString`.
 
If the variable is an `NSString`, it will work unless your string contains a `%`. `NSLog` will parse the `%` sequence as a format specifier and then read a garbage value off the stack, causing a crash or even [executing arbitrary code](https://en.wikipedia.org/wiki/Uncontrolled_format_string).

Instead, always make the first argument a format specifier, like this:

    NSLog(@"%@", anObjectVariable);
    NSLog(@"%d", anIntegerVariable);


## Removing Log Statements from Release Builds
Messages printed from `NSLog` are displayed on Console.app even in the release build of your app, which doesn't make sense for printouts that are only useful for debugging. To fix this, you can use this macro for debug logging instead of `NSLog`.

    #ifdef DEBUG
    #define DLog(...) NSLog(__VA_ARGS__)
    #else
    #define DLog(...)
    #endif

To use:

    NSString *value = @"value 1";
    DLog(@"value = %@", value);
    // little known fact: programmers look for job postings in Console.app
    NSLog(@"We're hiring!"); 

In debug builds, `DLog` will call `NSLog`. In release builds, `DLog` will do nothing.

## Empty message is not printed
When `NSLog` is asked to print empty string, it omits the log completely.

    NSString *name = @"";
    NSLog(@"%@", name);  // Resolves to @""

The above code will print **nothing**.

It is a good practice to prefix logs with labels:

    NSString *name = @"";
    NSLog(@"Name: %@", name);  // Resolves to @"Name: "

The above code will print:

```text
2016-07-21 14:20:28.623 App[87711:6153103] Name: 
```

## Using __FUNCTION __
    NSLog(@"%s %@",__FUNCTION__, @"etc etc");

Inserts the class and method name into the output:

    2016-07-22 12:51:30.099 loggingExample[18132:2971471] -[ViewController viewDidLoad] etc etc


## NSLog vs printf
    NSLog(@"NSLog message");
    printf("printf message\n");

Output:

<!-- language: lang-none -->

    2016-07-16 08:58:04.681 test[46259:1244773] NSLog message
    printf message

`NSLog` outputs the date, time, process name, process ID, and thread ID in addition to the log message. `printf` just outputs the message.

`NSLog` requires an `NSString` and automatically adds a newline at the end. `printf` requires a C string and does not automatically add a newline.


`NSLog` sends output to `stderr`, `printf` sends output to `stdout`.

____
Some `format-specifiers` in `printf` vs `NSLog` are different. For example when including a nested string, the following differences incur:

<!-- language: Objective-C-->
    NSLog(@"My string: %@", (NSString *)myString);
    printf("My string: %s", [(NSString *)myString UTF8String]);




## Logging NSLog meta data
    NSLog(@"%s %d %s, yourVariable: %@", __FILE__, __LINE__, __PRETTY_FUNCTION__, yourVariable);

Will log the file, line number and function data along with any variables you want to log. This can make the log lines much longer, particularly with verbose file and method names, however it can help to speed up error diagnostics.

You can also wrap this in a Macro (store this in a Singleton or where you'll need it most);

    #define ALog(fmt, ...) NSLog((@"%s [Line %d] " fmt), __PRETTY_FUNCTION__, __LINE__, ##__VA_ARGS__);

Then when you want to log, simply call
    
    ALog(@"name: %@", firstName);

Which will give you something like;

     -[AppDelegate application:didFinishLaunchingWithOptions:] [Line 27] name: John


## NSLog and BOOL type
There is no format specifier to print boolean type using NSLog. One way to print boolean value is to convert it to a string.

    BOOL boolValue = YES;
    NSLog(@"Bool value %@", boolValue ? @"YES" : @"NO");

Output:

    2016-07-30 22:53:18.269 Test[4445:64129] Bool value YES

____

Another way to print boolean value is to cast it to integer, achieving a binary output (1=yes, 0=no).

    BOOL boolValue = YES;
    NSLog(@"Bool value %i", boolValue);

Output:

    2016-07-30 22:53:18.269 Test[4445:64129] Bool value 1

## Logging by Appending to a File
NSLog is good, but you can also log by appending to a file instead, using code like:

    NSFileHandle* fh = [NSFileHandle fileHandleForWritingAtPath:path];
    if ( !fh ) {
        [[NSFileManager defaultManager] createFileAtPath:path contents:nil attributes:nil];
        fh = [NSFileHandle fileHandleForWritingAtPath:path];
    }
    if ( fh ) {
        @try {
            [fh seekToEndOfFile];
            [fh writeData:[self dataUsingEncoding:enc]];
        }
        @catch (...) {
        }
        [fh closeFile];
    }


