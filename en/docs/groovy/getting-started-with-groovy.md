---
title: "Getting started with groovy"
slug: "getting-started-with-groovy"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
There are two common ways to install Groovy.

**Download**

The Groovy binary can be downloaded on the [download](http://groovy-lang.org/download.html) page of the Groovy website. You can unpack archive and add path to `%GROOVY_HOME%/bin/groovy.bat` to the PATH system environment variable, where %GROOVY_HOME% is the directory where Groovy is unpacked.

**SDKMAN**

The other option is to use [SDKMAN](http://sdkman.io/). This option has grown quickly in popularity, and makes managing multiple versions of Groovy very simple. It also supports other applications in the "GR8" ecosphere. This option works very well natively on Linux and Mac, but requires [Cygwin](http://cygwin.com/) on Windows.

Following the instructions on the [Groovy download page](http://groovy-lang.org/download.html), you can take the following steps to install SDKMAN.

`$ curl -s get.sdkman.io | bash`

Once SDKMAN is installed, you now have access to the `sdk` command. With this command you can do many useful things.

*Install Groovy*

`$ sdk install groovy`

This will install the latest version of Groovy.

*List versions of Groovy*

`$ sdk ls groovy`

This allows you to run a Linux style `ls` command on the Groovy software, listing all of the available options. There is an `*` next to each installed version, and a `>` to indicate your current versions.

*Switch versions of Groovy*

`$ sdk use groovy 2.4.7`

This will change the current version of Groovy to 2.4.7. If you have other versions installed, you can switch to any of those.

You can list the current version of groovy with the `groovy -version` command.

**posh-gvm**

The initial name of SDKMAN was GVM and [posh-gvm][1] is a port of GVM for the Windows Powershell. So, if you develop on a Windows machine and don't want to use SDKMAN on Cygwin, posh-gvm is for you. It works the same as SDKMAN, but instead of `sdk`, the command is `gmv`. So

    PS C:\Users\You> gmv install groovy

will install groovy through posh-gvm on your Windows machine.


  [1]: https://github.com/flofreud/posh-gvm

## Hello World In groovy
Following example illustrate the simplest `Hello World` in groovy using script, place the following code snippet in a file, say `helloWorld.groovy`

    println 'Hello World!'

**How to execute:**
In the command line, `groovy helloWorld.groovy`

**Output:**
`Hello World!`

## Using Groovy on a Java project
Groovy has access to all java classes, in fact Groovy classes ARE Java classes and can be run by the JVM directly.  If you are working on a Java project, using Groovy as a simple scripting language to interact with your java code is a no-brainer.

To make things even better, nearly any Java class can be renamed to .groovy and compiled and run and will work exactly as it did, groovy is close to being a super-set of Java, this is a stated goal of groovy.

Groovy has a REPL.  `groovysh` comes with Groovy and can be used to quickly instantiate and test a Java class if your classpath is set up correctly.  For instance if your `classpath` pointed to your eclipse "classes/bin" directory, then you could save your file in eclipse, jump to `groovysh` and instantiate the class to test it.

The reasons to use Groovy to do this instead of just Java are:
The classloader is GREAT at picking up new classes as they are compiled.  You don't generally need to exit/re-start `groovysh` as you develop.

The syntax is TERSE.  This isn't great for maintainable code, but for scripts and tests it can cut your code significantly.  One of the big things it does is eliminate checked exceptions (or, more accurately, turn all checked exceptions into unchecked exceptions).  This turns code like this (Print hello after one second):

    class JavaClass {
        public static void main(String[] args) {
            try {
                Thread.sleep(1000);
            } catch(InterruptedException e) {
                // You shouldn't leave an empty catch block, but who cares if this was interrupted???
            }
            System.out.println("Hello!");
        }
    }

into Groovy's:

    Thread.sleep(1000)
    print "Hello!"

Groovy also has very tight initialization syntax.  This allows you to specify data just as you like it without thinking about it:

In Java to initialize a map you should probably do something like this:

    String[] init = { "1:Bill", "2:Doug", "3:Bev" };
    // Note the rest of this can be put in a function and reused or maybe found in a library, but I always seem to have to write this function!
    Map m = new HashMap<Integer, String>();
    for(String pair : int) {
        String[] split = pair.split(":");
        m.put(new Integer(split[0]), split[1])
    }

This isn't bad, but it's something else to maintain. In groovy you would just use:

    Map map = { 1 : "Bill", 2 : "Doug", 3 : "Bev" }

And you are done. List syntax is just as easy.

The other really big advantage is groovy's closure syntax.  It's amazingly terse and fun, somewhat more difficult to maintain, but for scripts that's not a priority.  As an example, here is some groovy code to find all `.txt` files that contain the word `Hello` in the current directory:

    println new File('.').files.findAll{ it.name.endsWith('.txt') && it.text.contains('Hello') }.collect{ it.name }

This example uses a few "Groovy" tricks:
 - `.files` refers to the `getFiles()` method - groovy can switch between getter/setter and property syntax at will
 - `it.` refers to the current element of an iteration. `{ it }` is a shortcut for `{ it -> it }`, e.g. :

    [1, 2, 3].collect{ it ^ 2 } == [1, 4, 9]

 - `it.text` (where `it` is a file) uses a method groovy adds to `File` to retrieve the entire text of the file.  This is amazingly helpful in scripts.


## Hello world Shebang (linux)
Given a hello.groovy file with content:

    #!/usr/bin/env groovy
    println "Hello world"

Can be executed from the command line if given execution permission as

    $ ./hello.groovy

## Using inject() On List To Create CSV String
In Groovy, the inject() method is one of the cumulative methods that allows us to add (or inject) new functionality into any object that implements the inject() method. In the case of a Collection, we can apply a closure to a collection of objects uniformly and then collate the results into a single value. The first parameter to the inject() method is the initial value of the cumulation and the second parameter is the closure.

In this example, we will take a List of Strings as a parameter and output the values of those strings delimited by commas. I have used this functionality to append a list of values to a REST query string and, if you modify it a bit, I've used it to include values into a SQL statement as part of a IN clause. Here is the code to do this: 

    public String convertToCSV( List<String> list ) {
        if (list == null) {
            return ""
        }
        return list.inject( '' ) { result, item ->
            result + ( result && item ? ',' : '' ) + ( item ? "${item.trim()}" : '' )
        }
    }

    assert convertToCSV( null ) == ""
    assert convertToCSV( ["aaa", "bbb  ", null, "  ccc  "] ) == "aaa,bbb,ccc" 

In this example, the first parameter to the inject() method is a zero length string, which means that when processing the first element of the list, result is also a zero length string. This resolves to false in the first ternary evaluation which is why we don't get a comma at the beginning of the string. With each consecutive iteration through the elements of the list, result becomes the concatenation of itself, a comma and then the next item until we reach the last item in the list.  

The advantage of this approach is that you don't need a variable outside of a looping construct to hold the concatenated String result. The implication being that this can lead to side effects in your code. With the inject() approach, this behavior is injected and the collection collates the result of the calls to the closure for you. The downside of this approach can be readability. But with some experience, it becomes easier to read and understand, and I hope this example helps you obtain that goal. 

## Hello World
The Groovy version of Hello World.

    println 'Hello World!'



