---
title: "Getting started with Swift Language"
slug: "getting-started-with-swift-language"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Your first Swift program
Write your code in a file named `hello.swift`:

    print("Hello, world!")

- To compile and run a script in one step, use `swift` from the terminal (in a directory where this file is located):

> To launch a terminal, press <kbd>CTRL</kbd>+<kbd>ALT</kbd>+<kbd>T</kbd> on *Linux*, or find it in Launchpad on *macOS*. To change directory, enter `cd`**`directory_name`** (or `cd ..` to go back)

<pre>$ <b>swift hello.swift</b>
Hello, world!</pre>

> A **compiler** is a computer program (or a set of programs) that transforms source code written in a programming language (the source language) into another computer language (the target language), with the latter often having a binary form known as object code. ([Wikipedia](https://en.wikipedia.org/wiki/Compiler))

- To compile and run separately, use `swiftc`:

<pre>$ <b>swiftc hello.swift</b></pre>

This will compile your code into `hello` file. To run it, enter `./`, followed by a filename.

<pre>$ <b>./hello</b>
Hello, world!</pre>

- Or use the swift REPL (Read-Eval-Print-Loop), by typing `swift` from the command line, then entering your code in the interpreter:

**Code:**

<pre>
func greet(name: String, surname: String) {
    print("Greetings \(name) \(surname)")
}

let myName = "Homer"
let mySurname = "Simpson"

greet(name: myName, surname: mySurname)
</pre>

> **Let's break this large code into pieces:**
>
> - `func greet(name: String, surname: String) { // function body }` - create a *function* that takes a `name` and a `surname`.
>
>  - `print("Greetings \(name) \(surname)")` - This prints out to the console "Greetings ", then `name`, then `surname`. Basically `\(`**`variable_name`**`)` prints out that variable's value.
>
> - `let myName = "Homer"` and `let mySurname = "Simpson"` - create *constants* (variables which value you can't change) using `let` with names: `myName`, `mySurname` and values: `"Homer"`, `"Simpson"` respectively.
>
> - `greet(name: myName, surname: mySurname)` - calls a *function* that we created earlier supplying the values of *constants* `myName`, `mySurname`.

**Running it using REPL:**

<pre>$  <b>swift</b>
Welcome to Apple Swift. Type :help for assistance.
  1> <b>func greet(name: String, surname: String) { </b>
  2.<b>     print("Greetings \(name) \(surname)") </b>
  3. <b>} </b>
  4>  
  5> <b>let myName = "Homer"</b>
myName: String = "Homer"
  6> <b>let mySurname = "Simpson"</b>
mySurname: String = "Simpson"
  7> <b>greet(name: myName, surname: mySurname)</b>
Greetings Homer Simpson
  8> ^D</pre>
<sup><sup>Press <kbd>CTRL</kbd>+<kbd>D</kbd> to quit from REPL.</sup></sup>

## Your first program in Swift on a Mac (using a Playground)
From your Mac, download and install Xcode from the Mac App Store following [this link][1].

After the installation is complete, open Xcode and select **Get started with a Playground**:

[![enter image description here][2]][2]

On the next panel, you can give your Playground a name or you can leave it `MyPlayground` and press **Next**:

[![enter image description here][3]][3]

Select a location where to save the Playground and press **Create**:

[![enter image description here][4]][4]

The Playground will open and your screen should look something like this:

[![enter image description here][5]][5]

Now that the Playground is on the screen, press <kbd>⇧</kbd> + <kbd>cmd</kbd> + <kbd>Y</kbd> to show the **Debug Area**.

Finally delete the text inside Playground and type:

    print("Hello world")

You should see 'Hello world' in the **Debug Area** and "Hello world\n" in the right **Sidebar**:

[![enter image description here][6]][6]

Congratulations! You've created your first program in Swift!


  [1]: https://itunes.apple.com/it/app/xcode/id497799835?mt=12
  [2]: http://i.stack.imgur.com/Ox1wg.png
  [3]: http://i.stack.imgur.com/sO7GW.png
  [4]: http://i.stack.imgur.com/DnLtL.png
  [5]: http://i.stack.imgur.com/BlAVs.png
  [6]: http://i.stack.imgur.com/VMeXE.png

## Installing Swift
First, [download][1] the compiler and components.

Next, add Swift to your path. On macOS, the default location for the downloadable toolchain is /Library/Developer/Toolchains. Run the following command in Terminal:

    export PATH=/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin:"${PATH}"

On Linux, you will need to install clang:

    $ sudo apt-get install clang

If you installed the Swift toolchain to a directory other than the system root, you will need to run the following command, using the actual path of your Swift installation:

    $ export PATH=/path/to/Swift/usr/bin:"${PATH}"

You can verify you have the current version of Swift by running this command:

    $ swift --version


  [1]: https://swift.org/download/

## Your first program in Swift Playgrounds app on iPad
Swift Playgrounds app is a great way to get started coding Swift on the go. To use it:

1- Download [Swift Playgrounds][1] for iPad from App Store.

[![enter image description here][2]][2]

2- Open the app.

3- In the **My Playgrounds** tab, tap **+** on the top left corner and then select Blank template.

4- Enter your code.

5- Tap Run My Code to run your code.

6- At the front of each line, the result will be stored in a small square. Tap it to reveal the result.

7- To step slowly through code to trace it, tap the button next to Run My Code.


  [1]: https://itunes.apple.com/us/app/swift-playgrounds/id908519492?mt=8
  [2]: https://i.stack.imgur.com/Ig7wZ.png

## Optional Value and Optional enum
 Optionals type, which handles the absence of a value. Optionals say either "there is a value, and it equals x" or "there isn't a value at all".

An Optional is a type on its own, actually one of Swift’s new super-powered enums. It has two possible values, `None` and `Some(T)`, where T is an associated value of the correct data type available in Swift.

Let's have a look at this piece of code for example:

    let x: String? = "Hello World"

    if let y = x {
       print(y)
    }


In fact if you add a **`print(x.dynamicType)`** statement in the code above you'll see this in the console:

    Optional<String>

String? is actually syntactic sugar for Optional<String>, and Optional is a type in its own right.

Here's a simplified version of the header of Optional, which you can see by command-clicking on the word Optional in your code from Xcode:

    enum Optional<Wrapped> {

     /// The absence of a value.
     case none

     /// The presence of a value, stored as `Wrapped`.
     case some(Wrapped)
    }

Optional is actually an enum, defined in relation to a generic type Wrapped. It has two cases: *`.none`* to represent the absence of a value, and *`.some`* to represent the presence of a value, which is stored as its associated value of type Wrapped.

Let me go through it again: `String?` is not a `String` but an `Optional<String>`.The fact that `Optional` is a type means that it has its own methods, for example `map` and `flatMap`.

