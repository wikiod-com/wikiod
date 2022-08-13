---
title: "Getting started with v8"
slug: "getting-started-with-v8"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Useful built-in functions and objects in d8
In addition to libraries defined in EcmaScript language specification and EcmaScript internationalization API specification, d8 also implements the following functions and objects.

 - `print(args...): function`. Print to `stdout`.
 - `printErr(args...): function`. Print to `stderr`.
 - `write(args...): function`. Same as `print` but no newline at the end.
 - `read(filename): function`. Read text from file and returned as `String`.
 - `readbuffer(filename): function`. Read binary from file and returned as `ArrayBuffer`.
 - `readline(): function`. Read line from `stdin`. Use `'\'` for multi-lines input.
 - `load(filename): function`. Load and execute JavaScript file.
 - `quit([exitCode]): function`. Quit with optional exit code.
 - `version(): function`. Return version code as `String`.
 - `os: object`. OS-related utilities, only available for POSIX.
   - `os.system(command): function`: Execute system command.
   - `os.chdir(path): function`: Change current directory.
   - `os.setenv(name, value): function`: Set environment variable.
   - `os.unsetenv(name): function`: Unset environment variable.
   - `os.umask(alue) function`: Calls the umask system call and returns the old umask.
   - `os.mkdirp(path[, mask]): function`: Creates a directory. The mask (if present) is anded with the current umask.  Intermediate directories are created if necessary.
   - `os.rmdir(path): function`: Remove directory.
 - `performance: object`: Use for performance analysis.
   - `performance.now(): function`. Returns a time stamp as double, measured in milliseconds.
 - `Worker: object`: Modified from [HTML5 Web Worker][1] (see example below for detail)
 - `Realm: object`: Create and manage isolated environment (realm).
   - `Realm.create(): function`: Create a new realm with distinct security token and return its index.
   - `Realm.createAllowCrossRealmAccess(): function`: Create a new realm with same security token as the current realm and return its index.
   - `Realm.current(): function`: Returns the index of the currently active realm. Index of global realm is 0.
   - `Realm.global(i): function`: Returns the global object of realm i.
   - `Realm.owner(globalObj): function`: Returns the index of the realm that created globalObj.
   - `Realm.eval(i, s): function`: Evaluates s in realm i and returns the result.
   - `Realm.switch(i): function `: Switches to the realm i for consecutive interactive inputs.
   - `Realm.dispose(i): function`: Disposes the reference to the realm i.
   - `Realm.shared: object`: An accessor for a single shared value across realms.

Detailed implementations and comments can be found in [d8.h][2], [d8.cc][3], [d8-posix.cc][4] and [d8-windows.cc][5].
 
 Example:

    print("Hello World!");
    write("Hello ");
    write("again!\n");
    printErr("Nothing went wrong.");
    
    write("Your name: ");
    var name = readline();
    print("Hello, ", name, "!");
    
    load("external.js");
    
    var string = read("text.txt");
    var buffer = readbuffer("binary.bin");
    
    print("Version: ", version());
    
    quit(0); // bye

Worker example:

main.js

    var workerScript = read("worker.js");
    var worker = new Worker(workerScript);
    worker.postMessage(12);

worker.js

    onmessage = ev => {
      print(ev); // 12
    };

Realm example:

    print(Realm.current()); // 0

    var rIndex = Realm.create();
    print(rIndex); // 1
    
    Realm.eval(rIndex, "var x = 100");
    Realm.eval(rIndex, "print(x)"); // 100
    var result = Realm.eval(rIndex, "x * 2");
    print(result); // 200
    
    Realm.eval(rIndex, "var rIndex2 = Realm.create()");
    Realm.eval(rIndex, "print(rIndex2)"); // 2
    Realm.eval(rIndex, "print(Realm.owner(this))"); // 1
    
    try {
      var childGlobal = Realm.global(rIndex);
      print(childGlobal); // error
    } catch (e) {
      print("Global object cannot be read/written cross-realm.");
    }
    
    var rIndex3 = Realm.createAllowCrossRealmAccess();
    Realm.eval(rIndex, "var x = 50");
    var childGlobal = Realm.global(rIndex3);
    childGlobal.x++;
    Realm.eval(rIndex3, "print(x)"); // 51
    
    try {
      Realm.dispose(rIndex3);
      Realm.eval(rIndex3, "print(x)"); // error
    } catch (e) {
      print("The realm is dereferenced");
    }
    
    Realm.shared = "Hello from another world";
    Realm.eval(rIndex, "print(Realm.shared)");


  [1]: https://developer.mozilla.org/en-US/docs/Web/API/Worker
  [2]: https://github.com/v8/v8/blob/master/src/d8.h
  [3]: https://github.com/v8/v8/blob/master/src/d8.cc
  [4]: https://github.com/v8/v8/blob/master/src/d8-posix.cc
  [5]: https://github.com/v8/v8/blob/master/src/d8-windows.cc

## Running v8 on a file
Use the d8 shell to run the v8 engine. Use the following pattern to run on a file:

    /path/to/d8 [flags] [file].js

For example:

    ./d8 --log-gc script.js

will run d8 on script.js with garbage collection logging enabled

