---
title: "Process"
slug: "process"
draft: false
images: []
weight: 9935
type: docs
toc: true
---

Notice that the API recommends that, as of version 1.5, the preferred way to create a Process is using `ProcessBuilder.start()`.

Another important remark is that the exit value produced by `waitFor` is dependent from the program/script being executed. For instance, the exit codes produced by **calc.exe** are different from **notepad.exe**.

## Pitfall: Runtime.exec, Process and ProcessBuilder don't understand shell syntax
The `Runtime.exec(String ...)` and `Runtime.exec(String)` methods allow you to execute a command as an external process<sup>1</sup>.  In the first version, you supply the command name and the command arguments as separate elements of the string array, and the Java runtime requests the OS runtime system to start the external command.  The second version is deceptively easy to use, but it has some pitfalls.

First of all, here is an example of using `exec(String)` being used safely:

    Process p = Runtime.exec("mkdir /tmp/testDir");
    p.waitFor();
    if (p.exitValue() == 0) {
        System.out.println("created the directory");
    }

Spaces in pathnames
-------------------

Suppose that we generalize the example above so that we can create an arbitrary directory:

    Process p = Runtime.exec("mkdir " + dirPath);
    // ...

This will typically work, but it will fail if `dirPath` is (for example) "/home/user/My Documents".  The problem is that `exec(String)` splits the string into a command and arguments by simply looking for whitespace.  The command string:

    "mkdir /home/user/My Documents"

will be split into:

    "mkdir", "/home/user/My", "Documents"

and this will cause the "mkdir" command to fail because it expects one argument, not two.

Faced with this, some programmers try to add quotes around the pathname.  This doesn't work either:

    "mkdir \"/home/user/My Documents\""

will be split into:

    "mkdir", "\"/home/user/My", "Documents\""

The extra double-quote characters that were added in attempt to "quote" the spaces are treated like any other non-whitespace characters.  Indeed, anything we do quote or escape the spaces is going to fail.

The way to deal with this particular problems is to use the `exec(String ...)` overload.

    Process p = Runtime.exec("mkdir", dirPath);
    // ...

This will work if `dirpath` includes whitespace characters because this overload of `exec` does not attempt to split the arguments.  The strings are passed through to the OS `exec` system call as-is.

Redirection, pipelines and other shell syntax
---------------------------------------------

Suppose that we want to redirect an external command's input or output, or run a pipeline.  For example:

    Process p = Runtime.exec("find / -name *.java -print 2>/dev/null");

or 

    Process p = Runtime.exec("find source -name *.java | xargs grep package");

(The first example lists the names of all Java files in the file system, and the second one prints the `package` statements<sup>2</sup> in the Java files in the "source" tree.)

These are not going to work as expected.  In the first case, the "find" command will be run with "2>/dev/null" as a command argument.  It will not be interpreted as a redirection.  In the second example, the pipe character ("|") and the works following it will be given to the "find" command.

The problem here is that the `exec` methods and `ProcessBuilder` do not understand any shell syntax.  This includes redirections, pipelines, variable expansion, globbing, and so on.

In a few cases (for example, simple redirection) you can easily achieve the desired effect using `ProcessBuilder`.  However, this is not true in general.  An alternative approach is to run the command line in a shell; for example:

    Process p = Runtime.exec("bash", "-c", 
                             "find / -name *.java -print 2>/dev/null");

or

    Process p = Runtime.exec("bash", "-c", 
                             "find source -name \\*.java | xargs grep package");

But note that in the second example, we needed to escape the wildcard character ("*") because we want the wildcard to be interpreted by "find" rather than the shell.

Shell builtin commands don't work
---------------------------------

Suppose the following examples won't work on a system with a UNIX-like shell:

    Process p = Runtime.exec("cd", "/tmp");     // Change java app's home directory

or 

    Process p = Runtime.exec("export", "NAME=value");  // Export NAME to the java app's environment

There are a couple of reasons why this won't work:

  1. On "cd" and "export" commands are shell builtin commands.  They don't exist as distinct executables.

  2. For shell builtins to do what they are supposed to do (e.g. change the working directory, update the environment), they need to change the place where that state resides.   For a normal application (including a Java application) the state is associated with the application process.  So for example, the child process that would run the "cd" command could not change the working directory of its parent "java" process.  Similarly, one `exec`'d process cannot change the working directory for a process that follows it.

This reasoning applies to all shell builtin commands.
    
----------

<sup>1 - You can use `ProcessBuilder` as well, but that is not relevant to the point of this example.</sup>

<sup>2 - This is a bit rough and ready ... but once again, the failings of this approach are not relevant to the example.</sup>

## Simple example (Java version < 1.5)
This example will call the windows calculator. It's important to notice that the exit code will vary accordingly to the program/script that is being called.


    package process.example;
    
    import java.io.IOException;
    
    public class App {
    
        public static void main(String[] args) {
            try {
                // Executes windows calculator
                Process p = Runtime.getRuntime().exec("calc.exe");

                // Wait for process until it terminates
                int exitCode = p.waitFor();

                System.out.println(exitCode);
            } catch (IOException e) {
                e.printStackTrace();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

## ch.vorburger.exec


## Using the ProcessBuilder class


## Blocking vs. Non-Blocking Calls


