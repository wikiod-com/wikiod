---
title: "Introduction to ANTLR v4"
slug: "introduction-to-antlr-v4"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

ANTLR v4 is a powerful tool used for building new programming languages and processing/translating structured text or binary files. ANTLR uses a grammar you create to generate a parser which can build and traverse a parse tree (or abstract syntax tree, AST). The parser consists of output files in a target language that you specify. ANTLR v4 supports several targets including: Java, C#, JavaScript, Python2, and Python3. Support for C++ is being worked on. For working in GUI IDEs, there are plug-ins for Visual Studio, Intellij, NetBeans, and Eclipse.  

For general information, visit the [ANTLR website][1]. To get serious about ANTLR, check out the highly recommended book written by Terrence Parr (the guy who created ANTLR) [The Definitive ANTLR 4 Reference][2]. 


----------
**Significant Version Info**

 - 4.5: 01/22/15 - Added JavaScript target and upgraded C# target. [4.5 Release Notes][3]
 - 4.4: 07/16/14 - Added Python2 and Python3 as targets. [4.4 Release Notes][4]
 - 4.3: 06/18/14 - Major bug fixes; prepared for adding new targets. [4.3 Release Notes][5]
 - 4.2: 02/04/14 - Improved syntax for selecting/matching parse trees. [4.2 Release Notes][6]
 - 4.1: 06/30/13 - Improved parsing performance; export ASTs to PNG. [4.1 Release Notes][7]
 - 4.0: 01/21/13 - Initial release.

  [1]: http://www.antlr.org/
  [2]: https://pragprog.com/book/tpantlr2/the-definitive-antlr-4-reference
  [3]: https://github.com/antlr/antlr4/releases/tag/4.5
  [4]: https://github.com/antlr/antlr4/releases/tag/4.4
  [5]: https://github.com/antlr/antlr4/releases/tag/4.3
  [6]: https://github.com/antlr/antlr4/releases/tag/4.2
  [7]: https://github.com/antlr/antlr4/releases/tag/4.1

## Installing ANTLR in Visual Studio 2015 (using Nuget)
 1. Open Visual Studio 2015, navigate to Tools → Extensions → Online and search for Antlr. Download the extension ANTLR Language Support (Created by Sam Harwell) and restart Visual Studio.
 2. Create new Console Application Project. Right click on the Solution → Manage Nuget Packages for Solution → Browse (Tab) and search for Antlr4 and install it. 
[![Antlr4 Nuget][1]][1]
 3. Add a New Item to your Project by right clicking on it. And look for ANTLR4 Templates. 
[![Antlr4 templates][2]][2]
 4. From your ANTLR file (ending .g4) go to File → Advance Save Options and search for Unicode (UTF-8 **without signature**) - Codepage 65001  and click OK. Thats it. 
[![Antlr Advanced Save Options][3]][3]


<h2>Test if everything works</h2>

 - Create a ANTLR 4 Combined Grammar item and name it Calculator.g4
 - Copy and Paste the Calculator source code from this Github project here: [Calculator by Tom Everett][4]
 - Change grammar calculator to grammar Calculator
 - On Solution Explorer → Click on Show All Files.  
[![Solution Explorer Show All Files][5]][5]
 - Save and Run (Start) the project
 - In Solution Explorer under obj folder you should see cs classes generated like the Visitor and Listener. If this is the case you succeeded. Now you can start working with ANTLR in Visual Studio 2015.
[![ANTLR generated classes][6]][6]


  [1]: https://i.stack.imgur.com/nVweD.png
  [2]: https://i.stack.imgur.com/fNaF9.png
  [3]: https://i.stack.imgur.com/ed3Tc.png
  [4]: https://github.com/antlr/grammars-v4/blob/master/calculator/calculator.g4
  [5]: https://i.stack.imgur.com/pvPV9.png
  [6]: https://i.stack.imgur.com/BhkWn.png

## Installing for Command Line Use
ANTLR is distributed as a Java Jar file It can be downloaded [here][1]. As ANTLR is compiled as a jar file it subsequently requires the Java runtime environment to operate, if you do not have It can be downloaded [here][2].

Once the ANTLR JAR file has been downloaded you can run ANTLR from the command line in the same way as any other JAR file:


    Java -jar antlr-4.5.3-complete.jar

(Assuming you are operating in the same directory as the antlr-4.5.3-complete.jar file).

This should output something similar to this :

    ANTLR Parser Generator  Version 4.5.3
     -o ___              specify output directory where all output is generated
     -lib ___            specify location of grammars, tokens files
     -atn                generate rule augmented transition network diagrams
     -encoding ___       specify grammar file encoding; e.g., euc-jp
     -message-format ___ specify output style for messages in antlr, gnu, vs2005
     -long-messages      show exception details when available for errors and warnings
     -listener           generate parse tree listener (default)
     -no-listener        don't generate parse tree listener
     -visitor            generate parse tree visitor
     -no-visitor         don't generate parse tree visitor (default)
     -package ___        specify a package/namespace for the generated code
     -depend             generate file dependencies
     -D<option>=value    set/override a grammar-level option
     -Werror             treat warnings as errors
     -XdbgST             launch StringTemplate visualizer on generated code
     -XdbgSTWait         wait for STViz to close before continuing
     -Xforce-atn         use the ATN simulator for all predictions
     -Xlog               dump lots of logging info to antlr-timestamp.log

other recommended actions for setup include:

         1. Add antlr4-complete.jar to CLASSPATH, either: Permanently: 
    Using System Properties dialog > Environment variables > Create or append to CLASSPATH variable Temporarily, at command line: SET CLASSPATH=.;C:\Javalib\antlr4-complete.jar;%CLASSPATH% 
         3.Create batch commands for ANTLR Tool, TestRig in dir in PATH
             antlr4.bat: java org.antlr.v4.Tool %*
             grun.bat:   java org.antlr.v4.gui.TestRig %*

After setup you can build an application using your .g4 grammar file :

    Java -jar antlr-4.5.3-complete.jar yourGrammar.g4

You can also build an application in other languages with the -Dlanguage parameter. For example to generate C# files you would do something like this:

    java -jar antlr-4.5.3-complete.jar yourGrammar.g4 -Dlanguage=CSharp

See [here][3] for full list of pre-made grammar's for common programming languages.


  [1]: http://www.antlr.org/download.html
  [2]: http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html
  [3]: https://github.com/antlr/grammars-v4

## Installing Using Build Automation tools
Download the [latest version of ANTLR](http://www.antlr.org/download.html) and extract it to a folder.

You can use also Maven, Gradle, or other build tool to depend on its runtime (the classes the generated grammars use): `org.antlr:antlr4-runtime`.

In order to automatically -as part of the build process- generate the parser in a maven project, use the [Maven plugin](http://www.antlr.org/api/maven-plugin/latest/index.html): `org.antlr:antlr4`.

## Install in Eclipse and Build Hello World
(Tested with ANTLR 4.5.3, Eclipse Neon, ANTLR 4 IDE 0.3.5, and Java 1.8)

 1. Download [the latest ANTLR][1]. Make sure to get the complete ANTLR Java binaries jar. Save to any appropriate location, for example the folder where other Java libraries are stored. It doesn’t matter where, just remember the location.


 2. Install the ANTLR IDE in Eclipse. 
    - From the Eclipse menu, click Help and select Eclipse Marketplace.
    - In the Find: box, type antlr and click Go.
    - Click Install for ANTLR 4 IDE.
    - Click Finish in the Confirm Selected Features window.
    - If a Security Warning window pops up, click OK.
    - Restart Eclipse.

 3. Work around for the “Failed to create injector…” error. 
    - When accessing ANTLR 4 Preferences in Eclipse or when the environment variable HOME is not set, the following error occurs: Failed to create injector for com.github.jknack.antlr-4ide.Antlr4 for com.github.jknack.antlr-4ide.Antlr4. 
    - Make sure the environment variable HOME is set. If not, set it as appropriate for your system.
    - Download [Xtext 2.7.3][2] to the same location as antlr-n.n.n-complete.jar.
    - In Eclipse, click on Help and select Install New Software.
    - Click Add… to get to the Add Repository window.
    - Type a name, xtext 2.7.3 for example, then click on Archive…, navigate to the Xtext 2.7.3 file and select it, then click OK.
    - In the Install window, click the Select All button then click Next> twice, accept the license agreement. and click Finish.
    - Restart Eclipse.

 4. Tell Eclipse/Java where ANTLR is. 
    - In Eclipse, click on Window and select Preferences.
    - In the left pane, expand Java and Build Path, then select Classpath Variables.
    - In the right pane, click New…, enter a Name, and click File… and browse to your location of antlr-n.n.n-complete.jar. Click OK to get back to the Classpath Variables window.
    - Click OK to exit Preferences.

 5. (Optional) Configure the ANTLR IDE generated sources directory.
    - In the Eclipse main window, click Window then Preferences.
    - In the left pane, expand ANTLR 4 and select Tool.
    - Under Options, change the directory if desired. For example, java is my target language so I use ./antlr-java.
    - Click OK to close the Preferences window.

 6. Create an ANTLR 4 project.
    - From the Eclipse main window, go to File, New, Project. 
    - In the New Project window, expand General and select ANTLR 4 Project.
    - Click Next, type a project name and click Finish. 
    - The default new project contains a Hello.g4 file and will automatically build the standard "Hello World" program.
    - In the Package Explorer, expand the new project folder to see the g4 file and a folder named target (or the name you gave it in Step 5) containing the target source files.  

  
  [1]: http://www.antlr.org/download.html
  [2]: http://www.eclipse.org/modeling/download.php?file=/modeling/tmf/xtext/downloads/drops/2.7.3/R201411190455/tmf-xtext-Update-2.7.3.zip

