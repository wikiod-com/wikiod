---
title : ANTLR Tutorial
slug : antlr-tutorial
weight : 9971
draft : false
images : []
type : docs
---

ANTLR (ANother Tool for Language Recognition) is a powerful parser generator for reading, processing, executing, or translating structured text or binary files. It's widely used to build languages, tools, and frameworks. From a grammar, ANTLR generates a parser that can build and walk parse trees.

 - [Official antlr website][1] (always points to the latest version)

**Antlr Versions**  
Antlr is separated in two big parts, the grammar (grammar files) and the generated code files, which derive from the grammar based on target language. The antlr versions are in the format of V1.V2.V3 :

 - V1: Change in V1 means that new syntax of features were introduced in grammar files
 - V2: Change in V2 means that new features or major fixes were introduced in the generated files (e.g addition of new functions)
 - V3: stands for bug fixes or minor improvements

**Runtime Libraries and Code Generation Targets**  
The Antlr tool is written in Java, however it is able to generate parsers and lexers in various languages. To run the parser and lexer you will also need having the runtime library of antlr alongside with the parser and lexer code. The supported target language (and runtime libraries) are the following:

 - Java

 - C# 

 - Python (2 and 3) 

 - JavaScript

  [1]: http://www.antlr.org/

