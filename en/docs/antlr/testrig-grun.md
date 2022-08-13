---
title: "TestRig  grun"
slug: "testrig--grun"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## Setup TestRig
ANTLR contains a testing tool in its runtime library, this tool can be used to display information detailing how the parsing is performed to match input against defined rules in your grammar file.

To use this tool contained within the ANTLR jar file you should setup your systems classpath to allow access to both the ANTLR tool and the runtime library :

    export CLASSPATH=".:/usr/local/lib/antlr-4.5.3-complete.jar:$CLASSPATH"

> Note: Ensure the Dot precedes any path to ensure the java virtual
> machine wont see classes in your current working directory.

Alises can be used on Linux/MAC/Unix to simplify commands used:

    alias antlr4='java -jar /usr/local/lib/antlr-4.5.3-complete.jar' 
    //or any directory where your jar is located

Note setup on windows for aliases and classpath setup may be more complicated, see [here][1] for more comprehensive details.

Accessing TestRig
-----------------

Once you have setup your alias you can setup TestRig in the following way, again using an alias is recommended as reduces the amount of time required to perform the action:

    alias grun='java org.antlr.v4.runtime.misc.TestRig'

If you do not wish to setup an alias on windows you can access TestRig by running the following command in the same location as your ANTLR jar directory:

    java -cp .;antlr.4.5.3-complete.jar org.antlr.v4.runtime.misc.TestRig
    //or
    java -cp .;antlr.4.5.3-complete.jar org.antlr.v4.gui.TestRig

To run TestRig on your grammar you can pass the parameters in for your grammar like this :

    grun yourGrammar yourRule -tree //using the setup alias
    java -cp .;antlr.4.5.3-complete.jar org.antlr.v4.gui.TestRig yourGrammar YourRule -tree //on windows with no alias
    java -cp .;antlr.4.5.3-complete.jar org.antlr.v4.gui.TestRig yourGrammar Hello r -tree
    //Windows with the grammar Hello.g4 starting from the rule 'r'.

  [1]: https://levlaz.org/setting-up-antlr4-on-windows/


## Build Grammar with Visual Parse Tree
Specifying the `-gui` command line option when running an ANTLR grammar in the test rig will result in a window popping up with a visual representation of the parse tree. For example:

Given the following grammar: 

**JSON.g4**

```
/** Taken from "The Definitive ANTLR 4 Reference" by Terence Parr */

// Derived from http://json.org
grammar JSON;

json
   : value
   ;

object
   : '{' pair (',' pair)* '}'
   | '{' '}'
   ;

pair
   : STRING ':' value
   ;

array
   : '[' value (',' value)* ']'
   | '[' ']'
   ;

value
   : STRING
   | NUMBER
   | object
   | array
   | 'true'
   | 'false'
   | 'null'
   ;


STRING
   : '"' (ESC | ~ ["\\])* '"'
   ;
fragment ESC
   : '\\' (["\\/bfnrt] | UNICODE)
   ;
fragment UNICODE
   : 'u' HEX HEX HEX HEX
   ;
fragment HEX
   : [0-9a-fA-F]
   ;
NUMBER
   : '-'? INT '.' [0-9] + EXP? | '-'? INT EXP | '-'? INT
   ;
fragment INT
   : '0' | [1-9] [0-9]*
   ;
// no leading zeros
fragment EXP
   : [Ee] [+\-]? INT
   ;
// \- since - means "range" inside [...]
WS
   : [ \t\n\r] + -> skip
   ;
```

Given the following JSON file:

**example.json**

```
{
  "name": "John Doe",
  "age": 25,
  "address": {
    "streetAddress": "21 2nd Street",
    "city": "New York",
    "state": "NY",
    "postalCode": "10021-3100"
  },
  "phoneNumbers": [
    {
      "type": "home",
      "number": "212 555-1234"
    },
    {
      "type": "mobile",
      "number": "123 456-7890"
    }
  ],
  "children": [],
  "spouse": null
}
```

The following syntax command line syntax:

```
export CLASSPATH=".:/usr/local/lib/antlr-4.0-complete.jar:$CLASSPATH"                                                                                      
alias antlr4='java -jar /usr/local/lib/antlr-4.0-complete.jar'                                                                                             
alias grun='java org.antlr.v4.runtime.misc.TestRig'

antlr4 -o . -lib . -no-listener -no-visitor  JSON.g4; javac *.java; grun JSON json -gui example.json

```

will result in the generated **.java** & **.tokens** files, as well as the compiled **.class** files:

```
JSON.g4                        JSONLexer.class                JSONListener.java JSONParser$PairContext.class   JSON.tokens                    JSONLexer.java JSONParser$ArrayContext.class  JSONParser$ValueContext.class  JSONBaseListener.class JSONLexer.tokens               JSONParser$JsonContext.class   JSONParser.class
JSONBaseListener.java          JSONListener.class
JSONParser$ObjectContext.class JSONParser.java
```
and the following parse tree:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/oNjUt.png

