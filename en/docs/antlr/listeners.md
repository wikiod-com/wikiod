---
title: "Listeners"
slug: "listeners"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Listener Events Using Labels
Labeling the alternatives inside a rule starting with the `#` operator tells ANTLR to generate listener methods for each label corresponding to the alternative.  

By specifying a label for each alternative in the following rule:

```
// Rule
type : int     #typeInt
     | short   #typeShort
     | long    #typeLong
     | string  #typeString
     ;

// Tokens
int : 'int' ;
short : 'short' ;
long : 'long' ;
string : 'string' ;
```

Will generate the following methods in the generated interface that extends [`ParseTreeListener`][1]:

```
public void enterTypeInt(TypeShortContext ctx);
public void enterTypeShort(TypeIntContext ctx);
public void enterTypeLong(TypeLongContext ctx);
public void enterTypeString(TypeStringContext ctx);
```

  [1]: http://www.antlr.org/api/Java/org/antlr/v4/runtime/tree/ParseTreeListener.html

