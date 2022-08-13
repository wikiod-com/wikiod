---
title: "Lexer rules in v4"
slug: "lexer-rules-in-v4"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Priority rules
Several lexer rules can match the same input text. In that case, the token type will be chosen as follows:

 - First, select the lexer rule which matches the *longest* input
 - If the text matches an implicitly defined token (like `'{'`), use the implicit rule
 - If several lexer rules match the same input length, choose the *first* one, based on definition order
 
---
 
The following combined grammar:
 
    grammar LexerPriorityRulesExample;
    
    // Parser rules
 
    randomParserRule: 'foo'; // Implicitly declared token type
        
    // Lexer rules
        
    BAR: 'bar';
    IDENTIFIER: [A-Za-z]+;
    BAZ: 'baz';
    
    WS: [ \t\r\n]+ -> skip;
    
Given the following input:

    aaa foo bar baz barz
    
Will produce the following token sequence from the lexer:

    IDENTIFIER 'foo' BAR IDENTIFIER IDENTIFIER

 - `aaa` is of type `IDENTIFIER`
 
   Only the `IDENTIFIER` rule can match this token, there is no ambiguity.
   
   
 - `foo` is of type `'foo'`
 
   The parser rule `randomParserRule` introduces the implicit `'foo'` token type, which is prioritary over the `IDENTIFIER` rule.
   
 
 - `bar` is of type `BAR`
 
   This text matches the `BAR` rule, which is defined *before* the `IDENTIFIER` rule, and therefore has precedence.
   
 
 - `baz` is of type `IDENTIFIER`
 
   This text matches the `BAZ` rule, but it also matches the `IDENTIFIER` rule. The latter is chosen as it is defined *before* `BAR`.
   
   Given the grammar, `BAZ` will *never* be able to match, as the `IDENTIFIER` rule already covers everything `BAZ` can match.
   
 
 - `barz` is of type `IDENTIFIER`
    
   The `BAR` rule can match the first 3 characters of this string (`bar`), but the `IDENTIFIER` rule will match 4 characters. As `IDENTIFIER` matches a longer substring, it is chosen over `BAR`.
   
As a rule of thumb, specific rules should de defined *before* more generic rules. If a rule can only match an input which is already covered by a previously defined rule, it will *never* be used.

Implicitly defined rules such as `'foo'` act as if they were defined *before* all other lexer rules.


## Implicit lexer rules
When tokens like `'{'` are used in a *parser* rule, an implicit lexer rule will be created for them unless an explicit rule exists.

In other words, if you have a lexer rule:

    OPEN_BRACE: '{';
    
Then both of these parser rules are equivalent:

    parserRule: '{';
    parserRule: OPEN_BRACE;
    
But if the `OPEN_BRACE` lexer rule is *not* defined, an implicit anonymous rule will be created. In that case, the implicit rule will be defined *as if* it were defined *before* the other rules: it will have a higher priority than other rules.


## Actions and semantic predicates
A lexer action is a block of arbitrary code in the target language surrounded by `{`...`}`, which is executed during matching:

    IDENTIFIER: [A-Z]+ { log("matched rule"); };
    
A semantic predicate is a block of arbitrary code in the target language surrounded by `{`...`}?`, which evaluates to a boolean value. If the returned value is false, the lexer rule is skipped.
    
    IDENTIFIER: [A-Z]+ { identifierIsValid() }?;

Semantic predicates should be defined at the end of the rule whenever possible for performance reasons.


## Simple rules
Lexer rules define token types. Their name has to start with an uppercase letter to distinguish them from parser rules.

    INTEGER: [0-9]+;
    IDENTIFIER: [a-zA-Z_] [a-zA-Z_0-9]*;
    
    OPEN_PAREN: '(';
    CLOSE_PAREN: ')';

Basic syntax:

Syntax | Meaning
------ | -------
`A` | Match lexer rule or fragment named `A`
`A B` | Match `A` followed by `B`
<code>(A\|B)</code> | Match either `A` or `B`
`'text'` | Match literal *"text"*
`A?` | Match `A` zero or one time
`A*` | Match `A` zero or more times
`A+` | Match `A` one or more times
`[A-Z0-9]` |  Match one character in the defined ranges (in this example between A-Z or 0-9)
`'a'..'z'` | Alternative syntax for a character range
`~[A-Z]` | Negation of a range - match any single character *not* in the range
`.` | Match any single character



## Fragments
Fragments are reusable parts of lexer rules which cannot match on their own - they need to be referenced from a lexer rule.

    INTEGER: DIGIT+
           | '0' [Xx] HEX_DIGIT+
           ;
    
    fragment DIGIT: [0-9];
    fragment HEX_DIGIT: [0-9A-Fa-f];

## Lexer commands
A lexer rule can have associated *commands*:

    WHITESPACE: [ \r\n] -> skip;
    
Commands are defined after a `->` at the end of the rule.

 - `skip`: Skips the matched text, no token will be emited
 - `channel(n)`: Emits the token on a different channel
 - `type(n)`: Changes the emitted token type
 - `mode(n)`, `pushMode(n)`, `popMode`, `more`: Controls lexer modes
 

