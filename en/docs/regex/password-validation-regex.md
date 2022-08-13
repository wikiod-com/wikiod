---
title: "Password validation regex"
slug: "password-validation-regex"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## A password containing at least 1 uppercase, 1 lowercase, 1 digit, 1 special character and have a length of at least of 10
As the characters/digits can be anywhere within the string, we require lookaheads. Lookaheads are of `zero width` meaning they do not consume any string. In simple words the position of checking resets to the original position after each condition of lookahead is met.

**Assumption** :- *Considering non-word characters as special*

    ^(?=.{10,}$)(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.*\W).*$

Before proceeding to explanation, let's take a look how the regex `^(?=.*[a-z])` works (*length is not considered here*) on string `1$d%aA`

[![enter image description here][1]][1]

*Image Credit* :- **https://regex101.com/**

**Things to notice**
 - Checking is started from beginning of the string due to anchor tag `^`.
 - The position of checking is being reset to the starting after condition of lookahead is met.

*Regex Breakdown*

    ^ #Starting of string
     (?=.{10,}$) #Check there is at least 10 characters in the string. 
                 #As this is lookahead the position of checking will reset to starting again
     (?=.*[a-z]) #Check if there is at least one lowercase in string.
                 #As this is lookahead the position of checking will reset to starting again
     (?=.*[A-Z]) #Check if there is at least one uppercase in string.
                 #As this is lookahead the position of checking will reset to starting again
     (?=.*[0-9]) #Check if there is at least one digit in string.
                 #As this is lookahead the position of checking will reset to starting again
     (?=.*\W) #Check if there is at least one special character in string.
              #As this is lookahead the position of checking will reset to starting again
    .*$ #Capture the entire string if all the condition of lookahead is met. This is not required if only validation is needed

We can also use the *non-greedy* version of the above regex

    ^(?=.{10,}$)(?=.*?[a-z])(?=.*?[A-Z])(?=.*?[0-9])(?=.*?\W).*$


  [1]: http://i.stack.imgur.com/ehu4F.png

## A password containing at least 2 uppercase, 1 lowercase, 2 digits and is of length of at least 10
This can be done with a bit of modification in the above regex

     ^(?=.{10,}$)(?=(?:.*?[A-Z]){2})(?=.*?[a-z])(?=(?:.*?[0-9]){2}).*$

or

     ^(?=.{10,}$)(?=(?:.*[A-Z]){2})(?=.*[a-z])(?=(?:.*[0-9]){2}).*

Let's see how a simple regex `^(?=(?:.*?[A-Z]){2})` works on string `abcAdefD`

   [![enter image description here][1]][1]

*Image Credit* :- **https://regex101.com/**

[1]: http://i.stack.imgur.com/DPQ6G.png

