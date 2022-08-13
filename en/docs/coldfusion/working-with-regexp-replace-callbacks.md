---
title: "Working with RegExp Replace callbacks"
slug: "working-with-regexp-replace-callbacks"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

If you want more than a simple string replacement with common regular expressions you certainly run into trouble and hit the wall when discovering the limits of the regex functions Coldfusion has. There is no build-in function like php's `preg_replace_callback`.

## Parameters
| Parameter  | Details                                          |  
| ---------- | ------------------------------------------------ |  
| `re`       | The regular expression                           |
| `str`      | The string which should be applyed the the regex |
| `callback` | The function where the captured grouped will be passed in if a match was found. There the matches can be processed                       |


Because Coldfusion itself does not offer what we want, we make recourse to the variety of Java, which is — as we all know — on top of Coldfusion. Java offers us `java.util.regex.Pattern`.

So here is what we actually do:

1. Invoke the `Compile` method from the `Pattern` Class object and passing the regex pattern to it (which probably deposits the regex pattern for later use).
2. Invoke the `Matcher` method on what the `Compile` method returned and passing the string where the pattern should be executed.
3. Test if matching was successfull by invoking the `find` method on what the `Matcher` method returned.

If `matcher.find()` returns `true`, we could say "That's it", but there is one little thing we have to consider: Java's Pattern object stores the groups and gives us access via another function, which is not always the best way for further processing and not that consistent regarding how other programming languages handle this case. Therefore we loop over `matcher.group()` so that we can pass an array containing the captured groups to the callback function. And now we can say: "That's it!"

## User defined REReplaceCallback function
    function REReplaceCallback(re,str,callback) {
        /*
            Thanks to Ben Nadel
            "Learning ColdFusion 8: REMatch() For Regular Expression Matching"
            from 2007-06-13
            https://www.bennadel.com/blog/769-learning-coldfusion-8-rematch-for-regular-expression-matching.htm
        */
        pattern = CreateObject("java","java.util.regex.Pattern").Compile(Arguments.re);
        matcher = pattern.Matcher(Arguments.str);
        if(matcher.find()) {
            groups = [];
            for(var i = 1; i lte matcher.groupCount(); i++) {
                ArrayAppend(groups,matcher.group(Val(i)));
            }
            return Arguments.callback(groups);
        }
        else {
            return Arguments.callback(false);
        }
    }

## Using REReplaceCallback function
    REReplaceCallback('YOUR REGEX GOES HERE','AND YOUR STRING HERE',function(groups) {
        //now you can access the 'groups' array containing all the captured groups
        return result; //return whatever you've processed inside
    });

