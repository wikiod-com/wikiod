---
title: "Groovy Truth (true-ness)"
slug: "groovy-truth-true-ness"
draft: false
images: []
weight: 9897
type: docs
toc: true
---

Groovy evaluates conditions in **if**, **while** and **for** statements **the same way as Java does for standard Java conditions** :
in Java you must provide a boolean expression (an expression that evaluates to a boolean) and the result is 
the result of the evaluation. 

In Groovy , the result is the same as in Java for thoses conditions
(no examples provided, this is standard Java).


The other **truthfulness evaluation mechanism** shown by examples can be summarized as:
- numbers:  a zero value evaluates to false, non zero to true.
- objects: a null object reference evaluates to false, a non null reference to true.
- Character : a character with a zero value evaluates to false, true otherwise.
- String : a string evaluates to true if not null and not empty, false if null or empty (applies to GStrings and CharSequences too).
- Collections and Maps (including subclasses  **List**, **Map**, **Set**, **HashSet** ...) :  also takes into account the size, evaluates to true if the collection is not null and not empty, false if null or empty.
- Enumerations and Iterators evaluates to true if not null and they are some more elements (groovy evaluates **hasMoreElements** or **hasNext** on the object), false if null or no more elements.
- Matcher : a matcher evaluates to true if there is at least one match, false if not match is found.
- Closure : a closure evaluates to the evaluation of the result returned by the closure.

The asBoolean method can be overriden in a user defined class to provide custom boolean evaluation.

## Numbers boolean evaluation
**for numbers, a zero value evaluates to false, non zero to true**

        int i = 0
    ...
        if (i)
            print "some ${i}"
        else
            print "nothing"

will print "nothing"



## Strings boolean evaluation
**a string (including GStrings) evaluates to true if not null and not empty, false if null or empty**

    def s = ''
    ...
    if (s) 
        println 's is not empty'
    else
        println 's is empty'

will print: 's is empty'

## Collections and maps boolean evaluation
**Collections and Maps evaluates to true if not null and  not empty and false if null or empty**

    /* an empty map example*/
    def userInfo = [:]
    if (!userInfo)
        userInfo << ['user': 'Groot', 'species' : 'unknown' ]

will add `user: 'Groot' , species : 'unknown'` as default userInfo since the userInfo map is empty (note that the map is not null here)

With a null object, the code is lightly different, we cannot invoke  << on userInfo because it is null, we have to make an assignment (refer also to Object boolean evaluation):

    /* an example with a null object (def does not implies Map type) */
    def userInfo = null
    if (!userInfo)
        userInfo = ['user': 'Groot', 'species' : 'unknown' ]
And with a null Map:

    /* The same example with a null Map */
    Map<String,String> userInfo = null
    if (!userInfo)
        userInfo = ['user': 'Groot', 'species' : 'unknown' ]




## Object boolean evaluation
a null object reference evaluates to false, a non null reference to true,
but for for strings, collections, iterators and enumerations it also takes into account the size.

    def m = null
    
    if (!m)
        println "empty"
    else
        println "${m}"

will print "empty"

    def m = [:]
    
    if (!m)
        println "empty"
    else
        println "${m}"

The map is not null but empty, this code will print "empty"

After doing

    m << ['user' : 'Groot' ]

it will print the Map:

    [user:Groot]

## Overriding boolean evaluation in a user defined class
Sometimes it may be useful to have a specific asBoolean  definition in your own program for some kind of objects.
 

    /** an oversimplified robot controller */
    class RunController {
        
        def complexCondition
        int position = 0
        
        def asBoolean() {
            return complexCondition(this);
        }
        def advanceTo(step) {
            position += step
        }
    }
    def runController = new RunController(complexCondition : { c -> c.position < 10 } )
    
    assert runController
    runController.advanceTo(5)
    assert runController
    runController.advanceTo(5)
    // The limit has been reached : the controller evaluates to false
    assert !runController

This code shows an oversimplifed robot controller who checks that the position of the robot does not exceeds 10 (with a closure for condition evaluation)


## Character evaluation
a Character evaluates to true if it's value is not zero, false if zero

    assert ! new Character((char)0)
    assert ! new Character('\u0000Hello Zero Char'.charAt(0))
    assert  new Character('Hello'.charAt(0))



## Matcher evaluation
a Matcher evaluates to true if it can find at least one match, false if no match is found 

    // a match is found => true
    assert 'foo' =~ /[a-z]/
    // the regexp does not match fully => no match => false
    assert !( 'foo' ==~ /[a-z]/ )
    // a match is found => true
    assert 'foo' =~ /o/
    // no match => false
    assert !( 'foo' =~ /[A-Z]/ )

## Closure evaluation
The evaluation of a closure is the evaluation of the result of the closure.

All rules applies : if the closure returns a null , zero number or empty String, Collection, Map or Array it evaluates to false
otherwise to true.

    // Closure return non zero number => true
    assert { 42 }()
    // closure returns 0 => false
    assert ! ( { 0 }())
    // closure returns null => false
    assert !( { }())

