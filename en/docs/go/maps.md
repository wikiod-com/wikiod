---
title: "Maps"
slug: "maps"
draft: false
images: []
weight: 9370
type: docs
toc: true
---

Maps are data types used for storing unordered key-value pairs, so that looking up the value associated to a given key is very efficient. Keys are unique. The underlying data structure grows as needed to accommodate new elements, so the programmer does not need to worry about memory management. They are similar to what other languages call hash tables, dictionaries, or associative arrays.

## Syntax
- var mapName map[KeyType]ValueType  // declare a Map
- var mapName = map[KeyType]ValueType{}  // declare and assign an empty Map
- var mapName = map[KeyType]ValueType{key1: val1, key2: val2}  // declare and assign a Map
- mapName := make(map[KeyType]ValueType) // declare and initialize default size map
- mapName := make(map[KeyType]ValueType, length) // declare and initialize *length* size map
- mapName := map[KeyType]ValueType{}  // auto-declare and assign an empty Map with :=
- mapName := map[KeyType]ValueType{key1: value1, key2: value2}  // auto-declare and assign a Map with :=
- value := mapName[key]  // Get value by key
- value, hasKey := mapName[key]  // Get value by key, 'hasKey' is 'true' if key exists in map
- mapName[key] = value  // Set value by key


Go provides a built-in `map` type that implements a _hash table_. Maps are Go's built-in associative data type (also called _hashes_ or _dictionaries_ in other languages).


## Declaring and initializing a map
You define a map using the keyword `map`, followed by the types of its keys and its values:

    // Keys are ints, values are ints.
    var m1 map[int]int // initialized to nil

    // Keys are strings, values are ints.
    var m2 map[string]int // initialized to nil

Maps are reference types, and once defined they have a [_zero value_ of `nil`](https://www.wikiod.com/go/maps#Zero value of a map). Writes to nil maps will [panic](https://www.wikiod.com/go/panic-and-recover#Panic) and reads will always return the zero value.

To initialize a map, use the [`make`](https://golang.org/pkg/builtin/#make) function:

    m := make(map[string]int)

With the two-parameter form of `make`, it's possible to specify an initial entry capacity for the map, overriding the default capacity:

    m := make(map[string]int, 30)

Alternatively, you can declare a map, initializing it to its zero value, and then assign a literal value to it later, which helps if you marshal the struct into json thereby producing an empty map on return.

    m := make(map[string]int, 0)

You can also make a map and set its initial value with curly brackets (`{}`).

    var m map[string]int = map[string]int{"Foo": 20, "Bar": 30}

    fmt.Println(m["Foo"]) // outputs 20

All the following statements result in the variable being bound to the same value.

    // Declare, initializing to zero value, then assign a literal value.
    var m map[string]int
    m = map[string]int{}

    // Declare and initialize via literal value.
    var m = map[string]int{}

    // Declare via short variable declaration and initialize with a literal value.
    m := map[string]int{}

We can also use a _map literal_ to [create a new map with some initial key/value pairs](https://www.wikiod.com/go/maps#Creating a map).

The key type can be any [comparable](http://golang.org/ref/spec#Comparison_operators) type; notably, [this excludes functions, maps, and slices](https://golang.org/ref/spec#Map_types). The value type can be any type, including custom types or `interface{}`.

    type Person struct {
        FirstName string
        LastName  string
    }

    // Declare via short variable declaration and initialize with make.
    m := make(map[string]Person)

    // Declare, initializing to zero value, then assign a literal value.
    var m map[string]Person
    m = map[string]Person{}

    // Declare and initialize via literal value.
    var m = map[string]Person{}

    // Declare via short variable declaration and initialize with a literal value.
    m := map[string]Person{}



## Check for element in a map
To get a value from the map, you just have to do something like:00

    value := mapName[ key ]

If the map contains the key, it returns the corresponding value.  
If not, it returns zero-value of the map's value type (`0` if map of `int` values, `""` if map of `string` values...)

    m  := map[string]string{"foo": "foo_value", "bar": ""}
    k  := m["foo"]  // returns "foo_value" since that is the value stored in the map
    k2 := m["bar"]  // returns "" since that is the value stored in the map
    k3 := m["nop"]  // returns "" since the key does not exist, and "" is the string type's zero value

To differentiate between empty values and non-existent keys, you can use the second returned value of the map access (using like `value, hasKey := map["key"]`).
  
This second value is `boolean` typed, and will be:
- `true` when the value is in the map,
- `false` when the map does not contains the given key.

Look at the following example:

    value, hasKey = m[ key ]
    if hasKey {
        // the map contains the given key, so we can safely use the value
        // If value is zero-value, it's because the zero-value was pushed to the map
    } else {
        // The map does not have the given key
        // the value will be the zero-value of the map's type
    }

## Counting map elements
The built-in function [`len`](https://golang.org/pkg/builtin/#len) returns the number of elements in a `map`

    m := map[string]int{}
    len(m) // 0

    m["foo"] = 1
    len(m) // 1

If a variable points to a `nil` map, then `len` returns 0.

    var m map[string]int
    len(m) // 0


## Zero value of a map
The zero value of a `map` is `nil` and has a length of `0`.

    var m map[string]string
    fmt.Println(m == nil) // true
    fmt.Println(len(m) ==0) // true

A `nil` map has no keys nor can keys be added. A `nil` map behaves like an empty map if read from but causes a runtime panic if written to.

    var m map[string]string
    
    // reading
    m["foo"] == "" // true. Remember "" is the zero value for a string
    _, ok = m["foo"] // ok == false
    
    // writing
    m["foo"] = "bar" // panic: assignment to entry in nil map

You should not try to read from or write to a zero value map. Instead, initialize the map (with `make` or assignment) before using it.

    var m map[string]string
    m = make(map[string]string) // OR m = map[string]string{}
    m["foo"] = "bar"

## Concurrent Access of Maps
Maps in go are not safe for concurrency. You must take a lock to read and write on them if you will be accessing them concurrently. Usually the best option is to use `sync.RWMutex` because you can have read and write locks. However, a `sync.Mutex` could also be used.


    type RWMap struct {
        sync.RWMutex
        m map[string]int
    }
    
    // Get is a wrapper for getting the value from the underlying map
    func (r RWMap) Get(key string) int {
        r.RLock()
        defer r.RUnlock()
        return r.m[key]
    }

    // Set is a wrapper for setting the value of a key in the underlying map
    func (r RWMap) Set(key string, val int) {
        r.Lock()
        defer r.Unlock()
        r.m[key] = val
    }
    
    // Inc increases the value in the RWMap for a key.
    //   This is more pleasant than r.Set(key, r.Get(key)++)
    func (r RWMap) Inc(key string) {
        r.Lock()
        defer r.Unlock()
        r.m[key]++
    }

    func main() {
    
        // Init
        counter := RWMap{m: make(map[string]int)}
    
        // Get a Read Lock
        counter.RLock()
        _ = counter.["Key"]
        counter.RUnlock()

        // the above could be replaced with
        _ = counter.Get("Key")
    
        // Get a write Lock
        counter.Lock()
        counter.m["some_key"]++
        counter.Unlock()

        // above would need to be written as 
        counter.Inc("some_key")
    }

The trade-off of the wrapper functions is between the public access of the underlying map and using the appropriate locks correctly.

## Copy a Map
Like slices, maps hold **references** to an underlying data structure. So by assigning its value to another variable, only the reference will be passed. To copy the map, it is necessary to create another map and copy each value:

    // Create the original map
    originalMap := make(map[string]int)
    originalMap["one"] = 1
    originalMap["two"] = 2

    // Create the target map
    targetMap := make(map[string]int)

    // Copy from the original map to the target map
    for key, value := range originalMap {
      targetMap[key] = value
    }

## Iterating the elements of a map
    import fmt
    
    people := map[string]int{
      "john": 30,
      "jane": 29,
      "mark": 11,
    }

    for key, value := range people {
      fmt.Println("Name:", key, "Age:", value)
    }

Note that when iterating over a map with a range loop, [the iteration order is not specified](https://blog.golang.org/go-maps-in-action) and is not guaranteed to be the same from one iteration to the next. 

You can also discard either the keys or the values of the map, if you are looking to just [grab keys](https://www.wikiod.com/go/maps#Iterating the keys of a map) or just grab values.



## Iterating the keys of a map
    people := map[string]int{
      "john": 30,
      "jane": 29,
      "mark": 11,
    }

    for key, _ := range people {
      fmt.Println("Name:", key)
    }

If you are just looking for the keys, since they are the first value, you can simply drop the underscore:

    for key := range people {
      fmt.Println("Name:", key)
    }

Note that when iterating over a map with a range loop, [the iteration order is not specified](https://blog.golang.org/go-maps-in-action) and is not guaranteed to be the same from one iteration to the next. 


## Creating a map
One can declare and initialize a map in a single statement using a [*composite literal*][1].

Using automatic type Short variable declaration:

    mapIntInt := map[int]int{10: 100, 20: 100, 30: 1000}
    mapIntString := map[int]string{10: "foo", 20: "bar", 30: "baz"}
    mapStringInt := map[string]int{"foo": 10, "bar": 20, "baz": 30}
    mapStringString := map[string]string{"foo": "one", "bar": "two", "baz": "three"}

The same code, but with Variable types:

    var mapIntInt = map[int]int{10: 100, 20: 100, 30: 1000}
    var mapIntString = map[int]string{10: "foo", 20: "bar", 30: "baz"}
    var mapStringInt = map[string]int{"foo": 10, "bar": 20, "baz": 30}
    var mapStringString = map[string]string{"foo": "one", "bar": "two", "baz": "three"}

You can also include your own structs in a map:

You can use custom types as value:

    // Custom struct types
    type Person struct {
      FirstName, LastName string
    }

    var mapStringPerson = map[string]Person{
      "john": Person{"John", "Doe"},
      "jane": Person{"Jane", "Doe"}}
    mapStringPerson := map[string]Person{
      "john": Person{"John", "Doe"},
      "jane": Person{"Jane", "Doe"}}

Your struct can also be the _key_ to the map:

    type RouteHit struct {
        Domain string
        Route  string
    }

    var hitMap = map[RouteHit]int{
      RouteHit{"example.com","/home"}: 1,
      RouteHit{"example.com","/help"}: 2}
    hitMap := map[RouteHit]int{
      RouteHit{"example.com","/home"}: 1,
      RouteHit{"example.com","/help"}: 2}


You can create an empty map simply by not entering any value within the brackets `{}`.

    mapIntInt := map[int]int{}
    mapIntString := map[int]string{}
    mapStringInt := map[string]int{}
    mapStringString := map[string]string{}
    mapStringPerson := map[string]Person{}

You can create and use a map directly, without the need to assign it to a variable. However, you will have to specify both the declaration and the content.

    // using a map as argument for fmt.Println()
    fmt.Println(map[string]string{
      "FirstName": "John",
      "LastName": "Doe",
      "Age": "30"})

    // equivalent to
    data := map[string]string{
      "FirstName": "John",
      "LastName": "Doe",
      "Age": "30"}
    fmt.Println(data)


  [1]: https://golang.org/ref/spec#Composite_literals

## Deleting a map element
The [`delete`](https://golang.org/pkg/builtin/#delete) built-in function removes the element with the specified key from a map.

    people := map[string]int{"john": 30, "jane": 29}
    fmt.Println(people) // map[john:30 jane:29]

    delete(people, "john")
    fmt.Println(people) // map[jane:29]

If the `map` is `nil` or there is no such element, `delete` has no effect.

    people := map[string]int{"john": 30, "jane": 29}
    fmt.Println(people) // map[john:30 jane:29]

    delete(people, "notfound")
    fmt.Println(people) // map[john:30 jane:29]

    var something map[string]int
    delete(something, "notfound") // no-op


## Using a map as a set
Some languages have a native structure for sets. To make a set in Go, it's best practice to use a map from the value type of the set to an empty struct (`map[Type]struct{}`).

For example, with strings:

    // To initialize a set of strings:
    greetings := map[string]struct{}{
        "hi":    {},
        "hello": {},
    }

    // To delete a value:
    delete(greetings, "hi")

    // To add a value:
    greetings["hey"] = struct{}{}

    // To check if a value is in the set:
    if _, ok := greetings["hey"]; ok {
        fmt.Println("hey is in greetings")
    }


## Iterating the values of a map
    people := map[string]int{
      "john": 30,
      "jane": 29,
      "mark": 11,
    }
    
    for _, value := range people {
      fmt.Println("Age:", value)
    }

Note that when iterating over a map with a range loop, [the iteration order is not specified](https://blog.golang.org/go-maps-in-action) and is not guaranteed to be the same from one iteration to the next. 


## Creating maps with slices as values
    m := make(map[string][]int)

Accessing a non-existent key will return a nil slice as a value. Since nil slices act like zero length slices when used with `append` or other built-in functions you do not normally need to check to see if a key exists:

    // m["key1"] == nil && len(m["key1"]) == 0
    m["key1"] = append(m["key1"], 1)
    // len(m["key1"]) == 1

Deleting a key from map sets the key back to a nil slice:

    delete(m, "key1")
    // m["key1"] == nil

