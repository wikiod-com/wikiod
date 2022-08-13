---
title: "JSON"
slug: "json"
draft: false
images: []
weight: 9866
type: docs
toc: true
---

## Syntax
- func Marshal(v interface{}) ([]byte, error)
- func Unmarshal(data []byte, v interface{}) error


The package [`"encoding/json"`][1] Package json implements encoding and decoding of JSON objects in `Go`.


----------


Types in JSON along with their corresponding concrete types in Go are:

| JSON Type | Go Concrete Type |
| ------ | ------ |
| boolean   | bool   |
| numbers   | float64 or int   |
| string   | string   |
| null   | nil   |



  [1]: https://golang.org/pkg/encoding/json/

## Basic JSON decoding
[`json.Unmarshal`](https://golang.org/pkg/encoding/json/#Marshal) from the package `"encoding/json"` decodes a JSON value into the value pointed by the given variable.

The parameters are the value to decode in `[]bytes` and a variable to use as a storage for the de-serialized value. The returned value is an error (on failure).

    encodedValue := []byte(`{"London":18,"Rome":30}`)

    // generic storage for the decoded JSON
    var data map[string]interface{}

    // decode the value into data
    // notice that we must pass the pointer to data using &data
    err := json.Unmarshal(encodedValue, &data)

    // check if the decoding is successful
    if err != nil {
        panic(err)
    }

    fmt.Println(data)
    map[London:18 Rome:30]

[Playground](https://play.golang.org/p/CjplBCptH8)

Notice how in the example above we knew in advance both the type of the key and the value. But this is not always the case. In fact, in most cases the JSON contains mixed value types.

    encodedValue := []byte(`{"city":"Rome","temperature":30}`)

    // generic storage for the decoded JSON
    var data map[string]interface{}

    // decode the value into data
    if err := json.Unmarshal(encodedValue, &data); err != nil {
        panic(err)
    }

    // if you want to use a specific value type, we need to cast it
    temp := data["temperature"].(float64)
    fmt.Println(temp) // 30
    city := data["city"].(string)
    fmt.Println(city) // "Rome"

[Playground](https://play.golang.org/p/SawE86QKRt)

In the last example above we used a generic map to store the decoded value. We must use a `map[string]interface{}` because we know that the keys are strings, but we don't know the type of their values in advance.

This is a very simple approach, but it's also extremely limited. In the real world, you would generally [decode a JSON into a custom-defined `struct` type](https://www.wikiod.com/go/json).


## Using anonymous structs for decoding
The goal with using anonymous structs is to decode only the information we care about without littering our app with types that are used only in a single function.

    jsonBlob := []byte(`
      {
        "_total": 1,
        "_links": {
          "self": "https://api.twitch.tv/kraken/channels/foo/subscriptions?direction=ASC&limit=25&offset=0",
          "next": "https://api.twitch.tv/kraken/channels/foo/subscriptions?direction=ASC&limit=25&offset=25"
        },
        "subscriptions": [
          {
            "created_at": "2011-11-23T02:53:17Z",
            "_id": "abcdef0000000000000000000000000000000000",
            "_links": {
              "self": "https://api.twitch.tv/kraken/channels/foo/subscriptions/bar"
            },
            "user": {
              "display_name": "bar",
              "_id": 123456,
              "name": "bar",
              "staff": false,
              "created_at": "2011-06-16T18:23:11Z",
              "updated_at": "2014-10-23T02:20:51Z",
              "logo": null,
              "_links": {
                "self": "https://api.twitch.tv/kraken/users/bar"
              }
            }
          }
        ]
      }
    `)

    var js struct {
        Total int `json:"_total"`
        Links struct {
            Next string `json:"next"`
        } `json:"_links"`
        Subs []struct {
            Created string `json:"created_at"`
            User    struct {
                Name string `json:"name"`
                ID   int    `json:"_id"`
            } `json:"user"`
        } `json:"subscriptions"`
    }

    err := json.Unmarshal(jsonBlob, &js)
    if err != nil {
        fmt.Println("error:", err)
    }
    fmt.Printf("%+v", js)

Output:
`{Total:1 Links:{Next:https://api.twitch.tv/kraken/channels/foo/subscriptions?direction=ASC&limit=25&offset=25} Subs:[{Created:2011-11-23T02:53:17Z User:{Name:bar ID:123456}}]}`

[Playground](https://play.golang.org/p/bSNc758imH)

For the general case see also: https://www.wikiod.com/go/json


## Basic JSON Encoding
[`json.Marshal`](https://golang.org/pkg/encoding/json/#Marshal) from the package `"encoding/json"` encodes a value to JSON.

The parameter is the value to encode. The returned values are an array of bytes representing the JSON-encoded input (on success), and an error (on failure).

    decodedValue := []string{"foo", "bar"}

    // encode the value
    data, err := json.Marshal(decodedValue)
    
    // check if the encoding is successful
    if err != nil {
        panic(err)
    }

    // print out the JSON-encoded string
    // remember that data is a []byte
    fmt.Println(string(data))
    // "["foo","bar"]"

[Playground](https://play.golang.org/p/ihOs95HToW)

Here's some basic examples of encoding for built-in data types:

    var data []byte

    data, _ = json.Marshal(1)
    fmt.Println(string(data))
    // 1

    data, _ = json.Marshal("1")
    fmt.Println(string(data))
    // "1"

    data, _ = json.Marshal(true)
    fmt.Println(string(data))
    // true

    data, _ = json.Marshal(map[string]int{"London": 18, "Rome": 30})
    fmt.Println(string(data))
    // {"London":18,"Rome":30}

[Playground](https://play.golang.org/p/pcX_AGeSIz)

Encoding simple variables is helpful to understand how the JSON encoding works in Go. However, in the real world, you'll likely [encode more complex data stored in structs](https://www.wikiod.com/go/json).

## Decoding JSON data from a file
JSON data can also be read from files.

Let's assume we have a file called `data.json` with the following content:


    [
        {
          "Name" : "John Doe",
          "Standard" : 4
        },
        {
          "Name" : "Peter Parker",
          "Standard" : 11
        },
        {
          "Name" : "Bilbo Baggins",
          "Standard" : 150
        }
    ]

The following example reads the file and decodes the content:

    package main
    
    import (
        "encoding/json"
        "fmt"
        "log"
        "os"
    )
    
    type Student struct {
        Name     string
        Standard int `json:"Standard"`
    }
    
    func main() {
        // open the file pointer
        studentFile, err := os.Open("data.json")
        if err != nil {
            log.Fatal(err)
        }
        defer studentFile.Close()

        // create a new decoder
        var studentDecoder *json.Decoder = json.NewDecoder(studentFile)
        if err != nil {
            log.Fatal(err)
        }

        // initialize the storage for the decoded data
        var studentList []Student
        
        // decode the data
        err = studentDecoder.Decode(&studentList)
        if err != nil {
            log.Fatal(err)
        }

        for i, student := range studentList {
            fmt.Println("Student", i+1)
            fmt.Println("Student name:", student.Name)
            fmt.Println("Student standard:", student.Standard)
        }
    }

The file `data.json` must be in the same directory of the Go executable program. Read [Go File I/O documentation][1] for more information on how to work with files in Go.

  [1]: https://www.wikiod.com/go/file-io


## Configuring JSON struct fields
Consider the following example:

    type Company struct {
        Name     string
        Location string
    }

### Hide/Skip Certain Fields

To export `Revenue` and `Sales`, but hide them from encoding/decoding, use `json:"-"` or rename the variable to begin with a lowercase letter. Note that this prevents the variable from being visible outside the package.

    type Company struct {
        Name     string `json:"name"`
        Location string `json:"location"`
        Revenue  int    `json:"-"`
        sales    int
    }

### Ignore Empty Fields

To prevent `Location` from being included in the JSON when it is set to its zero value, add `,omitempty` to the `json` tag.

    type Company struct {
        Name     string `json:"name"`
        Location string `json:"location,omitempty"`
    }

[Example in Playground](https://play.golang.org/p/q8keNCcYAn)

## Marshaling structs with private fields
As a good developer you have created following struct with both exported and unexported fields:

```
type MyStruct struct {
    uuid string    
    Name string
}
```

Example in Playground:
https://play.golang.org/p/Zk94Il2ANZ

Now you want to `Marshal()` this struct into valid JSON for storage in something like etcd. However, since `uuid` in not exported, the `json.Marshal()` skips it. What to do? Use an anonymous struct and the `json.MarshalJSON()` interface! Here's an example:

```
type MyStruct struct {
    uuid string    
    Name string
}

func (m MyStruct) MarshalJSON() ([]byte, error {
    j, err := json.Marshal(struct {
        Uuid string
        Name string
    }{
        Uuid: m.uuid,
        Name: m.Name,
    })
    if err != nil {
           return nil, err
    }
    return j, nil
}
```

Example in Playground:
https://play.golang.org/p/Bv2k9GgbzE



## Encoding/Decoding using Go structs
Let's assume we have the following `struct` that defines a `City` type:

    type City struct {  
        Name string  
        Temperature int  
    }

We can encode/decode City values using the [`encoding/json`](https://golang.org/pkg/encoding/json/) package.

First of all, we need to use the Go metadata to tell the encoder the correspondence between the struct fields and the JSON keys.

    type City struct {  
        Name string `json:"name"`  
        Temperature int `json:"temp"`  
        // IMPORTANT: only exported fields will be encoded/decoded  
        // Any field starting with a lower letter will be ignored  
    }  

To keep this example simple, we'll declare an explicit correspondence between the fields and the keys. However, you can use several variants of the `json:` metadata [as explained in the docs](https://golang.org/pkg/encoding/json/#Marshal).

**IMPORTANT:** **Only [exported fields](https://www.wikiod.com/go/structs#Exported vs. Unexported Fields (Private vs Public)) (fields with capital name) will be serialized/deserialized.** For example, if you name the field _**t**emperature_ it will be ignored even if you set the `json` metadata.

## Encoding

To encode a `City` struct, use `json.Marshal` as in the basic example:

    // data to encode  
    city := City{Name: "Rome", Temperature: 30}  
     
    // encode the data  
    bytes, err := json.Marshal(city)  
    if err != nil {  
        panic(err)  
    }  
     
    fmt.Println(string(bytes))  
    // {"name":"Rome","temp":30} 

[Playground](https://play.golang.org/p/KlziJIDWPW)

## Decoding

To decode a `City` struct, use `json.Unmarshal` as in the basic example:

    // data to decode  
    bytes := []byte(`{"name":"Rome","temp":30}`)  
     
    // initialize the container for the decoded data  
    var city City  
     
    // decode the data  
    // notice the use of &city to pass the pointer to city  
    if err := json.Unmarshal(bytes, &city); err != nil {  
        panic(err)  
    }  
     
    fmt.Println(city)  
    // {Rome 30} 

[Playground](https://play.golang.org/p/VHS28E-234)

