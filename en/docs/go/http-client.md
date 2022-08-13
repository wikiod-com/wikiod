---
title: "HTTP Client"
slug: "http-client"
draft: false
images: []
weight: 9520
type: docs
toc: true
---

## Syntax
- resp, err := http.Get(url) // Makes a HTTP GET request with the default HTTP client. A non-nil error is returned if the request fails.
- resp, err := http.Post(url, bodyType, body) // Makes a HTTP POST request with the default HTTP client. A non-nil error is returned if the request fails.
- resp, err := http.PostForm(url, values) // Makes a HTTP form POST request with the default HTTP client. A non-nil error is returned if the request fails.

## Parameters
| Parameter | Details |
| ------ | ------ |
|resp   | A response of type `*http.Response` to an HTTP request |
|err   | An `error`. If not nil, it represents an error that occured when the function was called. |
|url   | A URL of type `string` to make a HTTP request to. |
|bodyType   | The MIME type of type `string` of the body payload of a POST request. |
|body   | An `io.Reader` (implements `Read()`) which will be read from until an error is reached to be submitted as the body payload of a POST request.|
|values   | A key-value map of type `url.Values`. The underlying type is a `map[string][]string`.|

It is important to `defer resp.Body.Close()` after every HTTP request that does not return a non-nil error, else resources will be leaked.

## Basic GET
Perform a basic GET request and prints the contents of a site (HTML).

    package main
    
    import (
        "fmt"
        "io/ioutil"
        "net/http"
    )
    
    func main() {
        resp, err := http.Get("https://example.com/")
        if err != nil {
            panic(err)
        }
    
        // It is important to defer resp.Body.Close(), else resource leaks will occur.
        defer resp.Body.Close()
    
        data, err := ioutil.ReadAll(resp.Body)
        if err != nil {
            panic(err)
        }
    
        // Will print site contents (HTML) to output
        fmt.Println(string(data))
    }


## Time out request with a context
## 1.7+

Timing out an HTTP request with a context can be accomplished with only the standard library (not the subrepos) in 1.7+:

    import (
        "context"
        "net/http"
        "time"
    )

    req, err := http.NewRequest("GET", `https://example.net`, nil)
    ctx, _ := context.WithTimeout(context.TODO(), 200 * time.Milliseconds)
    resp, err := http.DefaultClient.Do(req.WithContext(ctx))
    // Be sure to handle errors.
    defer resp.Body.Close()

## Before 1.7

    import (
        "net/http"
        "time"

        "golang.org/x/net/context"
        "golang.org/x/net/context/ctxhttp"
    )

    ctx, err := context.WithTimeout(context.TODO(), 200 * time.Milliseconds)
    resp, err := ctxhttp.Get(ctx, http.DefaultClient, "https://www.example.net")
    // Be sure to handle errors.
    defer resp.Body.Close()

## Further Reading

For more information on the `context` package see https://www.wikiod.com/go/context

## PUT request of JSON object
The following updates a User object via a PUT request and prints the status code of the request:

```
package main

import (
    "bytes"
    "encoding/json"
    "fmt"
    "net/http"
)

type User struct {
    Name  string
    Email string
}

func main() {
    user := User{
        Name:  "John Doe",
        Email: "johndoe@example.com",
    }

    // initialize http client
    client := &http.Client{}

    // marshal User to json
    json, err := json.Marshal(user)
    if err != nil {
        panic(err)
    }

    // set the HTTP method, url, and request body
    req, err := http.NewRequest(http.MethodPut, "http://api.example.com/v1/user", bytes.NewBuffer(json))
    if err != nil {
        panic(err)
    }

    // set the request header Content-Type for json
    req.Header.Set("Content-Type", "application/json; charset=utf-8")
    resp, err := client.Do(req)
    if err != nil {
        panic(err)
    }

    fmt.Println(resp.StatusCode)
}

## GET with URL parameters and a JSON response
A request for the top 10 most recently active StackOverflow posts using the Stack Exchange API.

    package main
    
    import (
        "encoding/json"
        "fmt"
        "net/http"
        "net/url"
    )
    
    const apiURL = "https://api.stackexchange.com/2.2/posts?"
    
    // Structs for JSON decoding
    type postItem struct {
        Score int    `json:"score"`
        Link  string `json:"link"`
    }
    
    type postsType struct {
        Items []postItem `json:"items"`
    }
    
    func main() {
        // Set URL parameters on declaration
        values := url.Values{
            "order": []string{"desc"},
            "sort":  []string{"activity"},
            "site":  []string{"stackoverflow"},
        }
    
        // URL parameters can also be programmatically set
        values.Set("page", "1")
        values.Set("pagesize", "10")
    
        resp, err := http.Get(apiURL + values.Encode())
        if err != nil {
            panic(err)
        }
    
        defer resp.Body.Close()
    
        // To compare status codes, you should always use the status constants
        // provided by the http package.
        if resp.StatusCode != http.StatusOK {
            panic("Request was not OK: " + resp.Status)
        }
    
        // Example of JSON decoding on a reader.
        dec := json.NewDecoder(resp.Body)
        var p postsType
        err = dec.Decode(&p)
        if err != nil {
            panic(err)
        }
    
        fmt.Println("Top 10 most recently active StackOverflow posts:")
        fmt.Println("Score", "Link")
        for _, post := range p.Items {
            fmt.Println(post.Score, post.Link)
        }
    }


