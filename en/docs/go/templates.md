---
title: "Templates"
slug: "templates"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Syntax
- t, err := template.Parse(`{{.MyName .MyAge}}`)

- t.Execute(os.Stdout,struct{MyValue,MyAge string}{"John Doe","40.1"})

Golang provides packages like:

 1. `text/template`

 2. `html/template`

to implement data-driven templates for generating textual and HTML outputs.

## Output values of struct variable to Standard Output using a text template
    package main
    
    import (
        "log"
        "text/template"
        "os"
    )
    
    type Person struct{
        MyName string
        MyAge int
    }
    
    var myTempContents string= `
    This person's name is : {{.MyName}}
    And he is {{.MyAge}} years old.
    `
    
    func main() {
        t,err := template.New("myTemp").Parse(myTempContents)
        if err != nil{
            log.Fatal(err)
        }
        myPersonSlice := []Person{ {"John Doe",41},{"Peter Parker",17} }
        for _,myPerson := range myPersonSlice{
            t.Execute(os.Stdout,myPerson)
        }
    }

[Playground](https://play.golang.org/p/HwaxzuwO7A)

## Defining functions for calling from template
 

    package main
    
    import (
        "fmt"
        "net/http"
        "os"
        "text/template"
    )
    
    var requestTemplate string = `
    {{range $i, $url := .URLs}}
    {{ $url }} {{(status_code $url)}}
    {{ end }}`
    
    type Requests struct {
        URLs []string
    }
    
    func main() {
        var fns = template.FuncMap{
            "status_code": func(x string) int {
                resp, err := http.Head(x)
                if err != nil {
                    return -1
                }
                return resp.StatusCode
            },
        }
    
        req := new(Requests)
        req.URLs = []string{"http://godoc.org", "http://stackoverflow.com", "http://linux.org"}
    
        tmpl := template.Must(template.New("getBatch").Funcs(fns).Parse(requestTemplate))
        err := tmpl.Execute(os.Stdout, req)
        if err != nil {
            fmt.Println(err)
        }
    }

Here we use our defined function `status_code` to get status code of web page right from template.

Output:

    http://godoc.org 200
    
    http://stackoverflow.com 200
    
    http://linux.org 200

