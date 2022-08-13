---
title: "Text + HTML Templating"
slug: "text-+-html-templating"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Single item template
Note the use of `{{.}}` to output the item within the template.

    package main

    import (
        "fmt"
        "os"
        "text/template"
    )

    func main() {
        const (
            letter = `Dear {{.}}, How are you?`
        )

        tmpl, err := template.New("letter").Parse(letter)
        if err != nil {
            fmt.Println(err.Error())
        }

        tmpl.Execute(os.Stdout, "Professor Jones")
    }

Results in:

    Dear Professor Jones, How are you?


## Multiple item template
Note the use of `{{range .}}` and `{{end}}` to cycle over the collection.

    package main

    import (
        "fmt"
        "os"
        "text/template"
    )

    func main() {
        const (
            letter = `Dear {{range .}}{{.}}, {{end}} How are you?`
        )

        tmpl, err := template.New("letter").Parse(letter)
        if err != nil {
            fmt.Println(err.Error())
        }

        tmpl.Execute(os.Stdout, []string{"Harry", "Jane", "Lisa", "George"})
    }

Results in:

    Dear Harry, Jane, Lisa, George,  How are you?

## Templates with custom logic
In this example, a function map named `funcMap` is supplied to the template via the `Funcs()` method and then invoked inside the template. Here, the function `increment()` is used to get around the lack of a less than or equal function in the templating language. Note in the output how the final item in the collection is handled.

A `-` at the beginning ``{{-`` or end ``-}}`` is used to trim whitespace and can be used to help make the template more legible.

    package main

    import (
        "fmt"
        "os"
        "text/template"
    )

    var funcMap = template.FuncMap{
        "increment": increment,
    }

    func increment(x int) int {
        return x + 1
    }

    func main() {
        const (
            letter = `Dear {{with $names := .}}
            {{- range $i, $val := $names}}
                {{- if lt (increment $i) (len $names)}}
                    {{- $val}}, {{else -}} and {{$val}}{{end}}
            {{- end}}{{end}}; How are you?`
        )

        tmpl, err := template.New("letter").Funcs(funcMap).Parse(letter)
        if err != nil {
            fmt.Println(err.Error())
        }

        tmpl.Execute(os.Stdout, []string{"Harry", "Jane", "Lisa", "George"})
    }

Results in:

    Dear Harry, Jane, Lisa, and George; How are you?

## Templates with structs
Note how field values are obtained using `{{.FieldName}}`.

    package main

    import (
        "fmt"
        "os"
        "text/template"
    )

    type Person struct {
        FirstName string
        LastName  string
        Street    string
        City      string
        State     string
        Zip       string
    }

    func main() {
        const (
            letter = `------------------------------
    {{range .}}{{.FirstName}} {{.LastName}}
    {{.Street}}
    {{.City}}, {{.State}} {{.Zip}}

    Dear {{.FirstName}},
        How are you?

    ------------------------------
    {{end}}`
        )

        tmpl, err := template.New("letter").Parse(letter)
        if err != nil {
            fmt.Println(err.Error())
        }

        harry := Person{
            FirstName: "Harry",
            LastName:  "Jones",
            Street:    "1234 Main St.",
            City:      "Springfield",
            State:     "IL",
            Zip:       "12345-6789",
        }

        jane := Person{
            FirstName: "Jane",
            LastName:  "Sherman",
            Street:    "8511 1st Ave.",
            City:      "Dayton",
            State:     "OH",
            Zip:       "18515-6261",
        }

        tmpl.Execute(os.Stdout, []Person{harry, jane})
    }

Results in:

    ------------------------------
    Harry Jones
    1234 Main St.
    Springfield, IL 12345-6789
    
    Dear Harry,
        How are you?
    
    ------------------------------
    Jane Sherman
    8511 1st Ave.
    Dayton, OH 18515-6261
    
    Dear Jane,
        How are you?
    
    ------------------------------

## HTML templates
Note the different package import.

    package main

    import (
        "fmt"
        "html/template"
        "os"
    )

    type Person struct {
        FirstName string
        LastName  string
        Street    string
        City      string
        State     string
        Zip       string
        AvatarUrl string
    }

    func main() {
        const (
            letter = `<html><body><table>
    <tr><th></th><th>Name</th><th>Address</th></tr>
    {{range .}}
    <tr>
    <td><img src="{{.AvatarUrl}}"></td>
    <td>{{.FirstName}} {{.LastName}}</td>
    <td>{{.Street}}, {{.City}}, {{.State}} {{.Zip}}</td>
    </tr>
    {{end}}
    </table></body></html>`
        )

        tmpl, err := template.New("letter").Parse(letter)
        if err != nil {
            fmt.Println(err.Error())
        }

        harry := Person{
            FirstName: "Harry",
            LastName:  "Jones",
            Street:    "1234 Main St.",
            City:      "Springfield",
            State:     "IL",
            Zip:       "12345-6789",
            AvatarUrl: "harry.png",
        }

        jane := Person{
            FirstName: "Jane",
            LastName:  "Sherman",
            Street:    "8511 1st Ave.",
            City:      "Dayton",
            State:     "OH",
            Zip:       "18515-6261",
            AvatarUrl: "jane.png",
        }

        tmpl.Execute(os.Stdout, []Person{harry, jane})
    }

Results in:

    <html><body><table>
    <tr><th></th><th>Name</th><th>Address</th></tr>
    
    <tr>
    <td><img src="harry.png"></td>
    <td>Harry Jones</td>
    <td>1234 Main St., Springfield, IL 12345-6789</td>
    </tr>
    
    <tr>
    <td><img src="jane.png"></td>
    <td>Jane Sherman</td>
    <td>8511 1st Ave., Dayton, OH 18515-6261</td>
    </tr>

    </table></body></html>

## How HTML templates prevent malicious code injection
First, here's what can happen when `text/template` is used for HTML. Note Harry's `FirstName` property).

    package main

    import (
        "fmt"
        "html/template"
        "os"
    )

    type Person struct {
        FirstName string
        LastName  string
        Street    string
        City      string
        State     string
        Zip       string
        AvatarUrl string
    }

    func main() {
        const (
            letter = `<html><body><table>
    <tr><th></th><th>Name</th><th>Address</th></tr>
    {{range .}}
    <tr>
    <td><img src="{{.AvatarUrl}}"></td>
    <td>{{.FirstName}} {{.LastName}}</td>
    <td>{{.Street}}, {{.City}}, {{.State}} {{.Zip}}</td>
    </tr>
    {{end}}
    </table></body></html>`
        )

        tmpl, err := template.New("letter").Parse(letter)
        if err != nil {
            fmt.Println(err.Error())
        }

        harry := Person{
            FirstName: `Harry<script>alert("You've been hacked!")</script>`,
            LastName:  "Jones",
            Street:    "1234 Main St.",
            City:      "Springfield",
            State:     "IL",
            Zip:       "12345-6789",
            AvatarUrl: "harry.png",
        }

        jane := Person{
            FirstName: "Jane",
            LastName:  "Sherman",
            Street:    "8511 1st Ave.",
            City:      "Dayton",
            State:     "OH",
            Zip:       "18515-6261",
            AvatarUrl: "jane.png",
        }

        tmpl.Execute(os.Stdout, []Person{harry, jane})
    }

Results in:

    <html><body><table>
    <tr><th></th><th>Name</th><th>Address</th></tr>
    
    <tr>
    <td><img src="harry.png"></td>
    <td>Harry<script>alert("You've been hacked!")</script> Jones</td>
    <td>1234 Main St., Springfield, IL 12345-6789</td>
    </tr>
    
    <tr>
    <td><img src="jane.png"></td>
    <td>Jane Sherman</td>
    <td>8511 1st Ave., Dayton, OH 18515-6261</td>
    </tr>
    
    </table></body></html>

The above example, if accessed from a browser, would result in the script being executed an an alert being generated. If, instead, the `html/template` were imported instead of `text/template`, the script would be safely sanitized:

    <html><body><table>
    <tr><th></th><th>Name</th><th>Address</th></tr>
    
    <tr>
    <td><img src="harry.png"></td>
    <td>Harry&lt;script&gt;alert(&#34;You&#39;ve been hacked!&#34;)&lt;/script&gt; Jones</td>
    <td>1234 Main St., Springfield, IL 12345-6789</td>
    </tr>
    
    <tr>
    <td><img src="jane.png"></td>
    <td>Jane Sherman</td>
    <td>8511 1st Ave., Dayton, OH 18515-6261</td>
    </tr>
    
    </table></body></html>

The second result would look garbled when loaded in a browser, but would not result in a potentially malicious script executing.

