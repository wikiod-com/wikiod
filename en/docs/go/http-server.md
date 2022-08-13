---
title: "HTTP Server"
slug: "http-server"
draft: false
images: []
weight: 9662
type: docs
toc: true
---

[`http.ServeMux`](https://godoc.org/net/http#ServeMux) provides a multiplexer which calls handlers for HTTP requests.

Alternatives to the standard library multiplexer include:

  - [Gorilla Mux][1]

    [1]: https://godoc.org/github.com/gorilla/mux

## Hello World
The typical way to begin writing webservers in golang is to use the standard library `net/http` module.  

There is also a tutorial for it [here][1].  

The following code also uses it.  Here is the simplest possible HTTP server implementation. It responds `"Hello World"` to any HTTP request.

Save the following code in a `server.go` file in your workspaces.

    package main
    
    import (
        "log"
        "net/http"
    )
    
    func main() {
        // All URLs will be handled by this function
        // http.HandleFunc uses the DefaultServeMux
        http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
            w.Write([]byte("Hello, world!"))
        })
    
        // Continue to process new requests until an error occurs
        log.Fatal(http.ListenAndServe(":8080", nil))
    }

You can run the server using:

    $ go run server.go

Or you can compile and run.

    $ go build server.go
    $ ./server

The server will listen to the specified port (`:8080`). You can test it with any HTTP client. Here's an example with `cURL`:

    curl -i http://localhost:8080/
    HTTP/1.1 200 OK
    Date: Wed, 20 Jul 2016 18:04:46 GMT
    Content-Length: 13
    Content-Type: text/plain; charset=utf-8
    
    Hello, world!

Press <kbd>Ctrl</kbd>+<kbd>C</kbd> to stop the process.


  [1]: https://golang.org/doc/articles/wiki/

## Create a HTTPS Server
# Generate a certificate
In order to run a HTTPS server, a certificate is necessary. Generating a self-signed certificate with `openssl` is done by executing this command:

    openssl req -x509 -newkey rsa:4096 -sha256 -nodes -keyout key.pem -out cert.pem -subj "/CN=example.com" -days 3650`

The parameters are:

 - `req` Use the certificate request tool
 - `x509` Creates a self-signed certificate
 - `newkey rsa:4096` Creates a new key and certificate by using the RSA algorithms with `4096` bit key length
 - `sha256` Forces the SHA256 hashing algorithms which major browsers consider as secure (at the year 2017)
 - `nodes` Disables the password protection for the private key. Without this parameter, your server had to ask you for the password each time its starts.
 - `keyout` Names the file where to write the key
 - `out` Names the file where to write the certificate
 - `subj` Defines the domain name for which this certificate is valid
 - `days` Fow how many days should this certificate valid? `3650` are approx. 10 years.

Note: A self-signed certificate could be used e.g. for internal projects, debugging, testing, etc. Any browser out there will mention, that this certificate is not safe. In order to avoid this, the certificate must signed by a certification authority. Mostly, this is not available for free. One exception is the "Let's Encrypt" movement: https://letsencrypt.org

# The necessary Go code
You can handle configure TLS for the server with the following code. `cert.pem` and `key.pem` are your SSL certificate and key, which where generated with the above command.

    package main
    
    import (
        "log"
        "net/http"
    )
    
    func main() {
        http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
            w.Write([]byte("Hello, world!"))
        })
    
        log.Fatal(http.ListenAndServeTLS(":443","cert.pem","key.pem", nil))
    }

## HTTP Hello World with custom server and mux
    package main
    
    import (
        "log"
        "net/http"
    )
    
    func main() {

        // Create a mux for routing incoming requests
        m := http.NewServeMux()

        // All URLs will be handled by this function
        m.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
            w.Write([]byte("Hello, world!"))
        })

        // Create a server listening on port 8000
        s := &http.Server{
            Addr:    ":8000",
            Handler: m,
        }

        // Continue to process new requests until an error occurs
        log.Fatal(s.ListenAndServe())
    }

Press <kbd>Ctrl</kbd>+<kbd>C</kbd> to stop the process.

## Responding to an HTTP Request using Templates
Responses can be written to a `http.ResponseWriter` using templates in Go. This proves as a handy tool if you wish to create dynamic pages.

(To learn how Templates work in Go, please visit the [Go Templates Documentation][1] page.)

Continuing with a simple example to utilise the `html/template` to respond to an HTTP Request:

    package main
    
    import(
        "html/template"
        "net/http"
        "log"
    )
    
    func main(){
        http.HandleFunc("/",WelcomeHandler)
        http.ListenAndServe(":8080",nil)
    }
    
    type User struct{
        Name string
        nationality string //unexported field.
    }
    
    func check(err error){
        if err != nil{
            log.Fatal(err)
        }
    }
    
    func WelcomeHandler(w http.ResponseWriter, r *http.Request){
        if r.Method == "GET"{
            t,err := template.ParseFiles("welcomeform.html")
            check(err)
            t.Execute(w,nil)
        }else{
            r.ParseForm()
            myUser := User{}
            myUser.Name = r.Form.Get("entered_name")
            myUser.nationality = r.Form.Get("entered_nationality")
            t, err := template.ParseFiles("welcomeresponse.html")
            check(err)
            t.Execute(w,myUser)
        }
    }

Where, the contents of

1) `welcomeform.html` are:


    <head>
        <title> Help us greet you </title>
    </head>
    <body>
        <form method="POST" action="/">
            Enter Name: <input type="text" name="entered_name">
            Enter Nationality: <input type="text" name="entered_nationality">
            <input type="submit" value="Greet me!">
        </form>
    </body>


1) `welcomeresponse.html` are:


    <head>
        <title> Greetings, {{.Name}} </title>
    </head>
    <body>
        Greetings, {{.Name}}.<br>
        We know you are a {{.nationality}}!
    </body>

Note: 

1) Make sure that the `.html` files are in the correct directory.

2) When `http://localhost:8080/` can be visited after starting the server.

3) As it can be seen after submitting the form, the *unexported* nationality field of the struct could not be parsed by the template package, as expected.

  [1]: https://www.wikiod.com/go/templates

## Serving content using ServeMux
A simple static file server would look like this:

    package main

    import (
        "net/http"
    )

    func main() {
        muxer := http.NewServeMux()
        fileServerCss := http.FileServer(http.Dir("src/css"))
        fileServerJs := http.FileServer(http.Dir("src/js"))
        fileServerHtml := http.FileServer(http.Dir("content"))
        muxer.Handle("/", fileServerHtml)
        muxer.Handle("/css", fileServerCss)
        muxer.Handle("/js", fileServerJs)
        http.ListenAndServe(":8080", muxer)
    }

## Using a handler function
[`HandleFunc`](https://golang.org/pkg/net/http/#ListenAndServe) registers the handler function for the given pattern in the server mux (router).

You can pass define an anonymous function, as we have seen in the basic _Hello World_ example:

    http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        fmt.Fprintln(w, "Hello, world!")
    }

But we can also pass a [`HandlerFunc`](https://golang.org/pkg/net/http/#HandlerFunc) type. In other words, we can pass any function that respects the following signature:

    func FunctionName(w http.ResponseWriter, req *http.Request)

We can rewrite the previous example passing the reference to a previously defined `HandlerFunc`. Here's the full example:

    package main
    
    import (
        "fmt"
        "net/http"
    )
    
    // A HandlerFunc function
    // Notice the signature of the function
    func RootHandler(w http.ResponseWriter, req *http.Request) {
        fmt.Fprintln(w, "Hello, world!")
    }
    
    func main() {
        // Here we pass the reference to the `RootHandler` handler function
        http.HandleFunc("/", RootHandler)
        panic(http.ListenAndServe(":8080", nil))
    }

Of course, you can define several function handlers for different paths.

    package main
    
    import (
        "fmt"
        "log"
        "net/http"
    )
    
    func FooHandler(w http.ResponseWriter, req *http.Request) {
        fmt.Fprintln(w, "Hello from foo!")
    }
    
    func BarHandler(w http.ResponseWriter, req *http.Request) {
        fmt.Fprintln(w, "Hello from bar!")
    }
    
    func main() {
        http.HandleFunc("/foo", FooHandler)
        http.HandleFunc("/bar", BarHandler)
    
        log.Fatal(http.ListenAndServe(":8080", nil))
    }

Here's the output using `cURL`:

    ➜  ~ curl -i localhost:8080/foo
    HTTP/1.1 200 OK
    Date: Wed, 20 Jul 2016 18:23:08 GMT
    Content-Length: 16
    Content-Type: text/plain; charset=utf-8
    
    Hello from foo!

    ➜  ~ curl -i localhost:8080/bar
    HTTP/1.1 200 OK
    Date: Wed, 20 Jul 2016 18:23:10 GMT
    Content-Length: 16
    Content-Type: text/plain; charset=utf-8
    
    Hello from bar!

    ➜  ~ curl -i localhost:8080/
    HTTP/1.1 404 Not Found
    Content-Type: text/plain; charset=utf-8
    X-Content-Type-Options: nosniff
    Date: Wed, 20 Jul 2016 18:23:13 GMT
    Content-Length: 19
    
    404 page not found



## Handling http method, accessing query strings & request body
Here are a simple example of some common tasks related to developing an API, differentiating between the HTTP Method of the request, accessing query string values and accessing the request body.

Resources
 * [http.Handler interface](https://golang.org/pkg/net/http/#Handler)
 * [http.ResponseWriter](https://golang.org/pkg/net/http/#ResponseWriter)
 * [http.Request](https://golang.org/pkg/net/http/#Request)
 * [Available Method and Status constants](https://golang.org/pkg/net/http/#pkg-constants)


    package main

    import (
        "fmt"
        "io/ioutil"
        "log"
        "net/http"
    )

    type customHandler struct{}

    // ServeHTTP implements the http.Handler interface in the net/http package
    func (h customHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {

        // ParseForm will parse query string values and make r.Form available
        r.ParseForm()

        // r.Form is map of query string parameters
        // its' type is url.Values, which in turn is a map[string][]string
        queryMap := r.Form

        switch r.Method {
        case http.MethodGet:
            // Handle GET requests
            w.WriteHeader(http.StatusOK)
            w.Write([]byte(fmt.Sprintf("Query string values: %s", queryMap)))
            return
        case http.MethodPost:
            // Handle POST requests
            body, err := ioutil.ReadAll(r.Body)
            if err != nil {
                // Error occurred while parsing request body
                w.WriteHeader(http.StatusBadRequest)
                return
            }
            w.WriteHeader(http.StatusOK)
            w.Write([]byte(fmt.Sprintf("Query string values: %s\nBody posted: %s", queryMap, body)))
            return
        }

        // Other HTTP methods (eg PUT, PATCH, etc) are not handled by the above
        // so inform the client with appropriate status code
        w.WriteHeader(http.StatusMethodNotAllowed)
    }

    func main() {
        // All URLs will be handled by this function
        // http.Handle, similarly to http.HandleFunc
        // uses the DefaultServeMux
        http.Handle("/", customHandler{})

        // Continue to process new requests until an error occurs
        log.Fatal(http.ListenAndServe(":8080", nil))
    }

Sample curl output:

    $ curl -i 'localhost:8080?city=Seattle&state=WA' -H 'Content-Type: text/plain' -X GET
    HTTP/1.1 200 OK
    Date: Fri, 02 Sep 2016 16:36:24 GMT
    Content-Length: 51
    Content-Type: text/plain; charset=utf-8

    Query string values: map[city:[Seattle] state:[WA]]%

    $ curl -i 'localhost:8080?city=Seattle&state=WA' -H 'Content-Type: text/plain' -X POST -d "some post data"
    HTTP/1.1 200 OK
    Date: Fri, 02 Sep 2016 16:36:35 GMT
    Content-Length: 79
    Content-Type: text/plain; charset=utf-8

    Query string values: map[city:[Seattle] state:[WA]]
    Body posted: some post data%

    $ curl -i 'localhost:8080?city=Seattle&state=WA' -H 'Content-Type: text/plain' -X PUT
    HTTP/1.1 405 Method Not Allowed
    Date: Fri, 02 Sep 2016 16:36:41 GMT
    Content-Length: 0
    Content-Type: text/plain; charset=utf-8

