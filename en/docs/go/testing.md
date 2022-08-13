---
title: "Testing"
slug: "testing"
draft: false
images: []
weight: 9896
type: docs
toc: true
---

Go comes with its own testing facilities that has everything needed to run tests and benchmarks. Unlike in most other programming languages, there is often no need for a separate testing framework, although some exist.

## Basic Test
`main.go`:
    
    package main

    import (
        "fmt"
    )
    
    func main() {
        fmt.Println(Sum(4,5))
    }
    
    func Sum(a, b int) int {
        return a + b
    }

`main_test.go`:

    package main

    import (
        "testing"
    )

    // Test methods start with `Test`
    func TestSum(t *testing.T) {
        got := Sum(1, 2)
        want := 3
        if got != want {
            t.Errorf("Sum(1, 2) == %d, want %d", got, want)
        }
    }

 
To run the test just use the `go test` command:

    $ go test
    ok      test_app    0.005s

Use the `-v` flag to see the results of each test:

    $ go test -v
    === RUN   TestSum
    --- PASS: TestSum (0.00s)
    PASS
    ok      _/tmp    0.000s

Use the path `./...` to test subdirectories recursively:

    $ go test -v ./...
    ok      github.com/me/project/dir1    0.008s
    === RUN   TestSum
    --- PASS: TestSum (0.00s)
    PASS
    ok      github.com/me/project/dir2    0.008s
    === RUN   TestDiff
    --- PASS: TestDiff (0.00s)
    PASS

**Run a Particular Test:**<br>
If there are multiple tests and you want to run a specific test, it can be done like this:<br>

    go test -v -run=<TestName> // will execute only test with this name

Example:<br>

    go test -v run=TestSum

## Table-driven unit tests
This type of testing is popular technique for testing with predefined input and output values.

Create a file called `main.go` with content:
    
    package main

    import (
        "fmt"
    )
    
    func main() {
        fmt.Println(Sum(4, 5))
    }
    
    func Sum(a, b int) int {
        return a + b
    }

After you run it with, you will see that the output is `9`. Although the `Sum` function looks pretty simple, it is a good idea to test your code. In order to do this, we create another file named `main_test.go` in the same folder as `main.go`, containing the following code:

    package main
    
    import (
        "testing"
    )
    
    // Test methods start with Test
    func TestSum(t *testing.T) {
        // Note that the data variable is of type array of anonymous struct,
        // which is very handy for writing table-driven unit tests.
        data := []struct {
            a, b, res int
        }{
            {1, 2, 3},
            {0, 0, 0},
            {1, -1, 0},
            {2, 3, 5},
            {1000, 234, 1234},
        }
    
        for _, d := range data {
            if got := Sum(d.a, d.b); got != d.res {
                t.Errorf("Sum(%d, %d) == %d, want %d", d.a, d.b, got, d.res)
            }
        }
    }

As you can see, a slice of anonymous structs is created, each with a set of inputs and the expected result. This allows a large number of test cases to be created all together in one place, then executed in a loop, reducing code repetition and improving clarity.

## Testing using setUp and tearDown function
You can set a setUp and tearDown function.

 - A setUp function prepares your environment to tests.
 - A tearDown function does a rollback.

This is a good option when you can't modify your database and you need to create an object that simulate an object brought of database or need to init a configuration in each test.

A stupid example would be:

    // Standard numbers map
    var numbers map[string]int = map[string]int{"zero": 0, "three": 3}
    
    // TestMain will exec each test, one by one
    func TestMain(m *testing.M) {
        // exec setUp function
        setUp("one", 1)
        // exec test and this returns an exit code to pass to os
        retCode := m.Run()
        // exec tearDown function
        tearDown("one")
        // If exit code is distinct of zero,
        // the test will be failed (red)
        os.Exit(retCode)
    }
    
    // setUp function, add a number to numbers slice
    func setUp(key string, value int) {
        numbers[key] = value
    }
    
    // tearDown function, delete a number to numbers slice
    func tearDown(key string) {
        delete(numbers, key)
    }
    
    // First test
    func TestOnePlusOne(t *testing.T) {
        numbers["one"] = numbers["one"] + 1
    
        if numbers["one"] != 2 {
            t.Error("1 plus 1 = 2, not %v", value)
        }
    }
    
    // Second test
    func TestOnePlusTwo(t *testing.T) {
        numbers["one"] = numbers["one"] + 2
    
        if numbers["one"] != 3 {
            t.Error("1 plus 2 = 3, not %v", value)
        }
    }

Other example would be to prepare database to test and to do rollback

     // ID of Person will be saved in database
    personID := 12345
    // Name of Person will be saved in database
    personName := "Toni"
    
    func TestMain(m *testing.M) {
        // You create an Person and you save in database
        setUp(&Person{
                ID:   personID,
                Name: personName,
                Age:  19,
            })
        retCode := m.Run()
        // When you have executed the test, the Person is deleted from database
        tearDown(personID)
        os.Exit(retCode)
    }
    
    func setUp(P *Person) {
        // ...
        db.add(P)
        // ...
    }
    
    func tearDown(id int) {
        // ...
        db.delete(id)
        // ...
    }
    
    func getPerson(t *testing.T) {
        P := Get(personID)
        
        if P.Name != personName {
            t.Error("P.Name is %s and it must be Toni", P.Name)
        }
    }

## Benchmark tests
If you want to measure benchmarks add a testing method like this:

`sum.go`:

    package sum
    
    // Sum calculates the sum of two integers
    func Sum(a, b int) int {
        return a + b
    }

`sum_test.go`:

    package sum

    import "testing"

    func BenchmarkSum(b *testing.B) {
        for i := 0; i < b.N; i++ {
            _ = Sum(2, 3)
        }
    }

Then in order to run a simple benchmark:

    $ go test -bench=. 
    BenchmarkSum-8    2000000000             0.49 ns/op
    ok      so/sum    1.027s



## Example tests (self documenting tests)
This type of tests make sure that your code compiles properly and will appear in the generated documentation for your project. In addition to that, the example tests can assert that your test produces proper output.

`sum.go`:

    package sum

    // Sum calculates the sum of two integers
    func Sum(a, b int) int {
        return a + b
    }

`sum_test.go`:

    package sum
    
    import "fmt"
    
    func ExampleSum() {
        x := Sum(1, 2)
        fmt.Println(x)
        fmt.Println(Sum(-1, -1))
        fmt.Println(Sum(0, 0))

        // Output:
        // 3
        // -2
        // 0
    }

To execute your test, run `go test` in the folder containing those files OR put those two files in a sub-folder named `sum` and then from the parent folder execute `go test ./sum`. In both cases you will get an output similar to this:

    ok      so/sum    0.005s

If you are wondering how this is testing your code, here is another example function, which actually fails the test:

    func ExampleSum_fail() {
        x := Sum(1, 2)
        fmt.Println(x)
    
        // Output:
        // 5
    }

When you run `go test`, you get the following output:

    $ go test
    --- FAIL: ExampleSum_fail (0.00s)
    got:
    3
    want:
    5
    FAIL
    exit status 1
    FAIL    so/sum    0.006s


If you want to see the documentation for your `sum` package – just run:

    go doc -http=:6060

and navigate to http://localhost:6060/pkg/FOLDER/sum/, where _FOLDER_ is the folder containing the `sum` package (in this example `so`). The documentation for the sum method looks like this:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/GNHv4.png

## Testing HTTP requests
main.go:

    package main
    
    import (
        "fmt"
        "io/ioutil"
        "log"
        "net/http"
    )
    
    func fetchContent(url string) (string, error) {
        res, err := http.Get(url)
        if err != nil {
            return "", nil
        }
        defer res.Body.Close()
    
        body, err := ioutil.ReadAll(res.Body)
        if err != nil {
            return "", err
        }
        return string(body), nil
    }
    
    func main() {
        url := "https://example.com/"
        content, err := fetchContent(url)
        if err != nil {
            log.Fatal(err)
        }
        fmt.Println("Content:", content)
    }


main_test.go:

    package main
    
    import (
        "fmt"
        "net/http"
        "net/http/httptest"
        "testing"
    )
    
    func Test_fetchContent(t *testing.T) {
        ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            fmt.Fprint(w, "hello world")
        }))
        defer ts.Close()
    
        content, err := fetchContent(ts.URL)
        if err != nil {
            t.Error(err)
        }
    
        want := "hello world"
        if content != want {
            t.Errorf("Got %q, want %q", content, want)
        }
    }



## Set/Reset Mock Function In Tests
This example shows how to mock out a function call that is irrelevant to our unit test, and then use the `defer` statement to re-assign the mocked function call back to its original function.

```
var validate = validateDTD

// ParseXML parses b for XML elements and values, and returns them as a map of 
// string key/value pairs.
func ParseXML(b []byte) (map[string]string, error) {
    // we don't care about validating against DTD in our unit test
    if err := validate(b); err != nil {
        return err
    }

    // code to parse b etc.
}

func validateDTD(b []byte) error {
    // get the DTD from some external storage, use it to validate b etc.
}

```
In our unit test,
```
func TestParseXML(t *testing.T) {
    // assign the original validate function to a variable.
    originalValidate = validate
    // use the mockValidate function in this test.
    validate = mockValidate
    // defer the re-assignment back to the original validate function.
    defer func() {
       validate = originalValidate
    }()

    var input []byte
    actual, err := ParseXML(input)
    // assertion etc.
}

func mockValidate(b []byte) error {
    return nil // always return nil since we don't care
}
```

## View code coverage in HTML format 
Run `go test` as normal, yet with the `coverprofile` flag. Then use `go tool` to view the results as HTML.

```
    go test -coverprofile=c.out
    go tool cover -html=c.out
```

