---
title: "cgo"
slug: "cgo"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Calling C Function From Go
Cgo enables the creation of Go packages that call C code.  
To use `cgo` write normal Go code that imports a pseudo-package "C". The Go code can then refer to types such as `C.int`, or functions such as `C.Add`.  
The import of "C" is immediately preceded by a comment, that comment, called the preamble, is used as a header when compiling the C parts of the package.  
Note that there must be no blank lines in between the `cgo` comment and the import statement.  
Note that `import "C"` can not  grouped with other imports into a parenthesized, "factored" import statement. You must write multiple import statements, like:

    import "C"
    import "fmt"

And it is good style to use the factored import statement, for other imports, like:

    import "C"
    import (
        "fmt"
        "math"
    )

Simple example using `cgo`:

    package main
    
    //int Add(int a, int b){
    //    return a+b;
    //}
    import "C"
    import "fmt"
    
    func main() {
        a := C.int(10)
        b := C.int(20)
        c := C.Add(a, b)
        fmt.Println(c) // 30
    }
Then `go build`, and run it, output:  

    30

To build `cgo` packages, just use `go build` or `go install` as usual. The `go tool` recognizes the special `"C"` import and automatically uses `cgo` for those files.


## Wire C and Go code in all directions
**Calling C code from Go**


```
package main

/*
// Everything in comments above the import "C" is C code and will be compiles with the GCC. 
// Make sure you have a GCC installed.

int addInC(int a, int b) {
    return a + b;
}
 */
import "C"
import "fmt"

func main() {
       a := 3
       b := 5
       
       c := C.addInC(C.int(a), C.int(b))

       fmt.Println("Add in C:", a, "+", b, "=", int(c))
}
```

**Calling Go code from C**

```
package main

/*
static inline int multiplyInGo(int a, int b) {
    return go_multiply(a, b);
}
 */
import "C"
import (
       "fmt"
)

func main() {
       a := 3
       b := 5
       
       c := C.multiplyInGo(C.int(a), C.int(b))

       fmt.Println("multiplyInGo:", a, "*", b, "=", int(c))
}

//export go_multiply
func go_multiply(a C.int, b C.int) C.int {
       return a * b
}
````

Dealing with Function pointers

```
package main

/*
int go_multiply(int a, int b);

typedef int (*multiply_f)(int a, int b);
multiply_f multiply;

static inline init() {
    multiply = go_multiply;
}

static inline int multiplyWithFp(int a, int b) {
    return multiply(a, b);
}
 */
import "C"
import (
       "fmt"
)

func main() {
       a := 3
       b := 5
       C.init(); // OR:
       C.multiply = C.multiply_f(go_multiply);

       c := C.multiplyWithFp(C.int(a), C.int(b))

       fmt.Println("multiplyInGo:", a, "+", b, "=", int(c))
}

//export go_multiply
func go_multiply(a C.int, b C.int) C.int {
       return a * b
}
```

**Convert Types, Access Structs and Pointer Arithmetic**

From the official Go documentation:
```
// Go string to C string
// The C string is allocated in the C heap using malloc.
// It is the caller's responsibility to arrange for it to be
// freed, such as by calling C.free (be sure to include stdlib.h
// if C.free is needed).
func C.CString(string) *C.char

// Go []byte slice to C array
// The C array is allocated in the C heap using malloc.
// It is the caller's responsibility to arrange for it to be
// freed, such as by calling C.free (be sure to include stdlib.h
// if C.free is needed).
func C.CBytes([]byte) unsafe.Pointer

// C string to Go string
func C.GoString(*C.char) string

// C data with explicit length to Go string
func C.GoStringN(*C.char, C.int) string

// C data with explicit length to Go []byte
func C.GoBytes(unsafe.Pointer, C.int) []byte
```

How to use it:
```
func go_handleData(data *C.uint8_t, length C.uint8_t) []byte {
       return C.GoBytes(unsafe.Pointer(data), C.int(length))
}

// ...

goByteSlice := []byte {1, 2, 3}
goUnsafePointer := C.CBytes(goByteSlice)
cPointer := (*C.uint8_t)(goUnsafePointer)

// ...

func getPayload(packet *C.packet_t) []byte {
       dataPtr := unsafe.Pointer(packet.data)
       // Lets assume a 2 byte header before the payload.
       payload := C.GoBytes(unsafe.Pointer(uintptr(dataPtr)+2), C.int(packet.dataLength-2))
       return payload
}
```

