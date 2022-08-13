---
title: "Build Constraints"
slug: "build-constraints"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Syntax
- // +build tags

Build tags are used for conditionally building certain files in your code. Build tags may ignore files that you don't want build unless explicitly included, or some predefined build tags may be used to have a file only be built on a particular architecture or operating system.

Build tags may appear in any kind of source file (not just Go), but they must appear near the top of the file, preceded only by blank lines and other line comments. These rules mean that in Go files a build constraint must appear before the package clause.

A series of build tags must be followed by a blank line.

## Separate integration tests
Build constraints are commonly used to separate normal unit tests from integration tests that require external resources, like a database or network access. To do this, add a custom build constraint to the top of the test file:

    // +build integration
     
    package main
     
    import (
        "testing"
    )
     
    func TestThatRequiresNetworkAccess(t *testing.T) {
        t.Fatal("It failed!")
    }

The test file will not compile into the build executable unless the following invocation of `go test` is used:

    go test -tags "integration"

Results:

    $ go test
    ?       bitbucket.org/yourname/yourproject    [no test files]
    $ go test -tags "integration"
    --- FAIL: TestThatRequiresNetworkAccess (0.00s)
            main_test.go:10: It failed!
    FAIL
    exit status 1
    FAIL    bitbucket.org/yourname/yourproject    0.003s

## Optimize implementations based on architecture
We can optimize a simple xor function for only architectures that support unaligned reads/writes by creating two files that define the function and prefixing them with a build constraint (for an actual example of the xor code which is out of scope here, see `crypto/cipher/xor.go` in the standard library):

    // +build 386 amd64 s390x
    
    package cipher
    
    func xorBytes(dst, a, b []byte) int { /* This function uses unaligned reads / writes to optimize the operation */ }

and for other architectures:

    // +build !386,!amd64,!s390x
    
    package cipher
    
    func xorBytes(dst, a, b []byte) int { /* This version of the function just loops and xors */ }

