---
title: "Inline Expansion"
slug: "inline-expansion"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Inline expansion is a common optimization in compiled code that prioritized performance over binary size. It lets the compiler replace a function call with the actual body of the function; effectively copy/pasting code from one place to another at compile time. Since the call site is expanded to just contain the machine instructions that the compiler generated for the function, we don't have to perform a CALL or PUSH (the x86 equivalant of a GOTO statement or a stack frame push) or their equivalant on other architectures.

The inliner makes decisions about whether or not to inline a function based on a number of heuristics, but in general Go inlines by default. Because the inliner gets rid of function calls, it effectively gets to decide where the scheduler is allowed to preempt a goroutine.

Function calls will not be inlined if any of the following are true (there are many other reasons too, this list is incomplete):

  - Functions are variadic (eg. they have `...` args)
  - Functions have a "max hairyness" greater than the budget (they recurse too much or can't be analyzed for some other reason)
  - They contain `panic`, `recover`, or `defer`

## Disabling inline expansion
Inline expansion can be disabled with the `go:noinline` pragma. For example, if we build the following simple program:

    package main
     
    func printhello() {
        println("Hello")
    }
     
    func main() {
        printhello()
    }

we get output that looks like this (trimmed for readability):

    $ go version
    go version go1.6.2 linux/amd64
    $ go build main.go
    $ ./main
    Hello
    $ go tool objdump main
    TEXT main.main(SB) /home/sam/main.go
            main.go:7       0x401000        64488b0c25f8ffffff      FS MOVQ FS:0xfffffff8, CX
            main.go:7       0x401009        483b6110                CMPQ 0x10(CX), SP
            main.go:7       0x40100d        7631                    JBE 0x401040
            main.go:7       0x40100f        4883ec10                SUBQ $0x10, SP
            main.go:8       0x401013        e8281f0200              CALL runtime.printlock(SB)
            main.go:8       0x401018        488d1d01130700          LEAQ 0x71301(IP), BX
            main.go:8       0x40101f        48891c24                MOVQ BX, 0(SP)
            main.go:8       0x401023        48c744240805000000      MOVQ $0x5, 0x8(SP)
            main.go:8       0x40102c        e81f290200              CALL runtime.printstring(SB)
            main.go:8       0x401031        e89a210200              CALL runtime.printnl(SB)
            main.go:8       0x401036        e8851f0200              CALL runtime.printunlock(SB)
            main.go:9       0x40103b        4883c410                ADDQ $0x10, SP
            main.go:9       0x40103f        c3                      RET
            main.go:7       0x401040        e87b9f0400              CALL runtime.morestack_noctxt(SB)
            main.go:7       0x401045        ebb9                    JMP main.main(SB)
            main.go:7       0x401047        cc                      INT $0x3
            main.go:7       0x401048        cc                      INT $0x3
            main.go:7       0x401049        cc                      INT $0x3
            main.go:7       0x40104a        cc                      INT $0x3
            main.go:7       0x40104b        cc                      INT $0x3
            main.go:7       0x40104c        cc                      INT $0x3
            main.go:7       0x40104d        cc                      INT $0x3
            main.go:7       0x40104e        cc                      INT $0x3
            main.go:7       0x40104f        cc                      INT $0x3
    …

note that there is no `CALL` to `printhello`. However, if we then build the program with the pragma in place:

    package main
     
    //go:noinline
    func printhello() {
        println("Hello")
    }
     
    func main() {
        printhello()
    }

The output contains the printhello function and a `CALL main.printhello`:

    $ go version
    go version go1.6.2 linux/amd64
    $ go build main.go
    $ ./main
    Hello
    $ go tool objdump main
    TEXT main.printhello(SB) /home/sam/main.go
            main.go:4       0x401000        64488b0c25f8ffffff      FS MOVQ FS:0xfffffff8, CX
            main.go:4       0x401009        483b6110                CMPQ 0x10(CX), SP
            main.go:4       0x40100d        7631                    JBE 0x401040
            main.go:4       0x40100f        4883ec10                SUBQ $0x10, SP
            main.go:5       0x401013        e8481f0200              CALL runtime.printlock(SB)
            main.go:5       0x401018        488d1d01130700          LEAQ 0x71301(IP), BX
            main.go:5       0x40101f        48891c24                MOVQ BX, 0(SP)
            main.go:5       0x401023        48c744240805000000      MOVQ $0x5, 0x8(SP)
            main.go:5       0x40102c        e83f290200              CALL runtime.printstring(SB)
            main.go:5       0x401031        e8ba210200              CALL runtime.printnl(SB)
            main.go:5       0x401036        e8a51f0200              CALL runtime.printunlock(SB)
            main.go:6       0x40103b        4883c410                ADDQ $0x10, SP
            main.go:6       0x40103f        c3                      RET
            main.go:4       0x401040        e89b9f0400              CALL runtime.morestack_noctxt(SB)
            main.go:4       0x401045        ebb9                    JMP main.printhello(SB)
            main.go:4       0x401047        cc                      INT $0x3
            main.go:4       0x401048        cc                      INT $0x3
            main.go:4       0x401049        cc                      INT $0x3
            main.go:4       0x40104a        cc                      INT $0x3
            main.go:4       0x40104b        cc                      INT $0x3
            main.go:4       0x40104c        cc                      INT $0x3
            main.go:4       0x40104d        cc                      INT $0x3
            main.go:4       0x40104e        cc                      INT $0x3
            main.go:4       0x40104f        cc                      INT $0x3
     
    TEXT main.main(SB) /home/sam/main.go
            main.go:8       0x401050        64488b0c25f8ffffff      FS MOVQ FS:0xfffffff8, CX
            main.go:8       0x401059        483b6110                CMPQ 0x10(CX), SP
            main.go:8       0x40105d        7606                    JBE 0x401065
            main.go:9       0x40105f        e89cffffff              CALL main.printhello(SB)
            main.go:10      0x401064        c3                      RET
            main.go:8       0x401065        e8769f0400              CALL runtime.morestack_noctxt(SB)
            main.go:8       0x40106a        ebe4                    JMP main.main(SB)
            main.go:8       0x40106c        cc                      INT $0x3
            main.go:8       0x40106d        cc                      INT $0x3
            main.go:8       0x40106e        cc                      INT $0x3
            main.go:8       0x40106f        cc                      INT $0x3
    …

