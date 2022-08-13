---
title: "Profiling using go tool pprof"
slug: "profiling-using-go-tool-pprof"
draft: false
images: []
weight: 9852
type: docs
toc: true
---

For more in profiling go programs visit the [go blog](https://blog.golang.org/profiling-go-programs).

## Basic cpu and memory profiling
Add the following code in you main program.
```
var cpuprofile = flag.String("cpuprofile", "", "write cpu profile `file`")
var memprofile = flag.String("memprofile", "", "write memory profile to `file`")

func main() {
    flag.Parse()
    if *cpuprofile != "" {
        f, err := os.Create(*cpuprofile)
        if err != nil {
            log.Fatal("could not create CPU profile: ", err)
        }
        if err := pprof.StartCPUProfile(f); err != nil {
            log.Fatal("could not start CPU profile: ", err)
        }
        defer pprof.StopCPUProfile()
    }
    ...
    if *memprofile != "" {
        f, err := os.Create(*memprofile)
        if err != nil {
            log.Fatal("could not create memory profile: ", err)
        }
        runtime.GC() // get up-to-date statistics
        if err := pprof.WriteHeapProfile(f); err != nil {
            log.Fatal("could not write memory profile: ", err)
        }
        f.Close()
    }
}
```

after that **build** the go program if added in main `go build main.go`. Run main program with flags defined in code `main.exe -cpuprofile cpu.prof -memprof mem.prof`. If the profiling is done for test cases run the tests with same flags `go test -cpuprofile cpu.prof -memprofile mem.prof`


## Using Benchmarks to Create Profile
For a non-main packages as well as main, **instead of adding flags inside the code**, write **benchmarks** in the test package , for example:

    func BenchmarkHello(b *testing.B) {
        for i := 0; i < b.N; i++ {
            fmt.Sprintf("hello")
        }
    }

Then run the test with the profile flag 

> go test -cpuprofile cpu.prof -bench=.

And the benchmarks will be run and create a prof file with filename cpu.prof (in the above example).

## Basic memory Profiling
```
var memprofile = flag.String("memprofile", "", "write memory profile to `file`")

func main() {
    flag.Parse()
    if *memprofile != "" {
        f, err := os.Create(*memprofile)
        if err != nil {
            log.Fatal("could not create memory profile: ", err)
        }
        runtime.GC() // get up-to-date statistics
        if err := pprof.WriteHeapProfile(f); err != nil {
            log.Fatal("could not write memory profile: ", err)
        }
        f.Close()
    }
}
```
```
go build main.go
main.exe -memprofile mem.prof
go tool pprof main.exe mem.prof

```

## Set CPU/Block profile rate
```
// Sets the CPU profiling rate to hz samples per second
// If hz <= 0, SetCPUProfileRate turns off profiling
runtime.SetCPUProfileRate(hz) 

// Controls the fraction of goroutine blocking events that are reported in the blocking profile
// Rate = 1 includes every blocking event in the profile
// Rate <= 0 turns off profiling
runtime.SetBlockProfileRate(rate)
```

## Accessing Profile File
once a prof file has been generated, one can access the prof file using **go tools**:

> go tool pprof cpu.prof 

This will enter into a command line interface for exploring the `profile`

Common commands include:

    (pprof) top

lists top processes in memory

    (pprof) peek

Lists all processes, use *regex* to narrow search.

    (pprof) web

Opens an graph (in svg format) of the process.


An example of the `top` command:

    69.29s of 100.84s total (68.71%)
    Dropped 176 nodes (cum <= 0.50s)
    Showing top 10 nodes out of 73 (cum >= 12.03s)
          flat  flat%   sum%        cum   cum%
        12.44s 12.34% 12.34%     27.87s 27.64%  runtime.mapaccess1
        10.94s 10.85% 23.19%     10.94s 10.85%  runtime.duffcopy
         9.45s  9.37% 32.56%     54.61s 54.16%  github.com/tester/test.(*Circle).Draw
         8.88s  8.81% 41.36%      8.88s  8.81%  runtime.aeshashbody
         7.90s  7.83% 49.20%     11.04s 10.95%  runtime.mapaccess1_fast64
         5.86s  5.81% 55.01%      9.59s  9.51%  github.com/tester/test.(*Circle).isCircle
         5.03s  4.99% 60.00%      8.89s  8.82%  github.com/tester/test.(*Circle).openCircle
         3.14s  3.11% 63.11%      3.14s  3.11%  runtime.aeshash64
         3.08s  3.05% 66.16%      7.85s  7.78%  runtime.mallocgc
         2.57s  2.55% 68.71%     12.03s 11.93%  runtime.memhash


