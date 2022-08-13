---
title: "Console IO"
slug: "console-io"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Read input from console
Using `scanf`

> Scanf scans text read from standard input, storing successive space-separated values into successive arguments as determined by the format. It returns the number of items successfully scanned. If that is less than the number of arguments, err will report why. Newlines in the input must match newlines in the format. The one exception: the verb %c always scans the next rune in the input, even if it is a space (or tab etc.) or newline.

     # Read integer 
     var i int
     fmt.Scanf("%d", &i)

     # Read string 
     var str string
     fmt.Scanf("%s", &str)

Using `scan`

> Scan scans text read from standard input, storing successive space-separated values into successive arguments. Newlines count as space. It returns the number of items successfully scanned. If that is less than the number of arguments, err will report why.

     # Read integer 
     var i int
     fmt.Scan(&i)

     # Read string 
     var str string
     fmt.Scan(&str)
    
Using `scanln`

> Sscanln is similar to Sscan, but stops scanning at a newline and after the final item there must be a newline or EOF.

    # Read string
    var input string
    fmt.Scanln(&input)

Using `bufio`

    # Read using Reader
    reader := bufio.NewReader(os.Stdin)
    text, err := reader.ReadString('\n')

    # Read using Scanner
    scanner := bufio.NewScanner(os.Stdin)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }





