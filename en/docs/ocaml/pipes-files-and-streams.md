---
title: "Pipes, Files, and Streams"
slug: "pipes-files-and-streams"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Read from Standard Input and Print to Standard Output
We prepare a file called `reverser.ml` with the following contents:

```
let acc = ref [] in
    try
        while true do
            acc := read_line () :: !acc;
        done
    with
        End_of_file -> print_string (String.concat "\n" !acc)

```

We then compile our program using the following command:

```
$ ocamlc -o reverser.byte reverser.ml
```

We test it out by piping data to our new executable:

```
$ cat data.txt
one
two
three
$ ./reverser.byte < data.txt
three
two
one
```

The `reserver.ml` program is written in an imperative style.  While imperative style is fine, it is interesting to compare this to the functional translation:

```
let maybe_read_line () =
  try Some(read_line())
  with End_of_file -> None

let rec loop acc =
  match maybe_read_line () with
  | Some(line) -> loop (line :: acc)
  | None -> List.iter print_endline acc

let () = loop []
```

Thanks to introducing the function `maybe_read_line` the control flow is much simpler in this second version than in the first.

