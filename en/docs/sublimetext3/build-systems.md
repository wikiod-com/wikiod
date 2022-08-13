---
title: "Build Systems"
slug: "build-systems"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Syntax
- Build Systems are written in JSON, and have the file extension `.sublime-build`.

Build Systems are output-only, meaning it is not possible to execute some code that will request user input using the build system. The code will just block waiting for input forever. To work around this, many people use a REPL plugin.

----

JSON keys:
 - `shell_cmd` specifies the exact command to run in the shell, and has support for variable placeholders (like `${file}` that refers to the currently open file).
 - `result_file_regex` and `result_line_regex` are used to parse the output from a failed build and show the errors in such a way that it is possible to navigate the editor to where an error occurred.
 - `selector` is a scope selector that defines what syntax the build is relevant for. There's no point running a Python interpreter on PHP code, for example.
 - `variants` can be used to allow a different command to be run, for example to just check the syntax of the file rather than execute it. The variants can also specify or override the same JSON keys, and they will apply only to that variant.
 - `syntax` is used to set the syntax definition file which will be applied to the output panel, thus giving it syntax highlighting.

## Example Build System
New build systems can be created from the menu (Tools | Build System | New Build System).

    {
        "shell_cmd": "somecommand -u \"$file\"",
        "result_file_regex": "^[ ]*File \"(.*?)\"",
        "result_line_regex": "^[ ]*File \".*?\", line ([0-9]*)",

        "selector": "text.html",
        "syntax": "Packages/JavaScript/JSON.sublime-syntax",

        "env": {"ENCODING": "utf-8"},
        "working_dir": "${project_path:${folder}}",
        "path": "C:\\test\\;$PATH"

        "linux": {
            "variants":
            [
                {
                    "name": "Word Count (current file)",
                    "cmd": ["wc", "$file"]
                }
            ]
        }
    }

A build can be initiated by using the menu (Tools | Build) or by pressing <kbd>Ctrl+B</kbd>.

