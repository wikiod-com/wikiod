---
title: "Blaming"
slug: "blaming"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Syntax
 - git blame [filename]
 - git blame [-f][-e][-w] [filename]
 - git blame [-L range] [filename]

## Parameters
| Parameter | Details |
| ------ | ------ |
| filename   | Name of the file for which details need to be checked   |
|-f|Show the file name in the origin commit|
|-e|Show the author email instead of author name|
|-w|Ignore white spaces while making a comparison between child and parent's version |
|-L start,end|Show only the given line range Example: `git blame -L 1,2 [filename]`|
| --show-stats | Shows additional statistics at end of blame output |
| -l | Show long rev (Default: off) |
| -t | Show raw timestamp (Default: off) |
| -reverse | Walk history forward instead of backward |
| -p, --porcelain | Output for machine consumption |
| -M | Detect moved or copied lines within a file |
| -C | In addition to -M, detect lines moved or copied from other files that were modified in the same commit |
| -h | Show the help message |
| -c | Use the same output mode as git-annotate (Default: off) |
| -n | Show the line number in the original commit (Default: off) |


The git blame command is very useful when it comes to know who has made changes to a file on a per line base.


## Only show certain lines


## Show the commit that last modified a line


## Ignore whitespace-only changes


## To find out who changed a file
    // Shows the author and commit per line of specified file
    git blame test.c 
    
    // Shows the author email and commit per line of specified 
    git blame -e test.c file
    
    // Limits the selection of lines by specified range
    git blame -L 1,10 test.c 

