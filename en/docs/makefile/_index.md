---
title : makefile Tutorial
slug : makefile-tutorial
weight : 9917
draft : false
images : []
type : docs
---

A _makefile_ is a text file which controls the operation of the `make` program.  The `make` program is typically used to manage the creation of programs from their source files, but it can be more generally used to handle any process where files (or _targets_) need to be regenerated after other files (or _prerequisites_) have been modified.  The _makefile_ describes the relationship between targets and prerequisites, and also specifies the commands needed to bring the target up-to-date when one or more of the prerequisites has changed.  The only way that `make` determines "out of date-ness" is by comparing the modification time of target files and their prerequisites.

Makefiles are somewhat unique in a few ways which can be confusing initially.

First, a makefile consists of two completely different programming languages in the same file.  The bulk of the file is written in a language that `make` can understand: this provides variable assignment and expansion, some preprocessor capabilities (including other files, conditional parsing of sections of the file, etc.) as well as the definition of targets and their prerequisites.  In addition, each target can have a _recipe_ associated with it which specifies which commands should be invoked to cause that target to be brought up-to-date.  The recipe is written as a shell script (POSIX sh by default).  The `make` program doesn't parse this script: it runs a shell and passes the script to the shell to be run.  The fact that recipes are not parsed by `make`, but instead handled by a separate shell process, is central in understanding makefiles.

Second, a makefile is not a procedural language like a script: as `make` parses the makefile it constructs a _directed graph_ internally where targets are the nodes of the graph and the prerequisite relationships are the edges.  Only after all makefiles have been completely parsed and the graph is complete will `make` choose one node (target) and attempt to bring it up to date.  In order to ensure a target is up to date, it must first ensure that each of that target's prerequisites are up to date, and so on recursively.

