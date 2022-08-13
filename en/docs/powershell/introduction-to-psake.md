---
title: "Introduction to Psake"
slug: "introduction-to-psake"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
 - Task - main function to execute a step of your build script 
 - Depends - property that specify what the current step depends upon
 - default - there must always be a default task that will get executed if no initial task is specified
 - FormatTaskName - specifies how each step is displayed in the result window.

[psake][1] is a build automation tool written in PowerShell, and is inspired by Rake (Ruby make) and Bake (Boo make).  It is used to create builds using dependency pattern. 
Documentation available [here][2]


  [1]: https://github.com/psake/psake
  [2]: http://psake.readthedocs.io/en/latest/

## Basic outline

    Task Rebuild -Depends Clean, Build  {
       "Rebuild"
     }
    
    Task Build {
       "Build"
     }
    
    Task Clean {
       "Clean"
     }

    Task default -Depends Build

## FormatTaskName example
    # Will display task as:
    # -------- Rebuild --------
    # -------- Build --------
    FormatTaskName "-------- {0} --------"  
    
    # will display tasks in yellow colour:
    # Running Rebuild  
    FormatTaskName {
        param($taskName)
        "Running $taskName" - foregroundcolor yellow
    }

    Task Rebuild -Depends Clean, Build  {
       "Rebuild"
     }
    
    Task Build {
       "Build"
     }
    
    Task Clean {
       "Clean"
     }
    
    Task default -Depends Build

## Run Task conditionally
    propreties { 
        $isOk = $false
    }
    
    # By default the Build task won't run, unless there is a param $true
    Task Build -precondition { return $isOk } {
       "Build"
     }
    
    Task Clean {
       "Clean"
     }
    
    Task default -Depends Build

## ContinueOnError
    Task Build -depends Clean {
       "Build"
     }
    
    Task Clean -ContinueOnError {
       "Clean"
        throw "throw on purpose, but the task will continue to run"
     }
    
    Task default -Depends Build

