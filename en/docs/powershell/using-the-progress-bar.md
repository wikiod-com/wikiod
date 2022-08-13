---
title: "Using the progress bar"
slug: "using-the-progress-bar"
draft: false
images: []
weight: 9904
type: docs
toc: true
---

A progress bar can be used to show something is in a process.It is a time-saving and slick feature one should have. Progress bars are incredibly useful while debugging to figure out which part of the script is executing, and they’re satisfying for the people running scripts to track what’s happening. It is common to display some kind of progress when a script takes a long time to complete. When a user launches the script and nothing happens, one begins to wonder if the script launched correctly.

## Simple use of progress bar
    1..100 | ForEach-Object {
            Write-Progress -Activity "Copying files" -Status "$_ %" -Id 1 -PercentComplete $_ -CurrentOperation "Copying file file_name_$_.txt"
            Start-Sleep -Milliseconds 500    # sleep simulates working code, replace this line with your executive code (i.e. file copying)
        }

*Please note that for brevity this example does not contain any executive code (simulated with `Start-Sleep`). However it is possible to run it directly as is and than modify and play with it.*

This is how result looks in PS console:
[![Powershell console single progress bar][1]][1]

This is how result looks in PS ISE:
[![Powershell ISE single progress bar][2]][2]


  [1]: http://i.stack.imgur.com/7vWJK.png
  [2]: http://i.stack.imgur.com/58sB0.png

## Usage of inner progress bar
    1..10 | foreach-object {
            $fileName = "file_name_$_.txt"
            Write-Progress -Activity "Copying files" -Status "$($_*10) %" -Id 1 -PercentComplete ($_*10) -CurrentOperation "Copying file $fileName"
                
            1..100 | foreach-object {
                Write-Progress -Activity "Copying contents of the file $fileName" -Status "$_ %" -Id 2 -ParentId 1 -PercentComplete $_ -CurrentOperation "Copying $_. line"
                
                Start-Sleep -Milliseconds 20 # sleep simulates working code, replace this line with your executive code (i.e. file copying)
            }
    
            Start-Sleep -Milliseconds 500 # sleep simulates working code, replace this line with your executive code (i.e. file search)
    
       }

*Please note that for brevity this example does not contain any executive code (simulated with `Start-Sleep`). However it is possible to run it directly as is and than modify and play with it.*

This is how result looks in PS console:
[![Powershell console inner progress bar][1]][1]

This is how result looks in PS ISE:
[![Powershell ISE inner progress bar][2]][2]




  [1]: http://i.stack.imgur.com/URYAL.png
  [2]: http://i.stack.imgur.com/k8bEr.png

