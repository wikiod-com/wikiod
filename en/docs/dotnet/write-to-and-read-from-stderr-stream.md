---
title: "Write to and read from StdErr stream"
slug: "write-to-and-read-from-stderr-stream"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Write to standard error output using Console
    var sourceFileName = "NonExistingFile";
    try
    {
        System.IO.File.Copy(sourceFileName, "DestinationFile");
    }
    catch (Exception e)
    {
        var stdErr = Console.Error;
        stdErr.WriteLine($"Failed to copy '{sourceFileName}': {e.Message}");
    }


## Read from standard error of child process
    var errors = new System.Text.StringBuilder();
    var process = new Process
    {
        StartInfo = new ProcessStartInfo
        {
            RedirectStandardError = true,
            FileName = "xcopy.exe",
            Arguments = "\"NonExistingFile\" \"DestinationFile\"",
            UseShellExecute = false
        },
                
    };
    process.ErrorDataReceived += (s, e) => errors.AppendLine(e.Data);
    process.Start();
    process.BeginErrorReadLine();
    process.WaitForExit();

    if (errors.Length > 0) // something went wrong
        System.Console.Error.WriteLine($"Child process error: \r\n {errors}");


