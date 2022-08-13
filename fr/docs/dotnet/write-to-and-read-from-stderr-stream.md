---
title: "Écrire et lire à partir du flux StdErr"
slug: "ecrire-et-lire-a-partir-du-flux-stderr"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Écrire sur la sortie d'erreur standard à l'aide de la console
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


## Lire à partir de l'erreur standard du processus enfant
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


