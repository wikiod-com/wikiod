---
title: "Escribir y leer desde el flujo StdErr"
slug: "escribir-y-leer-desde-el-flujo-stderr"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Escribir en la salida de error estándar usando Consola
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


## Leer del error estándar del proceso hijo
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


