---
title: "StdErr akışına yazma ve okuma"
slug: "stderr-aksna-yazma-ve-okuma"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Konsolu kullanarak standart hata çıktısına yaz
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


## Alt sürecin standart hatasından okuma
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


