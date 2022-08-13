---
title: "Call Python from C#"
slug: "call-python-from-c"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The documentation provides a sample implementation of the inter-process communication between C# and Python scripts.




Note that in the example above data is serialized using **MongoDB.Bson** library that can be installed via NuGet manager.

Otherwise, you can use any JSON serialization library of your choice.


----------


Below are inter-process communication implementation steps:

 - Input arguments are serialized into JSON string and saved in a temporary text file:
   ```csharp
    BsonDocument argsBson = BsonDocument.Parse("{ 'x' : '1', 'y' : '2' }"); 
    string argsFile = string.Format("{0}\\{1}.txt", Path.GetDirectoryName(pyScriptPath), Guid.NewGuid());
    ```

 - Python interpreter python.exe runs the python script that reads JSON string from a temporary text file and backs-out input arguments:
   ```python
    filename = sys.argv[ 1 ]
    with open( filename ) as data_file:  
       input_args = json.loads( data_file.read() )

    x, y = [ float(input_args.get( key )) for key in [ 'x', 'y' ] ]
   ```

 - Python script is executed and output dictionary is serialized into JSON string and printed to the command window:
   ```python
    print json.dumps( { 'sum' : x + y , 'subtract' : x - y } )
   ```
      [![enter image description here][8]][1]

 - Read output JSON string from C# application:
   ```csharp
    using (StreamReader myStreamReader = process.StandardOutput)
    {
       outputString = myStreamReader.ReadLine();
       process.WaitForExit();
    }
   ```


----------


[![enter image description here][7]][7]

I am using the inter-process communication between C# and Python scripts in one of my projects that allows calling Python scripts directly from Excel spreadsheets.

The project utilizes ExcelDNA add-in for C# - Excel binding.

The source-code is stored in the GitHub [repository][1].


Below are links to wiki pages that provide an overview of the project and help to [get started in 4 easy steps][2].

 - [Getting Started][2]
 - [Implementation Overview][3]
 - [Examples][4]
 - [Object-Wizard][5]
 - [Functions][6]


I hope you find the example and the project useful.


----------


  [1]: https://github.com/JulijJegorov/tandem-algorithms
  [2]: https://github.com/JulijJegorov/tandem-algorithms/wiki/Getting-Started
  [3]: https://github.com/JulijJegorov/tandem-algorithms/wiki/Implementation-Overview
  [4]: https://github.com/JulijJegorov/tandem-algorithms/wiki/Examples
  [5]: https://github.com/JulijJegorov/tandem-algorithms/wiki/Object-Wizard
  [6]: https://github.com/JulijJegorov/tandem-algorithms/wiki/Functions
  [7]: https://i.stack.imgur.com/zDdC1.jpg
  [8]: https://i.stack.imgur.com/HjjT9.png

## Python script to be called by C# application
    import sys
    import json

    # load input arguments from the text file
    filename = sys.argv[ 1 ]
    with open( filename ) as data_file:   
        input_args = json.loads( data_file.read() )

    # cast strings to floats
    x, y = [ float(input_args.get( key )) for key in [ 'x', 'y' ] ]

    print json.dumps( { 'sum' : x + y , 'subtract' : x - y } )


## C# code calling Python script
    using MongoDB.Bson;
    using System;
    using System.Diagnostics;
    using System.IO;

    namespace python_csharp
    {
        class Program
        {
            static void Main(string[] args)
            {
                // full path to .py file
                string pyScriptPath = "...../sum.py";
                // convert input arguments to JSON string
                BsonDocument argsBson = BsonDocument.Parse("{ 'x' : '1', 'y' : '2' }");

                bool saveInputFile = false;
            
                string argsFile = string.Format("{0}\\{1}.txt", Path.GetDirectoryName(pyScriptPath), Guid.NewGuid());

                string outputString = null;
                // create new process start info 
                ProcessStartInfo prcStartInfo = new ProcessStartInfo
                {
                    // full path of the Python interpreter 'python.exe'
                    FileName = "python.exe", // string.Format(@"""{0}""", "python.exe"),
                    UseShellExecute = false,
                    RedirectStandardOutput = true,
                    CreateNoWindow = false
                };

                try
                {    
                    // write input arguments to .txt file 
                    using (StreamWriter sw = new StreamWriter(argsFile))
                    {
                        sw.WriteLine(argsBson);
                        prcStartInfo.Arguments = string.Format("{0} {1}", string.Format(@"""{0}""", pyScriptPath), string.Format(@"""{0}""", argsFile));
                    }
                    // start process
                    using (Process process = Process.Start(prcStartInfo))
                    {
                        // read standard output JSON string
                        using (StreamReader myStreamReader = process.StandardOutput)
                        {
                            outputString = myStreamReader.ReadLine();
                            process.WaitForExit();
                        }
                    }
                }
                finally
                {
                    // delete/save temporary .txt file 
                    if (!saveInputFile)
                    {
                        File.Delete(argsFile);
                    }
                }
                Console.WriteLine(outputString);
            }
        }
    }


