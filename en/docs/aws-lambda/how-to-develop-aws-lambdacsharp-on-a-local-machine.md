---
title: "How to develop aws-lambda(C#) on a local machine"
slug: "how-to-develop-aws-lambdac-on-a-local-machine"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

As part of the [Serverless][1] movement, AWS recently announced C# support for AWS Lambda functions.  In this article, I will show you how to develop, install and call a simple C# AWS Lambda Function using Visual Studio 2015.  

Before you start this example, first go to the Remarks section of this document and make sure you have all of the required components installed. 


  [1]: https://en.wikipedia.org/wiki/Serverless_computing


Install Required Components
---------------------------
At the time of this writing (Jan 2017),  the following components will need to be installed on your machine in this order.  The machine I used had Windows 10 Home installed.   


1. [Visual Studio 2015 update 3][2]
2. [AWS Toolkit for Visual Studio 2015][3] - version 1.11.2.0
3. [Microsoft .NET Core Tools (Preview 2)][4] - version 14.1.21111.0

Each of these components are actively developed and released, so double check the links and versions and update this document when needed.

Each of these [can take some time to install][1], so be patient, do one at a time to minimize mistakes.  

Lastly, rebooting Windows before starting development is sad, but always a good idea after doing major development tool installations on a Windows box.


  [1]: http://stackoverflow.com/questions/28788574/visual-studio-2015-installer-hangs-during-install
  [2]: https://www.visualstudio.com/vs/community/
  [3]: https://aws.amazon.com/visualstudio/
  [4]: https://go.microsoft.com/fwlink/?LinkId=827546

## Step 1.  Create a new solution in Visual Studio
1.  Open Visual Studio and Select File -> New Project
2.  Select AWS Lambda Project with Tests (.NET Core)
[![enter image description here][1]][1]
3.  Next the **Select Blueprint** screen will display.  Select **Empty Function** and Click the Finish button:
[![enter image description here][2]][2]
4.  Go to Tools -> NuGet Package Manager -> Package Manager Console.
5.  In the console window, type [Install-Package Amazon.Lambda.Core][3] 
6.  Right-click your project in the solution explorer and select Restore Packages.  This is the final preparation step before you start writing code.


[![enter image description here][4]][4]


  [1]: https://i.stack.imgur.com/WnOPj.png
  [2]: https://i.stack.imgur.com/WOATD.png
  [3]: https://www.nuget.org/packages/Amazon.Lambda.Core/
  [4]: https://i.stack.imgur.com/Dkkpx.png

## Step 2.  Add and change code in your project
1.  Open `Function.cs` and replace the class code with the following:
    
        public class Function
        {

        /// <summary>
        /// A simple function that takes a birth date and returns Age in years
        /// </summary>
        /// <param name="input"></param>
        /// <returns>Age is years</returns>
        /// 
        [LambdaSerializer(typeof(SimpleSerializer))]
        public string FunctionHandler(Dictionary<string, int> input)
        {
            var defaultMessage = "Age could not be determined.";

            var birthDate = new DateTime(input["year"], input["month"], input["day"]);
            var ageInYears = DateTime.Today.Year - birthDate.Year;
            if (birthDate.DayOfYear > DateTime.Today.DayOfYear)
                ageInYears--;

            defaultMessage = $"Age in years: {ageInYears}";

            return defaultMessage;
        }
        }

You will need to add the following using statements near the top: 
    
    using System.Collections.Generic;
    using Amazon.Lambda.Core;

2.  Add a file to the project named `SimpleSerializer.cs`
3.  Place the following code in that file:


    using System;

    using System.IO;
    using Amazon.Lambda.Core;
    using Newtonsoft.Json;

    namespace AWSLambdaFunctionAgeInYears
    {
    public class SimpleSerializer : ILambdaSerializer
    {
        public T Deserialize<T>(Stream requestStream)
        {
            string text;
            using (var reader = new StreamReader(requestStream))
                text = reader.ReadToEnd();

            try
            {
                return JsonConvert.DeserializeObject<T>(text);
            }
            catch (Exception ex)
            {
                if (typeof(T) == typeof(System.String))
                    return (T)Convert.ChangeType(text, typeof(T));

                throw ex;
            }

        }

        public void Serialize<T>(T response, Stream responseStream)
        {
            StreamWriter streamWriter = new StreamWriter(responseStream);
            try
            {
                string text = JsonConvert.SerializeObject(response);
                streamWriter.Write(text);
                streamWriter.Flush();
            }
            catch (Exception ex)
            {
                if (typeof(T) == typeof(System.String))
                {
                    streamWriter.Write(response);
                    streamWriter.Flush();
                    return;
                }

                throw ex;
            }

        }
    }
    }

4.  In the Test Project, change line 23 of the `FunctionTest.cs` to the following:

                var upperCase = function.FunctionHandler(null);

5.  Build your solution - you should have no build errors.


## Step 3 - Install your Lambda Function into AWS
1.  Right Click your project and select **Publish to AWS Lambda...**

2.  The Upload to AWS Lambda screen will appear.  Make sure the correct region is selected.  Keep all of the defaults.
Then only enter the Function Name:  AWSLambdaFunctionAgeInYears and then click next.
[![enter image description here][1]][1] 

3.  On the next page, select AWSLambdaRole for the Role Name field.  Click Upload and the function should upload without error.
[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/pAt9w.png
  [2]: https://i.stack.imgur.com/O4DTI.png

## Step 4.  Invoke your AWS Lambda function from within Visual Studio
1.  After Step 3 above, Visual Studio will open the `View Function` window with your function loaded.  
2.  On the bottom left of this screen, enter the following json into the **Sample Input** box:


    {
    "month": "10",
    "day": "28",
    "year": "1979"
    }

[![enter image description here][1]][1]

3.  Last step:  Click the green **Invoke** button.  The function will run on AWS and the  response will be displayed in the bottom right hand **Response** window.


  [1]: https://i.stack.imgur.com/kgCPb.png

