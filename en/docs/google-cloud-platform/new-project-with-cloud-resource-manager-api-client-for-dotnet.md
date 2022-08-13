---
title: "New Project with Cloud Resource Manager API Client for .NET"
slug: "new-project-with-cloud-resource-manager-api-client-for-net"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

We will use [Google API Client Libraries](https://developers.google.com/discovery/libraries) for .NET for this sample.

There are other libraries. Please see Google [Client Libraries Explained](https://cloud.google.com/apis/docs/client-libraries-explained) for details.

We will use the [Cloud Resource Manager API](https://cloud.google.com/resource-manager/) for [Creating and Managing Projects](https://cloud.google.com/resource-manager/docs/creating-managing-projects).

Let's get started.

Putting it all together... 

You should have two files. The first file is either called `packages.config` or `project.json`.

Let's name the second file `Program.cs`. All the code that was included in the sections above may be pasted into a single main method:

```
using System;
using System.Collections.Generic;
using System.Threading.Tasks;

using Google.Apis.Auth.OAuth2;
using Google.Apis.CloudResourceManager.v1;
using Google.Apis.Services;

using Data = Google.Apis.CloudResourceManager.v1.Data;

namespace OurFirstProject
{
    public class Program
    {
        private const string projectId = [YOUR-PROJECT-ID];
        private const string applicationName = "Test";

        public static void Main(string[] args)
        {

            var scopes = new String[] {
                CloudResourceManagerService.Scope.CloudPlatform
            };

            GoogleCredential credential = Task.Run(
                () => GoogleCredential.GetApplicationDefaultAsync()
            ).Result;

            if (credential.IsCreateScopedRequired)
            {
                credential = credential.CreateScoped(scopes);
            }

            CloudResourceManagerService service = new CloudResourceManagerService(
                new BaseClientService.Initializer()
                {
                    HttpClientInitializer = credential,
                    ApplicationName = applicationName
                }
            );

            Console.WriteLine("1. Create Project");
            Data.Operation operation1 = service.Projects.Create(
                new Data.Project()
                {
                    ProjectId = projectId,
                }
            ).Execute();

            Console.Write("2. Awaiting Operation Completion");
            Data.Operation operation2;
            do
            {
                operation2 = service.Operations.Get(operation1.Name).Execute();
                Console.WriteLine(operation2.Done.ToString());
                System.Threading.Thread.Sleep(1000);
            } while (operation2.Done != true);

            Console.WriteLine();
            Console.WriteLine("Enter to continue");
            Console.ReadLine();

            Console.WriteLine("3. Deleting Project");
            var operation3 = service.Projects.Delete(projectId).Execute();
        }
    }
}
```

If you're using Windows and Visual Studio, "Start"

If you're using Linux, you should first restore the packages, then run the app

```
dotnet restore
dotnet run
```

The output should be similar to:

```
Compiling Projects for .NETCoreApp,Version=v1.1

Compilation succeeded.
    0 Warning(s)
    0 Error(s)

Time elapsed 00:00:01.4161926
 

1. Create Project

2. Awaiting Operation Completion



True

Enter to continue

3. Deleting Project
```

The "Awaiting" step will contain blank lines (hopefully) ending in "True".

## Getting Started
The code will work on Windows and Linux.

It runs on .NET Core or .NET Standard. The only difference is that, if you wish to run on .NET Core, you should use `project.json`. If you wish to run on .NET Standard, you should use `packages.config`.

The API Client Library for Cloud Resource Manager API is available on nuget:

https://www.nuget.org/packages/Google.Apis.CloudResourceManager.v1/

The version as of writing is: 1.22.0.809

We'll do this 2 ways:

* Windows with .NET Standard; and
* Linux with .NET Core.

**Windows**

If you are using Visual Studio, create a new "Visual C#" "Console Application". Otherwise, create a directory for the project and create a file in it called `packages.config`. packages.config is for .NET Standard but we're using .NET Standard with Windows. You may only run .NET Standard on Windows. Replace the contents of `packages.config` with:

```
﻿<?xml version="1.0" encoding="utf-8"?>
<packages>
  <package id="Google.Apis" version="1.22.0"
    targetFramework="net452" />
  <package id="Google.Apis.Auth" version="1.22.0"
    targetFramework="net452" />
  <package id="Google.Apis.CloudResourceManager.v1" version="1.22.0.809" 
    targetFramework="net452" />
  <package id="Google.Apis.Core" version="1.22.0"
    targetFramework="net452" />
</packages>
```

**Linux**

Create a directory for the project and create a file in it called `project.json`. `project.json` is for .NET Core but we're using .NET Core with Linux in this example. You may run .NET Core on Linux or on Windows. Replace the contents of `project.json` with:

```
{
  "version": "1.0.0-*",
  "buildOptions": {
    "debugType": "portable",
    "emitEntryPoint": true
  },
  "dependencies": {},
  "frameworks": {
    "netcoreapp1.1": {
      "dependencies": {
        "Microsoft.NETCore.App": {
          "type": "platform",
          "version": "1.1.0"
        },
        "Google.Apis.CloudResourceManager.v1":"1.22.0.809"
      },
      "imports": "dnxcore50"
    }
  }
}
```

## Application Default Credentials
I won't repeat it all here: "[Application Default Credentials](https://developers.google.com/identity/protocols/application-default-credentials) provide a simple way to get authorization credentials for use calling Google APIs."

If you can use Application Default Credentials, do.

There is an extra step you will need to perform the before first using Application Default Credentials as your identity when calling APIs from your machine:

```
gcloud auth application-default login [yourname@gmail.com]
```

Here's why you'll prefer to use Application Default Credentials:


```
var scopes = new String[] {
    CloudResourceManagerService.Scope.CloudPlatform
};

GoogleCredential credential = Task.Run(
    () => GoogleCredential.GetApplicationDefaultAsync()
).Result;

if (credential.IsCreateScopedRequired)
{
    credential = credential.CreateScoped(scopes);
}
```

...That's all the code you need to authorize calls to (any) Google Cloud API!

We'll use the `credential` object in the next step to make calls against the Google service...

## Calling any method (!) on any Google service (!)
Once you become familiar with the code to call one method on one Google service, you will be able to infer how to call _any_ method on _any_ Google service.

First, we make a connection to the service using the `credential` object instantiated in the previous example:

```
CloudResourceManagerService service = new CloudResourceManagerService(
    new BaseClientService.Initializer()
    {
        HttpClientInitializer = credential,
        ApplicationName = "Our First Google API Client"
    }
);
```
Then we can call methods provided by the service. What methods are available?

https://cloud.google.com/resource-manager/docs/apis

What is the underlying REST request for `Projects.Create`?

https://cloud.google.com/resource-manager/reference/rest/v1/projects/create

OK... Let's write the code.

The code expects a string value for `projectId`. Project IDs are unique identifiers. I recommend you use a system for naming your projects to help you identify them.

`Projects.Create` expects a `Data.Project` object. This object one mandatory property, the Project ID which is all we'll provide but we could also provide a Project Name, Labels, details of the Project's parent etc.

```
Data.Operation operation1 = service.Projects.Create(
    new Data.Project()
    {
        ProjectId = projectId,
    }
).Execute();
```

Project creation is handled asynchronously. We are given an `Operation` object that we must poll to determine when the Project is created. Operations have a Name property that uniquely identifies the operation. The next section of code polls the platform "Are we done yet?". The project will be created when our new operation includes a `Done` property that is `True`.


```
Data.Operation operation2;
do
{
    operation2 = service.Operations.Get(operation1.Name).Execute();
    System.Threading.Thread.Sleep(1000);
} while (operation2.Done != true);
```

For completeness, and hopefully many years from now after much happy use of your project, you may need to delete your project. We simply call Projects.Delete and provide our Project ID. This also returns an operation and we ought really to poll this operation too until it completes definitively. Our project will then be deleted.

```
var operation3 = service.Projects.Delete(projectId).Execute();
```

That's it!

