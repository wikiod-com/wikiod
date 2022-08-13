---
title: "project.json"
slug: "projectjson"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Project json is a project configuration file structure, temporarily used by asp.net-core projects, before Microsoft moved back to the csproj files in favor of msbuild.

## Simple Library project example
A library based on NETStandard 1.6 would look like this:

    {
      "version": "1.0.0",
      "dependencies": {
        "NETStandard.Library": "1.6.1", //nuget dependency
       },
      "frameworks": { //frameworks the library is build for
        "netstandard1.6": {}
      },
      "buildOptions": {
        "debugType": "portable"
      }
    }

## Complete json file:
Taken from [microsoft's github page with official documentation](https://github.com/dotnet/docs)

    {
    "name": String, //The name of the project, used for the assembly name as well as the name of the package. The top level folder name is used if this property is not specified.
    "version": String, //The Semver version of the project, also used for the NuGet package.
    "description": String, //A longer description of the project. Used in the assembly properties.
    "copyright": String, //The copyright information for the project. Used in the assembly properties.
    "title": String, //The friendly name of the project, can contain spaces and special characters not allowed when using the `name` property. Used in the assembly properties.
    "entryPoint": String, //The entrypoint method for the project. `Main` by default.
    "testRunner": String, //The name of the test runner, such as NUnit or xUnit, to use with this project. Setting this also marks the project as a test project.
    "authors": String[], // An array of strings with the names of the authors of the project.
    "language": String, //The (human) language of the project. Corresponds to the "neutral-language" compiler argument.
    "embedInteropTypes": Boolean, //`true` to embed COM interop types in the assembly; otherwise, `false`. 
    "preprocess": String or String[], //Specifies which files are included in preprocessing.
    "shared": String or String[], //Specifies which files are shared, this is used for library export.
    "dependencies": Object { //project and nuget dependencies
        version: String, //Specifies the version or version range of the dependency. Use the \* wildcard to specify a floating dependency version.
        type: String, //type of dependency: build
        target: String, //Restricts the dependency to match only a `project` or a `package`.
        include: String,
        exclude: String,
        suppressParent: String
    },
    "tools": Object, //An object that defines package dependencies that are used as tools for the current project, not as references. Packages defined here are available in scripts that run during the build process, but they are not accessible to the code in the project itself. Tools can for example include code generators or post-build tools that perform tasks related to packing.
    "scripts": Object, // commandline scripts: precompile, postcompile, prepublish & postpublish
    "buildOptions": Object {
        "define": String[], //A list of defines such as "DEBUG" or "TRACE" that can be used in conditional compilation in the code.
        "nowarn": String[], //A list of warnings to ignore.
        "additionalArguments": String[], //A list of extra arguments that will be passed to the compiler.
        "warningsAsErrors": Boolean,
        "allowUnsafe": Boolean,
        "emitEntryPoint": Boolean,
        "optimize": Boolean,
        "platform": String,
        "languageVersion": String,
        "keyFile": String,
        "delaySign": Boolean,
        "publicSign": Boolean,
        "debugType": String,
        "xmlDoc": Boolean,
        "preserveCompilationContext": Boolean,
        "outputName": String,
        "compilerName": String,
        "compile": Object {
            "include": String or String[],
            "exclude": String or String[],
            "includeFiles": String or String[],
            "excludeFiles": String or String[],
            "builtIns": Object,
            "mappings": Object
        },
        "embed": Object {
            "include": String or String[],
            "exclude": String or String[],
            "includeFiles": String or String[],
            "excludeFiles": String or String[],
            "builtIns": Object,
            "mappings": Object
        },
        "copyToOutput": Object {
            "include": String or String[],
            "exclude": String or String[],
            "includeFiles": String or String[],
            "excludeFiles": String or String[],
            "builtIns": Object,
            "mappings": Object
        }
    },
    "publishOptions": Object {
        "include": String or String[],
        "exclude": String or String[],
        "includeFiles": String or String[],
        "excludeFiles": String or String[],
        "builtIns": Object,
        "mappings": Object
    },
    "runtimeOptions": Object {
        "configProperties": Object {
            "System.GC.Server": Boolean,
            "System.GC.Concurrent": Boolean,
            "System.GC.RetainVM": Boolean,
            "System.Threading.ThreadPool.MinThreads": Integer,
            "System.Threading.ThreadPool.MaxThreads": Integer
        },
        "framework": Object {
            "name": String,
            "version": String,
        },
        "applyPatches": Boolean
    },
    "packOptions": Object {
        "summary": String,
        "tags": String[],
        "owners": String[],
        "releaseNotes": String,
        "iconUrl": String,
        "projectUrl": String,
        "licenseUrl": String,
        "requireLicenseAcceptance": Boolean,
        "repository": Object {
            "type": String,
            "url": String
        },
        "files": Object {
            "include": String or String[],
            "exclude": String or String[],
            "includeFiles": String or String[],
            "excludeFiles": String or String[],
            "builtIns": Object,
            "mappings": Object
        }
    },
    "analyzerOptions": Object {
        "languageId": String
    },
    "configurations": Object,
    "frameworks": Object {
        "dependencies": Object {
            version: String,
            type: String,
            target: String,
            include: String,
            exclude: String,
            suppressParent: String
        },        
        "frameworkAssemblies": Object,
        "wrappedProject": String,
        "bin": Object {
            assembly: String
        }
    },
    "runtimes": Object,
    "userSecretsId": String
    }





## Simple startup project
A simple example of project configuration for a .NetCore 1.1 Console App


    {
      "version": "1.0.0",
      "buildOptions": {
        "emitEntryPoint": true // make sure entry point is emitted.
      },
      "dependencies": {
      },
      "tools": {
      },
      "frameworks": {
        "netcoreapp1.1": { // run as console app
          "dependencies": {
            "Microsoft.NETCore.App": {
              "type": "platform",
              "version": "1.1.0"
            }
          },
          "imports": "dnxcore50"
        }
      },
    }

