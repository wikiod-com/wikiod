---
title: "Using Workspaces"
slug: "using-workspaces"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

The workspace is a programmatic representation of the C# hierarchy that consists of a solution, child projects and child documents.

- Currently there is no MSBuild workspace that supports a .NET Standard compliant projects. For more information see [here][1].


  [1]: http://stackoverflow.com/questions/42395336/roslyn-workspace-for-net-cores-new-csproj-format

## Creating an MSBuildWorspace, loading a solution and getting all the documents in all that solution
The `MSBuildWorspace` is built around the concept of handling MSBuild solutions (`.sln` files) and their respective projects (`.csproj`, `.vbproj`).
Adding new projects and documents to this workspace is not supported.

<!-- language: lang-cs -->
    string solutionPath = @"C:\Path\To\Solution\Sample.sln";

    MSBuildWorkspace workspace = MSBuildWorkspace.Create();
    Solution solution = await workspace.OpenSolutionAsync(nancyApp);

    var allDocumentsInSolution = solution.Projects.SelectMany(x => x.Documents);


## Getting the VisualStudioWorkspace from inside a Visual Studio Extension
In contrast to the other types of workspaces, the `VisualStudioWorkspace`, cannot be created manually. It can be accessed when building a Visual Studio extension.

When inside your extension package project, go to `[YourVSPackage]Package.cs` file. There you can acquire the workspace in two ways:

<!-- language: lang-cs -->
    protected override void Initialize()
    { 
        // Additional code...

        var componentModel = (IComponentModel)this.GetService(typeof(SComponentModel));
        var workspace = componentModel.GetService<VisualStudioWorkspace>();
    }

Or by using MEF:
<!-- language: lang-cs -->

    [Import(typeof(VisualStudioWorkspace))]
    public VisualStudioWorkspace ImportedWorkspace { get; set; }

A great video tutorial about the `VisualStudioWorkspace`, can be found [here][1].


  [1]: https://www.youtube.com/watch?v=35BFJt91rxY

## Creating an AdhocWorkspace and adding a new project and a file to it.
The idea behind the `AdhocWorkspace` is to create a workspace on the fly.

<!-- language: lang-cs -->
    var workspace = new AdhocWorkspace();

    string projectName = "HelloWorldProject";
    ProjectId projectId = ProjectId.CreateNewId();
    VersionStamp versionStamp = VersionStamp.Create();
    ProjectInfo helloWorldProject = ProjectInfo.Create(projectId, versionStamp, projectName, projectName, LanguageNames.CSharp);
    SourceText sourceText = SourceText.From("class Program { static void Main() { System.Console.WriteLine(\"HelloWorld\"); } }");

    Project newProject = workspace.AddProject(helloWorldProject);
    Document newDocument = workspace.AddDocument(newProject.Id, "Program.cs", sourceText);

