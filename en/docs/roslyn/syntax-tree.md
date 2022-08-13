---
title: "Syntax Tree"
slug: "syntax-tree"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

One of the major parts of the Roslyn compiler is the Syntax API. It exposes the syntax trees the compilers use to understand Visual Basic and C# programs.

 - The Syntax Tree is a [Parse Tree][1] in the context of the Roslyn compiler.



  [1]: https://en.wikipedia.org/wiki/Parse_tree

## Traversing the Syntax Tree Using LINQ
You can easily navigate the a Syntax Tree using LINQ. For example it is easy to get all the `ClassDeclarationSyntax` nodes (declared classes), that have a name starting with the letter `A`:

<!-- language-all: lang-csh -->
    var allClassesWithNameStartingWithA = syntaxRoot.DescendantNodes()
        .OfType<ClassDeclarationSyntax>()
        .Where(x => x.Identifier.ToString().StartsWith("A"));

Or getting all the classes that have attributes:

    var allClassesWithAttriutes = syntaxRoot.DescendantNodes()
        .OfType<ClassDeclarationSyntax>()
        .Where(x => x.AttributeLists.Any(y => y.Attributes.Any()));



## Traversing the Syntax Tree using a CSharpSyntaxWalker
The `CSharpSyntaxWalker` class is out of the box implementation of the Visitor pattern, that we can use to traverse our Syntax Tree. Here is a simple example of a Syntax Walker that collects all the `struct`-s that have a name, starting with the letter `A`:

<!-- language-all: lang-csh -->

    public class StructCollector : CSharpSyntaxWalker
    {
        public StructCollector()
        {
            this.Structs = new List<StructDeclarationSyntax>();
        }

        public IList<StructDeclarationSyntax> Structs { get; }

        public override void VisitStructDeclaration(StructDeclarationSyntax node)
        {
            if (node.Identifier.ToString().StartsWith("A"))
            {
                this.Structs.Add(node);
            }
        }
    }

We can use our SyntaxWalker in the following way:

    var structCollector = new StructCollector();
    structCollector.Visit(syntaxRoot); // Or any other syntax node
    Console.WriteLine($"The number of structs that have a name starting with the letter 'A' is {structCollector.Structs.Count}");

## Getting the Syntax Tree Root from a Document.
If you already have access to your `Document` class from your workspace ([Using Workspaces][1]) it is easy to access the root of your Syntax tree.

<!-- language-all: lang-csh -->
     Document document = ... // Get document from workspace or other source 

     var syntaxRoot = await document.GetSyntaxRootAsync();


  [1]: https://www.wikiod.com/roslyn/using-workspaces

