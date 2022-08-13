---
title: "Change source code with Roslyn"
slug: "change-source-code-with-roslyn"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

Practical examples of using Roslyn for source code transformations.

 - Roslyn syntax trees are immutable. By calling a method like ReplaceNodes we generate a new node rather than modifying the existing one. This requires you to always change the object you have been working on. 

## Replace existing Attributes for all methods in C# using the syntax tree
The following snippet replaces all Attributes called `PreviousAttribute` by an Attribute called `ReplacementAttribute` for an entire solution. The sample manually searches the Syntax tree and replaces all affected nodes. 

<!-- language: lang-c# -->
        static async Task<bool> ModifySolution(string solutionPath)
        {
            using (var workspace = MSBuildWorkspace.Create())
            {
                // Selects a Solution File
                var solution = await workspace.OpenSolutionAsync(solutionPath);
                // Iterates through every project
                foreach (var project in solution.Projects)
                {
                    // Iterates through every file
                    foreach (var document in project.Documents)
                    {
                        // Selects the syntax tree
                        var syntaxTree = await document.GetSyntaxTreeAsync();
                        var root = syntaxTree.GetRoot();
                        // Finds all Attribute Declarations in the Document
                        var existingAttributesList = root.DescendantNodes().OfType<AttributeListSyntax>()
                            // Where the Attribute is declared on a method
                            .Where(curr => curr.Parent is MethodDeclarationSyntax)
                            // And the attribute is named "PreviousAttribute"
                            .Where(curr => curr.Attributes.Any(currentAttribute => currentAttribute.Name.GetText().ToString() == "PreviousAttribute"))
                            .ToList();
                        if (existingAttributesList.Any())
                        {
                            // Generates a replacement for every attribute
                            var replacementAttribute = SyntaxFactory.AttributeList(
                                SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("ReplacementAttribute"),
                                    SyntaxFactory.AttributeArgumentList(
                                        SyntaxFactory.SeparatedList(new[]
                                        {
                                        SyntaxFactory.AttributeArgument(
                                            SyntaxFactory.LiteralExpression(
                                                SyntaxKind.StringLiteralExpression, SyntaxFactory.Literal(@"Sample"))
                                            )
                                        })))));
                            // Replaces all attributes at once.
                            // Note that you should not use root.ReplaceNode
                            // since it would only replace the first note
                            root = root.ReplaceNodes(existingAttributesList, (node, n2) => replacementAttribute);
                            // Exchanges the document in the solution by the newly generated document
                            solution = solution.WithDocumentSyntaxRoot(document.Id, root);
                        }
                    }
                }
                // applies the changes to the solution
                var result = workspace.TryApplyChanges(solution);
                return result;
            }
        }

The above example can be tested for the following class:

<!-- language: lang-c# -->
    public class Program
    {
        [PreviousAttribute()]
        static void Main(string[] args)
        {
        }
    }


You should not use the Methode root.ReplaceNode to replace multiple nodes. Since the tree is immutable you will be working on different objects. Using the following snippet in the above example would not yield the expected result:

<!-- language: lang-c# -->
    foreach(var node in existingAttributesList){
        root = root.ReplaceNode(node, replacementAttribute);
    }

The first call to `ReplaceNode` would create a new root element. However the elements in `existingAttributesList` belong to a different root (the previous root element) and cannot be replaced because of this. This would result in the first Attribute being replaced and the following Attributes remaining unchanged since all consecutive calls would be performed on a node not present in the new tree.

## Replace existing Attributes for all methods in C# using a SyntaxRewriter
The following snippet replaces all Attributes called "PreviousAttribute" by an Attribute called "ReplacementAttribute" for an entire solution. The sample manually uses a SyntaxRewriter to exchange the attributes.
<!-- language: lang-c# -->
    
    /// <summary>
    /// The CSharpSyntaxRewriter allows to rewrite the Syntax of a node
    /// </summary>
    public class AttributeStatementChanger : CSharpSyntaxRewriter
    {
        /// Visited for all AttributeListSyntax nodes
        /// The method replaces all PreviousAttribute attributes annotating a method by ReplacementAttribute attributes
        public override SyntaxNode VisitAttributeList(AttributeListSyntax node)
        {
            // If the parent is a MethodDeclaration (= the attribute annotes a method)
            if (node.Parent is MethodDeclarationSyntax &&
                // and if the attribute name is PreviousAttribute
                node.Attributes.Any(
                    currentAttribute => currentAttribute.Name.GetText().ToString() == "PreviousAttribute"))
            {
                // Return an alternate node that is injected instead of the current node
                return SyntaxFactory.AttributeList(
                                SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("ReplacementAttribute"),
                                    SyntaxFactory.AttributeArgumentList(
                                        SyntaxFactory.SeparatedList(new[]
                                        {
                                        SyntaxFactory.AttributeArgument(
                                            SyntaxFactory.LiteralExpression(
                                                SyntaxKind.StringLiteralExpression, SyntaxFactory.Literal(@"Sample"))
                                            )
                                        })))));
            }
            // Otherwise the node is left untouched
            return base.VisitAttributeList(node);
        }
    }

    /// The method calling the Syntax Rewriter
    private static async Task<bool> ModifySolutionUsingSyntaxRewriter(string solutionPath)
    {
        using (var workspace = MSBuildWorkspace.Create())
        {
            // Selects a Solution File
            var solution = await workspace.OpenSolutionAsync(solutionPath);
            // Iterates through every project
            foreach (var project in solution.Projects)
            {
                // Iterates through every file
                foreach (var document in project.Documents)
                {
                    // Selects the syntax tree
                    var syntaxTree = await document.GetSyntaxTreeAsync();
                    var root = syntaxTree.GetRoot();

                    // Generates the syntax rewriter
                    var rewriter = new AttributeStatementChanger();
                    root = rewriter.Visit(root);

                    // Exchanges the document in the solution by the newly generated document
                    solution = solution.WithDocumentSyntaxRoot(document.Id, root);
                }
            }
            // applies the changes to the solution
            var result = workspace.TryApplyChanges(solution);
            return result;
        }
    }

The above example can be tested for the following class:

<!-- language: lang-c# -->
    public class Program
    {
        [PreviousAttribute()]
        static void Main(string[] args)
        {
        }
    }


