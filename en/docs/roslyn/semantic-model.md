---
title: "Semantic Model"
slug: "semantic-model"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

In contrast to the Syntax API the exposes all kinds of syntax level information, the semantic model gives our code more "meaning" and allows us to answer questions like "What names are in scope at this location?", "What members are accessible from this method?", "What variables are used in this block of text?", "What does this name/expression refer to?".

 - Querying the Semantic Model is more costly than querying the Syntax Tree, due to the fact that it most commonly triggers a compilation.

## Getting the Semantic Model
There qutie a fiew ways to get the sematic model.

<!-- language-all: lang-csh -->
- From a `Document` class

      Document document = ...;
      SemanticModel semanticModel = await document.GetSemanticModelAsync();

- From a `Compilation`class

      CSharpCompilation compilation = ...;
      var semanticModel = await compilation.GetSemanticModel(syntaxTree);

- From an `AnalysisContext`. Fro example inside a `DiagnosticAnalyzer` you can do:

      public override void Initialize(AnalysisContext context)
      {
          context.RegisterSemanticModelAction(x =>
          {
              var semanticModel = x.SemanticModel;
              // Do magical magic here.
          });
      }
    



## Get all the references to a method
<!-- language-all: lang-csh -->

    var syntaxRoot = await document.GetSyntaxRootAsync();

    var semanticModel = await document.GetSemanticModelAsync();
    var sampleMethodInvocation = syntaxRoot
        .DescendantNodes()
        .OfType<InvocationExpressionSyntax>()
        .First();

    var sampleMethodSymbol = semanticModel.GetSymbolInfo(sampleMethodInvocation).Symbol;
    var referencesToSampleMethod = await SymbolFinder.FindReferencesAsync(sampleMethodSymbol, document.Project.Solution);

