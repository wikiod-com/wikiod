---
title: "Plate-forme de compilation .NET (Roslyn)"
slug: "plate-forme-de-compilation-net-roslyn"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

## Modèle sémantique
Un **modèle sémantique** offre un niveau d'interprétation et un aperçu du code plus approfondis par rapport à un arbre de syntaxe. Là où les arbres de syntaxe peuvent indiquer les noms des variables, les modèles sémantiques donnent également le type et toutes les références. Les arbres de syntaxe remarquent les appels de méthode, mais les modèles sémantiques donnent des références à l'emplacement précis où la méthode est déclarée (après l'application de la résolution de surcharge).

    var workspace = Microsoft.CodeAnalysis.MSBuild.MSBuildWorkspace.Create();
    var sln = await workspace.OpenSolutionAsync(solutionFilePath);
    var project = sln.Projects.First();
    var compilation = await project.GetCompilationAsync();

    foreach (var syntaxTree in compilation.SyntaxTrees)
    {
        var root = await syntaxTree.GetRootAsync();

        var declaredIdentifiers = root.DescendantNodes()
            .Where(an => an is VariableDeclaratorSyntax)
            .Cast<VariableDeclaratorSyntax>();

        foreach (var di in declaredIdentifiers)
        {
            Console.WriteLine(di.Identifier);
            // => "root"

            var variableSymbol = compilation
                .GetSemanticModel(syntaxTree)
                .GetDeclaredSymbol(di) as ILocalSymbol;

            Console.WriteLine(variableSymbol.Type);
            // => "Microsoft.CodeAnalysis.SyntaxNode"

            var references = await SymbolFinder.FindReferencesAsync(variableSymbol, sln);
            foreach (var reference in references)
            {
                foreach (var loc in reference.Locations)
                {
                    Console.WriteLine(loc.Location.SourceSpan);
                    // => "[1375..1379)"
                }
            }
        }
    }

Cela génère une liste de variables locales à l'aide d'un arbre de syntaxe. Ensuite, il consulte le modèle sémantique pour obtenir le nom complet du type et trouver toutes les références de chaque variable.

## Arbre de syntaxe
Un **arbre de syntaxe** est une structure de données immuable représentant le programme sous la forme d'un arbre de noms, de commandes et de marques (comme précédemment configuré dans l'éditeur.)

Par exemple, supposons qu'une instance ``Microsoft.CodeAnalysis.Compilation`` nommée ``compilation`` a été configurée. Il existe plusieurs façons de lister les noms de chaque variable déclarée dans le code chargé. Pour le faire naïvement, prenez tous les éléments de syntaxe de chaque document (la méthode ``DescendantNodes``) et utilisez Linq pour sélectionner les nœuds qui décrivent la déclaration des variables :

    foreach (var syntaxTree in compilation.SyntaxTrees)
    {
        var root = await syntaxTree.GetRootAsync();
        var declaredIdentifiers = root.DescendantNodes()
            .Where(an => an is VariableDeclaratorSyntax)
            .Cast<VariableDeclaratorSyntax>()
            .Select(vd => vd.Identifier);

        foreach (var di in declaredIdentifiers)
        {
            Console.WriteLine(di);
        }
    }

Chaque type de construction C# avec un type correspondant existera dans l'arbre de syntaxe. Pour trouver rapidement des types spécifiques, utilisez la fenêtre ``Syntax Visualizer`` de Visual Studio. Cela interprétera le document actuellement ouvert comme un arbre de syntaxe Roslyn.

## Créer un espace de travail à partir du projet MSBuild
Obtenez d'abord le nuget ``Microsoft.CodeAnalysis.CSharp.Workspaces`` avant de continuer.

    var workspace = Microsoft.CodeAnalysis.MSBuild.MSBuildWorkspace.Create();
    var project = await workspace.OpenProjectAsync(projectFilePath);
    var compilation = await project.GetCompilationAsync();

    foreach (var diagnostic in compilation.GetDiagnostics()
        .Where(d => d.Severity == Microsoft.CodeAnalysis.DiagnosticSeverity.Error))
    {
        Console.WriteLine(diagnostic);
    }

Pour charger le code existant dans l'espace de travail, compilez et signalez les erreurs. Ensuite, le code sera situé en mémoire. À partir de là, le côté syntaxique et sémantique sera disponible pour travailler.

