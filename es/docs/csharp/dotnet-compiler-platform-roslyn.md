---
title: "Plataforma del compilador .NET (Roslyn)"
slug: "plataforma-del-compilador-net-roslyn"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

## Modelo semántico
Un **modelo semántico** ofrece un nivel más profundo de interpretación y conocimiento del código en comparación con un árbol de sintaxis. Donde los árboles de sintaxis pueden decir los nombres de las variables, los modelos semánticos también dan el tipo y todas las referencias. Los árboles de sintaxis notan las llamadas a métodos, pero los modelos semánticos brindan referencias a la ubicación precisa en la que se declara el método (después de que se haya aplicado la resolución de sobrecarga).

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

Esto genera una lista de variables locales utilizando un árbol de sintaxis. Luego consulta el modelo semántico para obtener el nombre completo del tipo y encontrar todas las referencias de cada variable.

## Árbol de sintaxis
Un **árbol de sintaxis** es una estructura de datos inmutable que representa el programa como un árbol de nombres, comandos y marcas (como se configuró previamente en el editor).

Por ejemplo, suponga que se ha configurado una instancia de ``Microsoft.CodeAnalysis.Compilation`` llamada ``compilation``. Hay varias formas de listar los nombres de cada variable declarada en el código cargado. Para hacerlo de manera ingenua, tome todas las partes de la sintaxis en cada documento (el método ``DescendantNodes``) y use Linq para seleccionar los nodos que describen la declaración de variables:

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

Cada tipo de construcción de C# con un tipo correspondiente existirá en el árbol de sintaxis. Para encontrar rápidamente tipos específicos, utilice la ventana ``Syntax Visualizer`` de Visual Studio. Esto interpretará el documento abierto actual como un árbol de sintaxis de Roslyn.

## Crear espacio de trabajo desde el proyecto MSBuild
Primero obtenga el nuget ``Microsoft.CodeAnalysis.CSharp.Workspaces`` antes de continuar.

    var workspace = Microsoft.CodeAnalysis.MSBuild.MSBuildWorkspace.Create();
    var project = await workspace.OpenProjectAsync(projectFilePath);
    var compilation = await project.GetCompilationAsync();

    foreach (var diagnostic in compilation.GetDiagnostics()
        .Where(d => d.Severity == Microsoft.CodeAnalysis.DiagnosticSeverity.Error))
    {
        Console.WriteLine(diagnostic);
    }

Para cargar código existente en el espacio de trabajo, compile e informe de errores. Posteriormente, el código se ubicará en la memoria. A partir de aquí, tanto el lado sintáctico como el semántico estarán disponibles para trabajar.

