---
title: "Plataforma do compilador .NET (Roslyn)"
slug: "plataforma-do-compilador-net-roslyn"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

## Modelo semântico
Um **Modelo Semântico** oferece um nível mais profundo de interpretação e percepção do código em comparação com uma árvore sintática. Onde as árvores de sintaxe podem informar os nomes das variáveis, os modelos semânticos também fornecem o tipo e todas as referências. As árvores de sintaxe notam as chamadas de método, mas os modelos semânticos fornecem referências ao local preciso em que o método é declarado (após a resolução de sobrecarga ter sido aplicada).

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

Isso gera uma lista de variáveis ​​locais usando uma árvore de sintaxe. Em seguida, ele consulta o modelo semântico para obter o nome completo do tipo e encontrar todas as referências de cada variável.

## Árvore de sintaxe
Uma **Árvore de Sintaxe** é uma estrutura de dados imutável que representa o programa como uma árvore de nomes, comandos e marcas (conforme configurado anteriormente no editor).

Por exemplo, suponha que uma instância ``Microsoft.CodeAnalysis.Compilation`` chamada ``compilation`` tenha sido configurada. Existem várias maneiras de listar os nomes de cada variável declarada no código carregado. Para fazer isso de forma ingênua, pegue todas as partes da sintaxe em cada documento (o método ``DescendantNodes``) e use o Linq para selecionar nós que descrevem a declaração de variáveis:

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

Cada tipo de construção C# com um tipo correspondente existirá na árvore de sintaxe. Para encontrar rapidamente tipos específicos, use a janela ``Syntax Visualizer`` do Visual Studio. Isso interpretará o documento aberto atual como uma árvore de sintaxe Roslyn.

## Criar espaço de trabalho do projeto MSBuild
Primeiro obtenha o nuget ``Microsoft.CodeAnalysis.CSharp.Workspaces`` antes de continuar.

    var workspace = Microsoft.CodeAnalysis.MSBuild.MSBuildWorkspace.Create();
    var project = await workspace.OpenProjectAsync(projectFilePath);
    var compilation = await project.GetCompilationAsync();

    foreach (var diagnostic in compilation.GetDiagnostics()
        .Where(d => d.Severity == Microsoft.CodeAnalysis.DiagnosticSeverity.Error))
    {
        Console.WriteLine(diagnostic);
    }

Para carregar o código existente no espaço de trabalho, compile e relate erros. Em seguida, o código será localizado na memória. A partir daqui, tanto o lado sintático quanto o semântico estarão disponíveis para trabalhar.

