---
title: ".NET Derleyici Platformu (Roslyn)"
slug: "net-derleyici-platformu-roslyn"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

## Semantik model
Bir **Semantik Model**, bir sözdizimi ağacına kıyasla daha derin bir yorumlama düzeyi ve kod anlayışı sunar. Sözdizim ağaçlarının değişkenlerin isimlerini söyleyebildiği yerde, anlamsal modeller de türü ve tüm referansları verir. Sözdizimi ağaçları, yöntem çağrılarını fark eder, ancak anlamsal modeller, yöntemin bildirildiği kesin konuma referanslar verir (aşırı yük çözümü uygulandıktan sonra).

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

Bu, bir sözdizimi ağacı kullanarak yerel değişkenlerin bir listesini verir. Daha sonra tam tip adını almak ve her değişkenin tüm referanslarını bulmak için anlamsal modele başvurur.

## Sözdizimi ağacı
Bir **Sözdizimi Ağacı**, programı bir adlar, komutlar ve işaretler ağacı olarak temsil eden değişmez bir veri yapısıdır (düzenleyicide daha önce yapılandırıldığı gibi).

Örneğin, "derleme" adlı bir "Microsoft.CodeAnalysis.Compilation" örneğinin yapılandırıldığını varsayalım. Yüklenen kodda bildirilen her değişkenin adını listelemenin birden çok yolu vardır. Bunu safça yapmak için, her belgedeki tüm sözdizim parçalarını ("DescendantNodes" yöntemi) alın ve değişken bildirimini tanımlayan düğümleri seçmek için Linq'i kullanın:

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

Karşılık gelen bir türe sahip her tür C# yapısı sözdizimi ağacında bulunacaktır. Belirli türleri hızlı bir şekilde bulmak için Visual Studio'dan ``Sözdizimi Görselleştirici`` penceresini kullanın. Bu, mevcut açık belgeyi bir Roslyn sözdizimi ağacı olarak yorumlayacaktır.

## MSBuild projesinden çalışma alanı oluşturun
Devam etmeden önce ilk olarak ``Microsoft.CodeAnalysis.CSharp.Workspaces`` nugetini edinin.

    var workspace = Microsoft.CodeAnalysis.MSBuild.MSBuildWorkspace.Create();
    var project = await workspace.OpenProjectAsync(projectFilePath);
    var compilation = await project.GetCompilationAsync();

    foreach (var diagnostic in compilation.GetDiagnostics()
        .Where(d => d.Severity == Microsoft.CodeAnalysis.DiagnosticSeverity.Error))
    {
        Console.WriteLine(diagnostic);
    }

Mevcut kodu çalışma alanına yüklemek için derleyin ve hataları bildirin. Daha sonra kod hafızada yer alacaktır. Buradan hem sözdizimsel hem de anlamsal tarafla çalışmak mümkün olacaktır.

