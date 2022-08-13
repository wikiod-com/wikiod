---
title: "Exemplos de AssemblyInfo.cs"
slug: "exemplos-de-assemblyinfocs"
draft: false
images: []
weight: 9764
type: docs
toc: true
---

O nome do arquivo `AssemblyInfo.cs` é usado por convenção como o arquivo de origem onde os desenvolvedores colocam atributos de metadados que descrevem todo o assembly que estão construindo.

## AssemblyInfo global e local


## [AssemblyVersion]
Este atributo aplica uma versão ao assembly.

    [assembly: AssemblyVersion("1.0.*")]

O caractere `*` é usado para incrementar automaticamente uma parte da versão automaticamente toda vez que você compila (geralmente usado para o número "build")

## [AssemblyTitle]
Este atributo é usado para dar um nome a este assembly específico.

    [assembly: AssemblyTitle("MyProduct")]



## [Produto de Montagem]
Este atributo é usado para descrever o produto para o qual este conjunto específico se destina. Vários assemblies podem ser componentes do mesmo produto e, nesse caso, todos podem compartilhar o mesmo valor para esse atributo.

    [assembly: AssemblyProduct("MyProduct")]


## Versionamento automatizado


## Campos comuns


## [InternalsVisibleTo]
Se você deseja tornar classes ou funções `internas` de um assembly acessíveis a partir de outro assembly, você declara isso por `InternalsVisibleTo` e o nome do assembly que tem permissão para acessar.


Neste código de exemplo no assembly `MyAssembly.UnitTests` é permitido chamar elementos `internal` de `MyAssembly`.

    [assembly: InternalsVisibleTo("MyAssembly.UnitTests")]

Isso é especialmente útil para testes de unidade para evitar declarações 'públicas' desnecessárias.

## Lendo Atributos do Assembly
Usando as ricas APIs de reflexão do .NET, você pode obter acesso aos metadados de um assembly. Por exemplo, você pode obter o atributo title do assembly `this` com o seguinte código

    using System.Linq;
    using System.Reflection;
    
    ...
    
    Assembly assembly = typeof(this).Assembly;
    var titleAttribute = assembly.GetCustomAttributes<AssemblyTitleAttribute>().FirstOrDefault();
    
    Console.WriteLine($"This assembly title is {titleAttribute?.Title}");


## [Configuração de montagem]
AssemblyConfiguration: O atributo AssemblyConfiguration deve ter a configuração que foi usada para construir o assembly.
Use a compilação condicional para incluir corretamente diferentes configurações de assembly.
Use o bloco semelhante ao exemplo abaixo. Adicione quantas configurações diferentes você costuma usar.


    #if (DEBUG)
    
    [assembly: AssemblyConfiguration("Debug")]

    #else

    [assembly: AssemblyConfiguration("Release")]
    
    #endif


## [AssemblyKeyFile]
Sempre que queremos que nosso assembly seja instalado no GAC, é necessário ter um nome forte. Para um assembly de nomenclatura forte, temos que criar uma chave pública.
Para gerar o arquivo `.snk`.

Para criar um arquivo de chave de nome forte

> 1. Prompt de comando de desenvolvedores para VS2015 (com acesso de administrador)
> 2. No prompt de comando, digite cd C:\Directory_Name e pressione ENTER.
> 3. No prompt de comando, digite sn -k KeyFileName.snk e pressione ENTER.

uma vez que o keyFileName.snk é criado no diretório especificado, forneça a referência em seu projeto. dê ao atributo `AssemblyKeyFileAttribute` o caminho para o arquivo `snk` para gerar a chave quando construirmos nossa biblioteca de classes.
    
> Propriedades -> AssemblyInfo.cs
    
    [assembly: AssemblyKeyFile(@"c:\Directory_Name\KeyFileName.snk")]

Isso criará um assembly de nome forte após a compilação. Depois de criar seu assembly de nome forte, você pode instalá-lo no GAC

Boa codificação :)

