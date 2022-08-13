---
title: "Comentários da Documentação XML"
slug: "comentarios-da-documentacao-xml"
draft: false
images: []
weight: 9846
type: docs
toc: true
---

Algumas vezes você precisa **criar documentação de texto estendida** a partir de seus comentários xml. Infelizmente ***não existe uma forma padrão para isso***.

Mas existem alguns projetos separados que você pode usar para este caso:

- [Castelo de areia][1]
- [Documento][2]
- [NDoc][1]
- [DocFX][4]


[1]: http://sandcastle.codeplex.com/
[2]: http://docu.jagregory.com/
[3]: http://ndoc.sourceforge.net/
[4]: https://dotnet.github.io/docfx/

## Anotação de método simples
Os comentários da documentação são colocados diretamente acima do método ou classe que descrevem. Eles começam com três barras `///` e permitem que as informações meta sejam armazenadas via XML.

    /// <summary>
    /// Bar method description
    /// </summary>
    public void Bar()
    { 
            
    }

As informações dentro das tags podem ser usadas pelo Visual Studio e outras ferramentas para fornecer serviços como o IntelliSense:

[![Exemplo de anotação xml de método][1]][1]


[1]: https://i.stack.imgur.com/NDAnP.png


Consulte também [lista de tags de documentação comuns da Microsoft](https://msdn.microsoft.com/en-us/library/5ast78ax.aspx).

## Gerando XML a partir de comentários de documentação
Para gerar um arquivo de documentação XML a partir de comentários de documentação no código, use a opção `/doc` com o compilador C# `csc.exe`.

No Visual Studio 2013/2015, em **Projeto** -> **Propriedades** -> **Build** -> **Saída**, marque a caixa de seleção `arquivo de documentação XML`:

[![arquivo de documentação XML][1]][1]

Ao construir o projeto, um arquivo XML será produzido pelo compilador com um nome correspondente ao nome do projeto (por exemplo, `XMLDocumentation.dll` -> `XMLDocumentation.xml`).

Ao usar o assembly em outro projeto, verifique se o arquivo XML está no mesmo diretório que a DLL que está sendo referenciada.

Este exemplo:

    /// <summary>
    /// Data class description
    /// </summary>
    public class DataClass
    {
        /// <summary>
        /// Name property description
        /// </summary>
        public string Name { get; set; }
    }


    /// <summary>
    /// Foo function
    /// </summary>
    public class Foo
    {
        /// <summary>
        /// This method returning some data
        /// </summary>
        /// <param name="id">Id parameter</param>
        /// <param name="time">Time parameter</param>
        /// <returns>Data will be returned</returns>
        public DataClass GetData(int id, DateTime time)
        {
            return new DataClass();
        }
    }


Produz este xml na compilação:

    <?xml version="1.0"?>
    <doc>
        <assembly>
            <name>XMLDocumentation</name>
        </assembly>
        <members>
            <member name="T:XMLDocumentation.DataClass">
                <summary>
                Data class description
                </summary>
            </member>
            <member name="P:XMLDocumentation.DataClass.Name">
                <summary>
                Name property description
                </summary>
            </member>
            <member name="T:XMLDocumentation.Foo">
                <summary>
                Foo function
                </summary>
            </member>
            <member name="M:XMLDocumentation.Foo.GetData(System.Int32,System.DateTime)">
                <summary>
                This method returning some data
                </summary>
                <param name="id">Id parameter</param>
                <param name="time">Time parameter</param>
                <returns>Data will be returned</returns>
            </member>
        </members>
    </doc>

[1]: https://i.stack.imgur.com/tXXQy.png

## Comentário da documentação do método com param e retorna elementos
    /// <summary>
    /// Returns the data for the specified ID and timestamp.
    /// </summary>
    /// <param name="id">The ID for which to get data. </param>
    /// <param name="time">The DateTime for which to get data. </param>
    /// <returns>A DataClass instance with the result. </returns>
    public DataClass GetData(int id, DateTime time)
    {
       // ...
    }

**IntelliSense** mostra a descrição de cada parâmetro:

[![comentário de parâmetro][1]][1]

Dica: Se o Intellisense não for exibido no Visual Studio, exclua o primeiro colchete ou vírgula e digite-o novamente.

[1]: https://i.stack.imgur.com/cH3OQ.png

## Comentários da documentação da interface e da classe
    /// <summary>
    /// This interface can do Foo
    /// </summary>
    public interface ICanDoFoo
    {
        // ... 
    }

    /// <summary>
    /// This Bar class implements ICanDoFoo interface
    /// </summary>
    public class Bar : ICanDoFoo
    {
        // ...
    }

**Resultado**

Resumo da interface

[![resumo da interface][1]][1]

Resumo da aula

[![resumo da aula][2]][2]

[1]: https://i.stack.imgur.com/ExpwI.png
[2]: https://i.stack.imgur.com/730eY.png

## Referenciando outra classe na documentação
A tag `<see>` pode ser usada para vincular a outra classe. Ele contém o membro `cref` que deve conter o nome da classe que deve ser referenciada. O Visual Studio fornecerá o Intellsense ao escrever essa marca e essas referências também serão processadas ao renomear a classe referenciada.

    /// <summary>
    /// You might also want to check out <see cref="SomeOtherClass"/>.
    /// </summary>
    public class SomeClass
    {
    }
Nos pop-ups do Visual Studio Intellisense, essas referências também serão exibidas coloridas no texto.

Para fazer referência a uma classe genérica, use algo semelhante ao seguinte:

    /// <summary>
    /// An enhanced version of <see cref="List{T}"/>.
    /// </summary>
    public class SomeGenericClass<T>
    {
    }

