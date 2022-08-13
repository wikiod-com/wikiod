---
title: "Commentaires sur la documentation XML"
slug: "commentaires-sur-la-documentation-xml"
draft: false
images: []
weight: 9846
type: docs
toc: true
---

Parfois, vous devez **créer une documentation textuelle étendue** à partir de vos commentaires XML. Malheureusement *** il n'y a pas de moyen standard pour cela ***.

Mais il existe des projets distincts que vous pouvez utiliser dans ce cas :

- [Château de sable][1]
- [Document][2]
- [NDoc][1]
- [DocFX][4]


[1] : http://sandcastle.codeplex.com/
[2] : http://docu.jagregory.com/
[3] : http://ndoc.sourceforge.net/
[4] : https://dotnet.github.io/docfx/

## Annotation de méthode simple
Les commentaires de documentation sont placés directement au-dessus de la méthode ou de la classe qu'ils décrivent. Ils commencent par trois barres obliques `///` et permettent de stocker les méta-informations via XML.

    /// <summary>
    /// Bar method description
    /// </summary>
    public void Bar()
    { 
            
    }

Les informations contenues dans les balises peuvent être utilisées par Visual Studio et d'autres outils pour fournir des services tels qu'IntelliSense :

[![Exemple d'annotation xml de méthode][1]][1]


[1] : https://i.stack.imgur.com/NDAnP.png


Voir aussi [liste des balises de documentation courantes de Microsoft](https://msdn.microsoft.com/en-us/library/5ast78ax.aspx).

## Génération de XML à partir de commentaires de documentation
Pour générer un fichier de documentation XML à partir des commentaires de documentation dans le code, utilisez l'option `/doc` avec le compilateur C# `csc.exe`.

Dans Visual Studio 2013/2015, dans **Project** -> **Properties** -> **Build** -> **Output**, cochez la case `XML documentation file` :

[![Fichier de documentation XML][1]][1]

Lorsque vous construisez le projet, un fichier XML sera produit par le compilateur avec un nom correspondant au nom du projet (par exemple `XMLDocumentation.dll` -> `XMLDocumentation.xml`).

Lorsque vous utilisez l'assembly dans un autre projet, assurez-vous que le fichier XML se trouve dans le même répertoire que la DLL référencée.

Cet exemple :

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


Produit ce xml lors de la construction :

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

[1] : https://i.stack.imgur.com/tXXQy.png

## Commentaire de la documentation de la méthode avec param et éléments de retour
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

**IntelliSense** vous montre la description de chaque paramètre :

[![commentaire de paramètre][1]][1]

Astuce : Si Intellisense ne s'affiche pas dans Visual Studio, supprimez le premier crochet ou virgule, puis retapez-le.

[1] : https://i.stack.imgur.com/cH3OQ.png

## Commentaires sur l'interface et la documentation des classes
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

**Résultat**

Résumé de l'interface

[![résumé de l'interface][1]][1]

Résumé de la classe

[![résumé de la classe][2]][2]

[1] : https://i.stack.imgur.com/ExpwI.png
[2] : https://i.stack.imgur.com/730eY.png

## Référencer une autre classe dans la documentation
La balise `<see>` peut être utilisée pour créer un lien vers une autre classe. Il contient le membre `cref` qui doit contenir le nom de la classe à référencer. Visual Studio fournira Intelsense lors de l'écriture de cette balise et ces références seront également traitées lors du changement de nom de la classe référencée.

    /// <summary>
    /// You might also want to check out <see cref="SomeOtherClass"/>.
    /// </summary>
    public class SomeClass
    {
    }
Dans les fenêtres contextuelles de Visual Studio Intellisense, ces références seront également affichées en couleur dans le texte.

Pour référencer une classe générique, utilisez quelque chose de similaire à ce qui suit :

    /// <summary>
    /// An enhanced version of <see cref="List{T}"/>.
    /// </summary>
    public class SomeGenericClass<T>
    {
    }

