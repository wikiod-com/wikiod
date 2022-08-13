---
title: "Chaînes textuelles"
slug: "chaines-textuelles"
draft: false
images: []
weight: 9434
type: docs
toc: true
---

## Syntaxe
- @"les chaînes textuelles sont des chaînes dont le contenu n'est pas échappé, donc dans ce cas \n ne représente pas le caractère de saut de ligne mais deux caractères individuels : \ et n. Les chaînes textuelles sont créées en préfixant le contenu de la chaîne avec le caractère @"

- @"Pour échapper aux guillemets, ""guillemets doubles"" sont utilisés."

Pour concaténer des littéraux de chaîne, utilisez le symbole @ au début de chaque chaîne.

    var combinedString = @"\t means a tab" + @" and \n means a newline";

## Chaînes textuelles interpolées
Les chaînes textuelles peuvent être combinées avec les nouvelles fonctionnalités https://www.wikiod.com/fr/docs/c%23/24/c-sharp-6-0-features/49/string-interpolation trouvées dans C#6.

    Console.WriteLine($@"Testing \n 1 2 {5 - 2}
    New line");

**Production:**

> Test \n 1 2 3
> Nouvelle ligne

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/cWyQE2)

Comme attendu d'une chaîne verbatim, les barres obliques inverses sont ignorées en tant que caractères d'échappement. Et comme attendu d'une chaîne interpolée, toute expression à l'intérieur des accolades est évaluée avant d'être insérée dans la chaîne à cette position.


## Échapper aux guillemets doubles
Les guillemets doubles à l'intérieur des chaînes textuelles peuvent être échappés en utilisant 2 guillemets doubles séquentiels `""` pour représenter un guillemet double `"` dans la chaîne résultante.

    var str = @"""I don't think so,"" he said.";
    Console.WriteLine(str);

**Production:**
> "Je ne pense pas", a-t-il dit.

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/c4OJoq)

## Les chaînes textuelles indiquent au compilateur de ne pas utiliser les caractères d'échappement
Dans une chaîne normale, le caractère barre oblique inverse est le caractère d'échappement, qui demande au compilateur de regarder le(s) caractère(s) suivant(s) pour déterminer le caractère réel dans la chaîne. ([Liste complète des échappements de caractères][1])

Dans les chaînes textuelles, il n'y a pas d'échappement de caractères (sauf pour `""` qui est transformé en `"`).
Pour utiliser une chaîne verbatim, ajoutez simplement un `@` avant les guillemets de début.

Cette chaîne verbatim

    var filename = @"c:\temp\newfile.txt"

**Production:**

>c:\temp\nouveaufichier.txt

Au lieu d'utiliser une chaîne ordinaire (non verbatim) :

    var filename = "c:\temp\newfile.txt"

qui affichera :

    c:    emp
    ewfile.txt

en utilisant l'échappement de caractères. (Le `\t` est remplacé par un caractère de tabulation et le `\n` est remplacé par une nouvelle ligne.)

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/7kslXQ)






[1] : https://www.wikiod.com/fr/docs/c%23/39/string-escape-sequences#t=201607172257361795538&a=syntax

## Chaînes multilignes
    var multiLine = @"This is a 

    multiline paragraph";

**Production:**
> C'est un
>
> paragraphe multiligne

[Démo en direct sur .NET Fiddle](https://dotnetfiddle.net/kfOUcH)

Les chaînes multilignes qui contiennent des guillemets doubles peuvent également être échappées comme elles l'étaient sur une seule ligne, car ce sont des chaînes textuelles.
 
    var multilineWithDoubleQuotes = @"I went to a city named

                            ""San Diego""

                          during summer vacation.";

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/0hwJpf)

*Il est à noter que les espaces/tabulations au début des lignes 2 et 3 ici sont bien présents dans la valeur de la variable ; consultez [cette question](http://stackoverflow.com/questions/7178136/multiline-formatting-for-verbatim-strings-in-c-sharp-prefix-with) pour les solutions possibles.*


