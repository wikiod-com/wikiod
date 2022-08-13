---
title: "Concaténer des chaînes"
slug: "concatener-des-chaines"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Si vous créez une chaîne dynamique, il est recommandé d'opter pour la classe `StringBuilder` plutôt que de joindre des chaînes à l'aide de la méthode + ou `Concat` car chaque +/`Concat` crée un nouvel objet chaîne à chaque fois qu'il est exécuté.

## + Opérateur
    string s1 = "string1";
    string s2 = "string2";
    
    string s3 = s1 + s2; // "string1string2"

## Concaténer des chaînes à l'aide de System.Text.StringBuilder
La concaténation de chaînes à l'aide d'un [StringBuilder][1] peut offrir des avantages en termes de performances par rapport à une simple concaténation de chaînes à l'aide de `+`. Cela est dû à la façon dont la mémoire est allouée. Les chaînes sont réallouées à chaque concaténation, les StringBuilders allouent de la mémoire dans les blocs uniquement réalloués lorsque le bloc actuel est épuisé. Cela peut faire une énorme différence lorsque vous faites beaucoup de petites concaténations.

    StringBuilder sb = new StringBuilder();
    for (int i = 1; i <= 5; i++)
    {
        sb.Append(i);
        sb.Append(" ");
    }
    Console.WriteLine(sb.ToString()); // "1 2 3 4 5 "

Les appels à `Append()` peuvent être chaînés, car ils renvoient une référence au `StringBuilder` :

    StringBuilder sb = new StringBuilder();
    sb.Append("some string ")
      .Append("another string");


[1] : https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

## Concatonner des éléments de tableau de chaînes à l'aide de String.Join
La méthode `String.Join` peut être utilisée pour concaténer plusieurs éléments à partir d'un tableau de chaînes.

    string[] value = {"apple", "orange", "grape", "pear"};
    string separator = ", ";

    string result = String.Join(separator, value, 1, 2);
    Console.WriteLine(result);

> Produit la sortie suivante : "orange, raisin"

Cet exemple utilise la surcharge `String.Join(String, String[], Int32, Int32)`, qui spécifie l'index de début et le nombre au-dessus du séparateur et de la valeur.

Si vous ne souhaitez pas utiliser les surcharges startIndex et count, vous pouvez joindre toutes les chaînes données. Comme ça:
    
    string[] value = {"apple", "orange", "grape", "pear"};
    string separator = ", ";
    string result = String.Join(separator, value);
    Console.WriteLine(result);

qui produira;

> pomme, orange, raisin, poire

## Concaténation de deux chaînes avec $
$ fournit une méthode simple et concise pour concaténer plusieurs chaînes.

    var str1 = "text1";
    var str2 = " ";
    var str3 = "text3";
    string result2 = $"{str1}{str2}{str3}"; //"text1 text3"

