---
title: "Analyse des expressions régulières"
slug: "analyse-des-expressions-regulieres"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Syntaxe
- `new Regex(pattern);` //* Crée une nouvelle instance avec un pattern défini.*
- `Regex.Match(input);` //*Démarre la recherche et renvoie la correspondance.*
- `Regex.Matches(input);` //*Démarre la recherche et renvoie une MatchCollection*


## Paramètres
| Nom | Détails|
| ------ | ------ |
| Motif | Le modèle `string` qui doit être utilisé pour la recherche. Pour plus d'informations : [msdn][1]|
| Options Regex *[Facultatif]* | Les options courantes ici sont `Singleline` et `Multiline`. Ils changent le comportement des éléments de motif comme le point (.) qui ne couvrira pas un `NewLine` (\n) en `Multiline-Mode` mais en `SingleLine-Mode`. Comportement par défaut : [msdn][2] |
| Délai d'expiration *[Facultatif]* | Lorsque les modèles deviennent plus complexes, la recherche peut prendre plus de temps. Il s'agit du délai d'expiration passé pour la recherche, tel qu'il est connu de la programmation réseau.|


[1] : https://msdn.microsoft.com/en-us/library/ae5bf541(v=vs.90).aspx
[2] : https://msdn.microsoft.com/en-US/library/yd1hzczs(v=vs.110).aspx#Default

** Nécessaire avec **

    using System.Text.RegularExpressions;

**Bon d'avoir**

- Vous pouvez tester vos modèles en ligne sans avoir besoin de compiler votre solution pour obtenir des résultats ici : [Cliquez-moi][1]
- Exemple Regex101 : [Cliquez-moi][2]

_________

* En particulier, les débutants ont tendance à exagérer leurs tâches avec regex car il se sent puissant et au bon endroit pour les recherches textuelles plus complexes. C'est le point où les gens essaient d'analyser des documents xml avec regex sans même se demander s'il pourrait y avoir une classe déjà terminée pour cette tâche comme `XmlDocument`.*

*Regex devrait être la dernière arme à choisir contre la complexité. Au moins, n'oubliez pas de faire un effort pour rechercher la "bonne voie" avant d'écrire 20 lignes de motifs.*


[1] : https://regex101.com/
[2] : https://regex101.com/r/cG9lP5/1


## Match unique
*`en utilisant System.Text.RegularExpressions;`*

    string pattern = ":(.*?):";
    string lookup = "--:text in here:--";
    
    // Instanciate your regex object and pass a pattern to it
    Regex rgxLookup = new Regex(pattern, RegexOptions.Singleline, TimeSpan.FromSeconds(1));
    // Get the match from your regex-object
    Match mLookup = rgxLookup.Match(lookup);
    
    // The group-index 0 always covers the full pattern.
    // Matches inside parentheses will be accessed through the index 1 and above.
    string found = mLookup.Groups[1].Value;

**Résultat:**

    found = "text in here"

## Plusieurs correspondances
*`en utilisant System.Text.RegularExpressions;`*

    List<string> found = new List<string>();
    string pattern = ":(.*?):";
    string lookup = "--:text in here:--:another one:-:third one:---!123:fourth:";
    
    // Instanciate your regex object and pass a pattern to it
    Regex rgxLookup = new Regex(pattern, RegexOptions.Singleline, TimeSpan.FromSeconds(1));
    MatchCollection mLookup = rgxLookup.Matches(lookup);
    
    foreach(Match match in mLookup)
    {
        found.Add(match.Groups[1].Value);
    }

**Résultat:**

    found = new List<string>() { "text in here", "another one", "third one", "fourth" }

